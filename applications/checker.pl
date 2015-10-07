:- module(checker,[testSchema/1,
		   runSchemaUpdate/3,
		   runInstanceUpdate/3,
		   runFullValidation/2,
		   runSchemaValidation/2]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils).
:- use_module(tbox).
:- use_module(abox). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Tests

%%%% Schema tests
%%%% must be pred/2 and have argument list (Schema,Reason)
%%%% Names should be ([A-Z]|[a-z])+SC (Schema Constraint)

% Required best practice
preTestSchema(classCycleSC).
preTestSchema(propertyCycleSC).
% best practice
testSchema(noImmediateDomainSC).
testSchema(noImmediateRangeSC).
testSchema(notUniqueClassLabelSC).
testSchema(notUniqueClassSC).
testSchema(notUniquePropertySC).
testSchema(schemaBlankNodeSC).
% OWL DL
testSchema(orphanClassSC).
testSchema(orphanPropertySC). 
testSchema(invalidRangeSC). 
testSchema(invalidDomainSC).
testSchema(domainNotSubsumedSC).
testSchema(rangeNotSubsumedSC).

%%%% Instance Tests
%%%% Local testing for violation of specific known elements in update.
%%%% must be pred/6 and have argument list (X,P,Y,Instance,Schema,Reason)
%% best Practice
edgeConstraints(noPropertyDomainIC).
edgeConstraints(noPropertyRangeIC).
edgeConstraints(instanceBlankNodeIC).
%% OWL DL (Constraint)
edgeConstraints(invalidEdgeIC).
edgeConstraints(edgeOrphanInstanceIC).
edgeConstraints(notFunctionalPropertyIC).
edgeConstraints(notInverseFunctionalPropertyIC).
edgeConstraints(localOrphanPropertyIC).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Deltas 

:- rdf_meta runInsert(r,r,o,?).
runInsert([XI,YI,ZI,G]) :-
    transactionGraph:insert(XI,YI,ZI,G).

:- rdf_meta runDelete(r,r,o,?).
runDelete([XI,YI,ZD,G]) :-
    transactionGraph:delete(XI,YI,ZD,G).

% obsolete
runUpdate([XU,YU,ZU,Action,G]) :-
    transactionGraph:update(XU,YU,ZU,Action,G). 

runDelta(Delta,Witnesses) :-
    getKey(deletes, Delta, Deletes, []),
    exclude(checker:runDelete,Deletes,Witnesses), !, % do not backtrack!
    getKey(inserts, Delta, Inserts, []),
    maplist(checker:runInsert,Inserts), !. % do not backtrack!

failure_witness([],[]) :- !.
failure_witness(DeleteFailures,DeleteWitness) :-
    convert_quads(JSONDeleteFailures,DeleteFailures),
    DeleteWitness = [json([error=deleteFailures,
			   deletes=JSONDeleteFailures,
			   message="Failed to delete all triples"])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Test running predicates

preSchemaTest(Pragma,Schema,Reason) :-
    preTestSchema(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)),
    call(Test, Schema, Reason).

schemaTest(Pragma,Schema,Reason) :-
    testSchema(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)),
    %nl,write(Test),nl,	
    call(Test, Schema, Reason).

runSchemaValidation(Pragma,Witnesses) :-
    getKey(schema, Pragma, Schema, 'schema'),
    % Only attempted schema test if the pre-test fails (Cycles!)
    (setof(json(Reason), checker:preSchemaTest(Pragma, Schema, Reason), Witnesses)
     ; uniqueSolns(json(Reason), checker:schemaTest(Pragma, Schema, Reason), Witnesses)).


instanceValidator(Delta,Pragma,Reason) :-
    % obtain change information
    getKey(schema, Pragma, Schema, 'schema'),
    getKey(instance, Pragma, Instance, 'instance'), 
    getKey(inserts, Delta, Inserts, []),
    getKey(deletes, Delta, Deletes, []),
    % write('Validating with instance: '), write(Instance), nl,
    (member([X,P,Y,Instance], Inserts)
     ; member([X,P,Y,Instance], Deletes)), 
    
    edgeConstraints(Test),

    member(tests=TList,Pragma), 

    (all=TList 
     *-> true
     ;  member(Test, TList)), 

    call(Test, X, P, Y, Instance, Schema, Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%
% Run instance only updates
runInstanceUpdate(Delta, Pragma, Witnesses) :-
    % first perform update.
    runDelta(Delta,DeleteFailures),
    failure_witness(DeleteFailures,DeleteWitness),
    uniqueSolns(json(W), checker:instanceValidator(Delta,Pragma,W), ValidationWitnesses),
    append(ValidationWitnesses, DeleteWitness, Witnesses),
    getKey(instance, Pragma, Instance, 'instance'), 
    (member(commit=true,Pragma),
     Witnesses = [],
     commit(Instance)	   
     ; rollback(Instance)
    ).

runInstanceValidation(Delta,Pragma,Witnesses) :-
    uniqueSolns(json(W), checker:instanceValidator(Delta,Pragma,W), Witnesses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Instance / Schema Updates
runSchemaUpdate(Delta, Pragma, Witnesses) :-
    runDelta(Delta,DeleteFailures), % first perform update
    failure_witness(DeleteFailures,DeleteWitness),
    % getKey(schema, Pragma, Schema, 'schema'),
    % getKey(instance, Pragma, Instance, 'instance'),
    runSchemaValidation(Pragma,Witnesses2),
    runInstanceValidation(Delta,Pragma,Witnesses1),
    append(Witnesses1,Witnesses2,Witnesses3),
    append(Witnesses3,DeleteWitness, Witnesses),

    getKey(schema, Pragma, Schema, 'schema'),
    getKey(schema, Pragma, Instance, 'instance'),
    (member(commit=true,Pragma),
     Witnesses = [], 
     commit(Instance), 
     commit(Schema) 
     ; rollback(Instance),
       rollback(Schema)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Full validation (Instance / Schema)
% TODO: Change this to local instance validation with free parameters!!!
% Maybe fixed?
fullInstanceValidator(Pragma,Reason) :- 
    edgeConstraints(Test),
    member(tests=TList,Pragma),
    (all=TList 
     *-> true
     ;  member(Test, TList)), 

    getKey(schema, Pragma, Schema, 'schema'),
    getKey(instance, Pragma, Instance, 'instance'), 

    call(Test, _, _, _, Instance, Schema,Reason).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schema validation 
runFullValidation(Pragma,Witnesses) :- 
    runSchemaValidation(Pragma,SchemaWitnesses), 
    
    % schema updates can be empty and will check all, but instances can not!
    uniqueSolns(json(W), checker:fullInstanceValidator(Pragma,W), InstanceWitnesses),
    
    append(SchemaWitnesses,InstanceWitnesses,Witnesses).
