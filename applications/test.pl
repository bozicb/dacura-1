:- module(test,[runTests/1]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils).
:- use_module(checker).
:- use_module(library(http/json)). 
:- use_module(library(http/json_convert)). 

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test harness.

%%% tests(Pragma,WitnessOfFailureExpected) 
tests([instance=instance, schema=blankNodes, tests=[schemaBlankNodeSC]],
      [json([error=instanceBlankNode, message='The object __blankNodes1 is a blank node', object='__blankNodes1'])]).
tests([instance=checkInstanceDomains, schema=validSchema, tests=[invalidEdgeIC]],
      [json([=(error,dataInvalidAtDatatype),=(message,'Literal can not be an object'),=(element,literal(type('http://www.w3.org/2001/XMLSchema#dateTime','2002-09-24+06:00'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#Territory')])]).
tests([instance=instance, schema=classCycles, tests=[classCycleSC]],
      [json([=(error,classCycle),=(message,'Class, http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation has a class cycle with path: [\'http://dacura.cs.tcd.ie/data/seshat#TemporalEntity\',\'http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation\']'),=(class,'http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation'),=(path,['http://dacura.cs.tcd.ie/data/seshat#TemporalEntity','http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation'])])]).
tests([instance=instance, schema=duplicateClasses, tests=[notUniqueClassSC]],
      []). % Don't see how to fix this at the moment due to enforced cardinality of 0/1
tests([instance=instance, schema=invalidDomain, tests=[invalidDomainSC]],
      [json([=(error,invalidDomain),=(message,'The property http://dacura.cs.tcd.ie/data/seshat#end has an undefined domain.'),=(property,'http://dacura.cs.tcd.ie/data/seshat#end'),=(domain,'http://dacura.cs.tcd.ie/data/seshat#bad')]),json([=(error,invalidDomain),=(message,'The property http://dacura.cs.tcd.ie/data/seshat#start has an undefined domain.'),=(property,'http://dacura.cs.tcd.ie/data/seshat#start'),=(domain,'http://www.w3.org/2001/XMLSchema#dateTime')])]).
tests([instance=instance, schema=invalidRange, tests=[invalidRangeSC]],
      [json([=(error,invalidRange),=(message,'ObjectProperty Range http://dacura.cs.tcd.ie/data/seshat#bad is not a valid range for property http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory.'),=(property,'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory'),=(range,'http://dacura.cs.tcd.ie/data/seshat#bad')])]).
% Probably need an addition here for cardinality too small.
tests([instance='oneof-instance', schema='oneof-schema', tests=[invalidEdgeIC]],
      [json([=(error,notRestrictionElement),=(message,'Cardinality too large on restriction class'),=(element,'http://example.org/time#me'),=(cardinality,'2'),=(class,'__oneof-schema6')]),json([=(error,notRestrictionElement),=(message,'Cardinality unequal on restriction class'),=(element,'http://example.org/time#me'),=(cardinality,'0'),=(class,'__oneof-schema1')])]).      
%tests([instance=instance, schema=orphanDomains, tests=all]).
tests([instance=instance, schema=orphanInstances, tests=[edgeOrphanInstanceIC]],
      [json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#someproperty'),=(object,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey'),=(class,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(predicate,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),=(object,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey'),=(class,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#floatTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#double','-12.5e10'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#floatTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#double','1.3e2'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#floatTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#double','10e0'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyepd'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','10'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','+1'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','-5'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','0'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','1'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','10'))),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#stringTyped'),=(object,literal('a string')),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),=(object,'http://dacura.cs.tcd.ie/data/seshat#typedThingy'),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')])]). tests([instance=instance, schema=orphanProperties, tests=[localOrphanPropertyIC]],
       [json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#someproperty'),=(object,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey'),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#someproperty.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#floatTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#double','-12.5e10'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#floatTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#floatTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#double','1.3e2'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#floatTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#floatTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#double','10e0'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#floatTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyepd'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','10'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#integerTyepd.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','+1'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#integerTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','-5'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#integerTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','0'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#integerTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','1'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#integerTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#integerTyped'),=(object,literal(type('http://www.w3.org/2001/XMLSchema#integer','10'))),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#integerTyped.')]),json([=(error,noInstancePropertyClass),=(subject,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#stringTyped'),=(object,literal('a string')),=(message,'No property class associated with property: http://dacura.cs.tcd.ie/data/seshat#stringTyped.')])]).
%tests([instance=instance, schema=orphanRanges, tests=all]).
%tests([instance=instance, schema=orphanSubProperties, tests=all]).
tests([instance=instance, schema=propertyCycles, tests=[propertyCycleSC]],
      [json([=(error,propertyClassCycle),=(property,'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory'),=(path,['http://dacura.cs.tcd.ie/data/seshat#controlsTerritory','http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish','http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory']),=(message,'Property class http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory has a cycle with path: [\'http://dacura.cs.tcd.ie/data/seshat#controlsTerritory\',\'http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish\',\'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory\']')]),json([=(error,propertyClassCycle),=(property,'http://dacura.cs.tcd.ie/data/seshat#controlsTerritory'),=(path,['http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish','http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory','http://dacura.cs.tcd.ie/data/seshat#controlsTerritory']),=(message,'Property class http://dacura.cs.tcd.ie/data/seshat#controlsTerritory has a cycle with path: [\'http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish\',\'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory\',\'http://dacura.cs.tcd.ie/data/seshat#controlsTerritory\']')]),json([=(error,propertyClassCycle),=(property,'http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish'),=(path,['http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory','http://dacura.cs.tcd.ie/data/seshat#controlsTerritory','http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish']),=(message,'Property class http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish has a cycle with path: [\'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory\',\'http://dacura.cs.tcd.ie/data/seshat#controlsTerritory\',\'http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish\']')])]).
tests([instance=instance, schema=typeCheck, tests=[edgeOrphanInstanceIC]],      
      [json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(predicate,'http://dacura.cs.tcd.ie/data/seshat#someproperty'),=(object,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey'),=(class,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey')]),json([=(error,edgeOrphanInstance),=(message,'Instance domain class is not valid'),=(subject,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(predicate,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),=(object,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey'),=(class,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey')])]).
tests([instance=rdfcioInstance, schema=rdfcio, tests=[invalidEdgeIC]],
      [json([=(error,objectInvalidAtClass),=(message,'Element is not valid at any class of union'),=(element,'http://www.w3.org/2015/rdfcio/data#notNil'),=(class,'http://www.w3.org/2015/rdfcio#List')])]).
tests([instance=disjointUnionInstance, schema=disjointUnion, tests=[invalidEdgeIC]],
      [json([=(error,objectInvalidAtClass),=(message,'More than one branch of disjointUnion is valid.'),=(element,'http://www.example.org/data#eltAB'),=(class,'http://www.example.org/schema#EitherAorB')])]).


fname(Name,FName) :-
    file_search_path(cliopatria,Path),
    atom_concat(Path, '/cpack/dacura/applications/testData/', FullPath),
    atom_concat(Name, '.ttl', File),
    atom_concat(FullPath, File, FName), !.

runTests(Witnesses) :-
    % fill with the witness of failure. 
    bagof(Witness, runTest(Witness), LL),
    flatten(LL,Witnesses).

runTest(Fail) :- 
    tests(Pragma, ExpectedWitnesses),
    nl,writeq(Pragma),nl,
    member(schema=Schema, Pragma),
    member(instance=Instance, Pragma),
    member(tests=Tests, Pragma),
    unloadDB(Instance,Schema), % make sure we don't exist first!
    loadDB(Instance,Schema),
    runFullValidation(Pragma,Witnesses),
    (ExpectedWitnesses = Witnesses
     *-> Fail = []
     ; Fail = [witnesses=Witnesses, expected=ExpectedWitnesses,test=Tests],
       %nl,current_output(Out), jsonify(Witnesses,JSON), json_write(Out, JSON),nl,
       nl,writeq(Witnesses),nl,
       nl,writeq(Fail),nl,
       nl,writeq(Tests),nl,
       nl,writeq(Witnesses),nl),
    unloadDB(Instance,Schema). 
    
loadDB(Instance,Schema) :- 
    rdf_retractall(_, _, _, Instance), 
    rdf_retractall(_, _, _, Schema),
    fname(Schema, FSchema),
    fname(Instance, FInstance), 
    rdf_load(FSchema, [graph(Schema)]), 
    rdf_load(FInstance, [graph(Instance)]).

unloadDB(Instance,Schema) :-
    rollback(Instance),
    rollback(Schema),
    rdf_unload_graph(Schema),
    rdf_unload_graph(Instance).
		       
		       
