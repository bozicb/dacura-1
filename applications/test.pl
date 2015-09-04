:- module(test,[runTests/1]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils).
:- use_module(schemaRules).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test harness.

%%% tests(Pragma,WitnessOfFailureExpected) 
tests([instance=instance, schema=blankNodes, tests=[schemaBlankNodes]],
      [[json([=(error,schemaBlankNode),=(blank,'__blankNodes1')])]]).
tests([instance=checkInstanceDomains, schema=validSchema, tests=[localInvalidInstanceDomains]],
      [[json([error=invalidInstanceDomain, instance='http://dacura.cs.tcd.ie/data/seshat#thing', property='http://dacura.cs.tcd.ie/data/seshat#includesTerritory', domain='http://dacura.cs.tcd.ie/data/seshat#CollectionOfTerritories'])]]).
tests([instance=checkInstanceRanges, schema=validSchema, tests=[localInvalidInstanceRanges]],
      [[json([error=invalidInstanceRange, instance='http://dacura.cs.tcd.ie/data/seshat#thingy', property='http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory', range='http://dacura.cs.tcd.ie/data/seshat#QualifiedTerritory', value='literal(type(http://www.w3.org/2001/XMLSchema#integer,10))'])]]).
tests([instance=instance, schema=classCycles, tests=[classCycles]],
      [[json([=(error,classCycle),=(class,'http://dacura.cs.tcd.ie/data/seshat#TemporalEntity'),=(path,['http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation','http://dacura.cs.tcd.ie/data/seshat#TemporalEntity']),=(message,"Class http://dacura.cs.tcd.ie/data/seshat#TemporalEntity has a class cycle with path: ['http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation','http://dacura.cs.tcd.ie/data/seshat#TemporalEntity']")])]]).
tests([instance=instance, schema=duplicateClasses, tests=[duplicateClasses]],
      []). % Don't see how to fix this at the moment due to enforced cardinality of 0/1
tests([instance=instance, schema=invalidDomain, tests=[invalidDomain]],
      [[json([=(error,notUniqueValidDomain),=(property,'http://dacura.cs.tcd.ie/data/seshat#end'),=(domain,'http://dacura.cs.tcd.ie/data/seshat#bad')]),json([=(error,notUniqueValidDomain),=(property,'http://dacura.cs.tcd.ie/data/seshat#start'),=(domain,'http://dacura.cs.tcd.ie/data/seshat#ValueWithDuration')]),json([=(error,notUniqueValidDomain),=(property,'http://dacura.cs.tcd.ie/data/seshat#start'),=(domain,'http://www.w3.org/2001/XMLSchema#dateTime')])]]).
tests([instance=instance, schema=invalidRange, tests=[invalidRange]],
      [[json([=(error,notUniqueValidRange),=(property,'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory'),=(range,'http://dacura.cs.tcd.ie/data/seshat#bad')]),json([=(error,notUniqueValidRange),=(property,'http://dacura.cs.tcd.ie/data/seshat#controlsTerritory'),=(range,'http://dacura.cs.tcd.ie/data/seshat#bad')]),json([=(error,notUniqueValidRange),=(property,'http://dacura.cs.tcd.ie/data/seshat#start'),=(range,'http://www.w3.org/2001/XMLSchema#dateTime')])]]).
% Probably need an addition here for cardinality too small.
tests([instance='oneof-instance', schema='oneof-schema', tests=[localCardinalityTooLarge]],
      [[json([=(error,cardinalityTooLarge),=(instance,'http://example.org/time#me'),=(range,'http://www.w3.org/2006/time#DateTimeDescription'),=(size,2),=(property,'http://www.w3.org/2006/time#dayOfWeek')])]]).
%tests([instance=instance, schema=orphanDomains, tests=all]).
tests([instance=instance, schema=orphanInstances, tests=[localOrphanInstances]],
      [[json([=(error,orphanInstance),=(instance,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(class,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey')]),json([=(error,orphanInstance),=(instance,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(class,'http://dacura.cs.tcd.ie/data/seshat#typedThingy')])]]).
tests([instance=instance, schema=orphanProperties, tests=[localOrphanProperties]],
      [[json([=(error,noInstancePropertyClass),=(instance,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(property,'http://dacura.cs.tcd.ie/data/seshat#someproperty')]),json([=(error,noInstancePropertyClass),=(instance,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(property,'http://dacura.cs.tcd.ie/data/seshat#floatTyped')]),json([=(error,noInstancePropertyClass),=(instance,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(property,'http://dacura.cs.tcd.ie/data/seshat#integerTyepd')]),json([=(error,noInstancePropertyClass),=(instance,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(property,'http://dacura.cs.tcd.ie/data/seshat#integerTyped')]),json([=(error,noInstancePropertyClass),=(instance,'http://dacura.cs.tcd.ie/data/seshat#typedThingy1'),=(property,'http://dacura.cs.tcd.ie/data/seshat#stringTyped')])]]).
%tests([instance=instance, schema=orphanRanges, tests=all]).
%tests([instance=instance, schema=orphanSubProperties, tests=all]).
tests([instance=instance, schema=propertyCycles, tests=[propertyCycles]],
      [[json([=(error,propertyClassCycle),=(property,'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory'),=(path,['http://dacura.cs.tcd.ie/data/seshat#controlsTerritory','http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish','http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory']),=(message,"Property Class http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory has a property class cycle with path: ['http://dacura.cs.tcd.ie/data/seshat#controlsTerritory','http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish','http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory']")])]]).
tests([instance=instance, schema=typeCheck, tests=[localCheckInstanceClass]],
      [[json([=(error,orphanInstance),=(instance,'http://dacura.cs.tcd.ie/data/seshat#thing'),=(class,'http://dacura.cs.tcd.ie/data/seshat#funkymonkey')])]]).

fname(Name,FName) :-
    file_search_path(cliopatria,Path),
    atom_concat(Path, '/cpack/dacura/applications/testData/', FullPath),
    atom_concat(Name, '.ttl', File),
    atom_concat(FullPath, File, FName), !.

runTests(Witnesses) :-
    % fill with the witness of failure. 
    bagof(Witness, runTest(Witness), Witnesses).

runTest(Fail) :- 
    tests(Pragma, ExpectedWitnesses),
    nl,write(Pragma),nl,
    member(schema=Schema, Pragma),
    member(instance=Instance, Pragma),
    member(tests=Tests, Pragma),
    loadDB(Schema,Instance),
    runFullValidation(Pragma,Witnesses),
    (ExpectedWitnesses = Witnesses
     *-> Fail = []
     ; Fail = [witnesses=Witnesses, expected=ExpectedWitnesses,test=Tests],
       nl,write(Fail),nl,
       nl,write(Tests),nl,
       nl,write_canonical(Witnesses),nl),
    unloadDB(Schema,Instance). 
    
loadDB(Schema,Instance) :-
    rdf_retractall(_, _, _, Instance), 
    rdf_retractall(_, _, _, Schema),
    fname(Schema, FSchema),
    fname(Instance, FInstance), 
    rdf_load(FSchema, [graph(Schema)]), 
    rdf_load(FInstance, [graph(Instance)]).

unloadDB(Schema,Instance) :-
    rollback(Instance),
    rollback(Schema),
    rdf_unload_graph(Schema),
    rdf_unload_graph(Instance).
		       
		       
