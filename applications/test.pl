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
tests([instance=instance, schema=checkInstanceDomains, tests=[localInvalidInstanceDomains]],
     false).
tests([instance=instance, schema=checkInstanceRanges, tests=[localInvalidInstanceRanges]],
     false).
tests([instance=instance, schema=classCycles, tests=[classCycles]],
      [[json([=(error,classCycle),=(class,'http://dacura.cs.tcd.ie/data/seshat#TemporalEntity'),=(path,['http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation','http://dacura.cs.tcd.ie/data/seshat#TemporalEntity']),=(message,"Class http://dacura.cs.tcd.ie/data/seshat#TemporalEntity has a class cycle with path: ['http://dacura.cs.tcd.ie/data/seshat#UnitOfSocialOrganisation','http://dacura.cs.tcd.ie/data/seshat#TemporalEntity']")])]]).
tests([instance=instance, schema=duplicateClasses, tests=[duplicateClasses]],
     false).
tests([instance=instance, schema=invalidDomain, tests=[invalidDomain]],
     false).
tests([instance=instance, schema=invalidRange, tests=[invalidRange]],
     false).
% Probably need an addition here for cardinality too small.
tests([instance='oneof-instance', schema='oneof-schema', tests=[localCardinalityTooLarge]],
     false).
%tests([instance=instance, schema=orphanDomains, tests=all]).
tests([instance=instance, schema=orphanInstances, tests=[localOrphanInstances]],
     false).
tests([instance=instance, schema=orphanProperties, tests=[localOrphanProperties]],
     false).
%tests([instance=instance, schema=orphanRanges, tests=all]).
%tests([instance=instance, schema=orphanSubProperties, tests=all]).
tests([instance=instance, schema=propertyCycles, tests=[propertyCycles]],
      [[json([=(error,propertyClassCycle),=(property,'http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory'),=(path,['http://dacura.cs.tcd.ie/data/seshat#controlsTerritory','http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish','http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory']),=(message,"Property Class http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory has a property class cycle with path: ['http://dacura.cs.tcd.ie/data/seshat#controlsTerritory','http://dacura.cs.tcd.ie/data/seshat#relatedToRubbish','http://dacura.cs.tcd.ie/data/seshat#associatedWithTerritory']")])]]).
tests([instance=instance, schema=typeCheck, tests=[localCheckInstanceClass]],
     false).

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
    loadDB(Schema,Instance),
    runFullValidation(Pragma,Witnesses),
    (ExpectedWitnesses = Witnesses
     *-> Fail = []
     ; Fail = [witnesses=Witnesses, expected=ExpectedWitnesses,test=Schema],
       nl,write(Schema),nl,
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
		       
		       
