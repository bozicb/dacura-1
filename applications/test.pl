:- module(test,[runTests/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test harness.

testPairs([

runTests(Witness) :-
    % fill with the witness of failure. 



    
demoDB(Schema,Instance) :-
    rdf_retractall(_, _, _, instance), 
    rdf_retractall(_, _, _, schema), 
    rdf_load(Schema, [graph(schema)]), 
    rdf_load(Instance, [graph(instance)]).

demoDB(Schema,Instance,Options) :-
    rdf_retractall(_, _, _, instance), 
    rdf_retractall(_, _, _, schema), 
    rdf_load(Schema, [graph(schema)|Options]),     
    rdf_load(Instance, [graph(instance)|Options]). 
-

		       
		       
