:- module(aux,[]).
:- use_module(schemaRules).
% This file carries auxilliary predicates that need to be used
% for reasoning tests.

% In order to apply property and class constraints to properties for DL, it's
% necessary to be a bit careful about the mechanism of reconstruction, especially
% considering transitivity of properties which can entail duplication of checking.
%%%% classes(X,[]).
%%%% classes(X,[C|L]) :-
%%     instanceClass(X, C),
%%     classes(X,L).

%%%% validateClass(X,Message1,Message2,Instance,Schema) :-
%%     (instanceClass(X,C,Instance) *-> 
%% 		  class(C) ; Message2 
%%      ; M = M)

%%%% validateObject(X,DB,Message,Instance,Schema) :-
%%     validateClass(X,Message,Instance,Schema),
%%     validateProperties(X,Instance,Schema).

:- rdf_meta collect(r,t,o).
collect(rdf:nil,[],_).
collect(X,[H|T],Graph) :-
    rdf(X,rdf:first,H,Graph),
    rdf(X,rdf:rest,Y,Graph),
    collect(Y,T,Graph).

:- rdf_meta less(r,r,o).
leStep(CC,CP,Schema) :-
    rdf(CC,rdfs:subClassOf,CP, Schema). % 3. Axiom
leStep(CC,CP,Schema) :-
    rdf(CC,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    forall(member(C,L),le(C,CP)).
leStep(CC,CP,Schema) :-
    rdf(CC,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    member(C,L),
    le(C,CP).
leStep(CC,CN,Schema) :-
    rdf(CC,owl:complementOf,CN,Schema),
    \+ le(CC,CN,Schema).
    
:- rdf_meta le(r,r,o).
le(CC,CC,_).                              % 1. reflexive
le(owl:'Nothing',_,_) :- !,false.         % 2. Impossible to be a subset of nothing, unless nothing
le(_,owl:'Thing',_).                      % 3. everything a subset of top.
le(CC,CP,Schema) :-
    leStep(CC,Z,Schema), le(Z,CP,Schema). % 4. Need graph progress as termination.

elt(X,CP,Instance,Schema) :-
    instanceClass(X,CC,Instance),
    le(X,CC,CP,Instance,Schema).
    
