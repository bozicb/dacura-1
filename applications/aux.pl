:- module(aux,[]).
:- use_module(library(semweb/rdf_db)).
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

:- rdf_meta subsumes(r,r,r,o,o).
subsumes(CC,CC,_).
subsumes(CC,CP,Schema) :-
    rdf(CC,rdfs:subClassOf,CZ,Schema),
    subsumes(CZ,CP).
subsumes(CC,CP,Schema) :-
    rdf(CZ,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    member(CC,L),
    subsumes(CZ,CP).
subsumes(CC,CP,Schema) :-
    rdf(CC,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(CZ,L),
    subsumes(CZ,CP).
    
:- rdf_meta valid(r,r,r,o,o).
valid(X,CC,Instance,Schema) :-
    setof(CZ,rdf(CC,rdfs:subClassOf,CZ,Schema),L),
    forall(member(C,L), 
	   valid(X,C,Instance,Schema)). 
valid(X,CC,_,Schema) :-
    rdf(CC,rdfs:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(X,L).
valid(X,CC,Instance,Schema) :-
    rdf(CC,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    forall(member(C,L),valid(X,C,Instance,Schema)).
valid(X,CC,Instance,Schema) :-
    rdf(CC,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    member(C,L),
    valid(X,C,Instance,Schema).
valid(X,CC,Instance,Schema) :-
    rdf(CC,owl:complementOf,CN,Schema), \+ valid(X,CN,Instance,Schema).
valid(X,CC,Instance,Schema) :-
    rdf(CC,owl:'Restriction', CR, Schema),
    eltRestriction(X,CR,Instance,Schema).
valid(_,CC,_,Schema) :-
    \+ hasFurtherConstraint(CC,Schema).  % we are already subsumed, so also valid
                                         % as there are no further constraints

hasFurtherConstraint(CC,Schema) :-
    rdf(CC,rdfs:subClassOf,_,Schema).
hasFurtherConstraint(CC,Schema) :-
    rdf(CC,owl:oneOf,_,Schema).
hasFurtherConstraint(CC,Schema) :-
    rdf(CC,owl:intersectionOf,_,Schema).
hasFurtherConstraint(CC,Schema) :-
    rdf(CC,owl:unionOf,_,Schema).
hasFurtherConstraint(CC,Schema) :-
    rdf(CC,owl:complementOf,_,Schema).
hasFurtherConstraint(CC,Schema) :-
    rdf(CC,owl:'Restriciton',_,Schema).
       

runChain(X,[P],Y,Instance,Schema) :-
    inferredEdge(X,P,Y,Instance,Schema).
runChain(X,[P|PropList],Z,Instance,Schema) :-
    inferredEdge(X,P,Y,Instance,Schema),
    runChain(Y,PropList,Z,Instance,Schema).

:- rdf_meta inferredEdge(r,r,r,o,o).
inferredEdge(X,OP,Y,Instance,Schema) :-
    rdf(OP,rdf:type,owl:'ObjectProperty', Schema),
    rdf(X,OP,Y,Instance).
inferredEdge(X,OP,Y,Instance,Schema) :-
    rdf(OP,rdf:type,owl:'TransitiveProperty', Schema),
    rdf(X,OP,Z,Instance),
    inferredEdge(Z,OP,Y,Instance,Schema).
inferredEdge(X,OP,Y,Instance,Schema) :-
    rdf(OP,rdf:propertyChain,ListObj, Schema),
    collect(ListObj,PropList,Schema),
    runChain(X,PropList,Y,Instance,Schema).
inferredEdge(X,OP,Y,Instance,Schema) :- 
    rdf(SOP,rdf:subPropertyOf,OP),
    inferredEdge(X,SOP,Y,Instance,Schema).

card(X,OP,Y,N) :-
    (setof(Y,inferredEdge(X,OP,Y), ListX) *-> ListX = L ; L = []),
    length(L,N).

qualitifiedCard(X,OP,Y,C,Instance,Schema,N) :-
    (setof(Y,(inferredEdge(X,OP,Y),
	      elt(Y,C,Instance,Schema)),
	   ListX) *-> ListX = L ; L = []),
    length(L,N).

:- rdf_meta eltRestriction(r,r,o,o).
eltRestriction(X,CR,Instance,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:someValuesFrom,C,Schema),
    inferredEdge(X,OP,Y,Instance,Schema),
    elt(Y,C,Instance,Schema).
eltRestriction(X,CR,Instance,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:allValuesFrom,C,Schema),
    forall(inferredEdge(X,OP,Y,Instance,Schema),
	   elt(Y,C,Instance,Schema)).
eltRestriction(X,CR,_,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:minCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,M),
    M >= N.
eltRestriction(X,CR,_,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:maxCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,M),
    N >= M.
eltRestriction(X,CR,_,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,N).
eltRestriction(X,CR,Instance,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:minQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    rdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,M),
    M >= N.
eltRestriction(X,CR,Instance,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:maxQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    rdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,M),
    N >= M.
eltRestriction(X,CR,Instance,Schema) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    rdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,N).

:- rdf_meta class(r,o).
class(X,Schema) :- rdf(X, rdf:type, rdfs:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Restriction', Schema).

:- rdf_meta elt(r,r,o,o).
elt(X,CP,Instance,Schema) :-
    rdf(X, rdf:type, CC, Instance),
    subsumes(CC,CP,Schema),
    valid(X,CP,Instance,Schema).

%%%%verifyEdge(X,P,Instance,Schema) :- 
%%%%    valid(X, CC, Instance, Schema).
