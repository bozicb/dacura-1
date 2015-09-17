:- module(aux,[]).
:- use_module(library(semweb/rdf_db)).
:- use_module(schemaRules).
:- use_module(xsdParser).
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
    
:- rdf_meta invalid(r,r,o,o,t).
invalid(X,CC,Instance,Schema,Reason) :-
    rdf(CC,rdfs:subClassOf,CZ,Schema),
    invalid(X,CZ,Instance,Schema,Reason).
invalid(X,CC,_,Schema,Reason) :-
    rdf(CC,rdfs:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    \+ member(X,L),
    Reason = [reason='Not an element of enumeration (oneOf)',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema,Reason) :-
    rdf(CC,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(C,L),
    invalid(X,C,Instance,Schema, Reason).
invalid(X,CC,Instance,Schema,Reason) :-
    rdf(CC,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    forall(member(C,L),
	   invalid(X,C,Instance,Schema,_)),
    Reason = [reason='No element of union is valid',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema, Reason) :-
    rdf(CC,owl:complementOf,CN,Schema),
    \+ invalid(X,CN,Instance,Schema,_),
    Reason = [reason='Complement is valid',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema,Reason) :-
    rdf(CC, rdf:type, owl:'Restriction', Schema),
    neltRestriction(X,CC,Instance,Schema,Reason).

%%%%% May not be relevant anymore
%% %% invalid(_,CC,_,Schema) :-
%% %%    \+ hasFurtherConstraint(CC,Schema).  % we are already subsumed, so also valid
%% %%                                         % as there are no further constraints

%% %% hasFurtherConstraint(CC,Schema) :-
%% %%     rdf(CC,rdfs:subClassOf,_,Schema).
%% %% hasFurtherConstraint(CC,Schema) :-
%% %%     rdf(CC,owl:oneOf,_,Schema).
%% %% hasFurtherConstraint(CC,Schema) :-
%% %%     rdf(CC,owl:intersectionOf,_,Schema).
%% %% hasFurtherConstraint(CC,Schema) :-
%% %%     rdf(CC,owl:unionOf,_,Schema).
%% %% hasFurtherConstraint(CC,Schema) :-
%% %%     rdf(CC,owl:complementOf,_,Schema).
%% %% hasFurtherConstraint(CC,Schema) :-
%% %%     rdf(CC,owl:'Restriciton',_,Schema).

runChain(X,[P],Y,Instance,Schema) :-
    inferredEdge(X,P,Y,Instance,Schema).
runChain(X,[P|PropList],Z,Instance,Schema) :-
    inferredEdge(X,P,Y,Instance,Schema),
    runChain(Y,PropList,Z,Instance,Schema).

% Impose an ordering to avoid non-termination (subproperty ordering)
% Concrete links are already in InferredEdge
inferredTransitiveEdge(X,OP,Z,Instance,Schema) :-
    rdf(SOP,rdfs:subPropertyOf,OP,Schema),
    inferredEdge(X,SOP,Y,Instance,Schema),
    inferredEdge(Y,OP,Z,Instance,Schema).

:- rdf_meta inferredEdge(r,r,r,o,o).
inferredEdge(X,OP,Y,Instance,Schema) :-
    rdf(OP,rdf:type,owl:'ObjectProperty', Schema),
    rdf(X,OP,Y,Instance).
inferredEdge(X,OP,Y,Instance,Schema) :-
    rdf(OP,rdf:type,owl:'TransitiveProperty', Schema),
    inferredTransitiveEdge(X,OP,Y,Instance,Schema).
inferredEdge(X,OP,Y,Instance,Schema) :-
    rdf(OP,owl:propertyChain,ListObj, Schema),
    collect(ListObj,PropList,Schema),
    runChain(X,PropList,Y,Instance,Schema).
inferredEdge(X,OP,Y,Instance,Schema) :- 
    rdf(SOP,rdfs:subPropertyOf,OP,Schema),
    inferredEdge(X,SOP,Y,Instance,Schema).

card(X,OP,Y,Instance,Schema,N) :-
    (setof(Y,inferredEdge(X,OP,Y,Instance,Schema), ListX) *-> ListX = L ; L = []),
    length(L,N).

qualitifiedCard(X,OP,Y,C,Instance,Schema,N) :-
    (setof(Y,(inferredEdge(X,OP,Y,Instance,Schema),
	      elt(Y,C,Instance,Schema)),
	   ListX) *-> ListX = L ; L = []),
    length(L,N).

:- rdf_meta neltRestriction(r,r,o,o,t).
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:someValuesFrom,C,Schema),
    forall(inferredEdge(X,OP,Y,Instance,Schema),
           (\+ nelt(Y,C,Instance,Schema,_))),
    Reason = [reason='No values from restriction class',
	      element=X,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:allValuesFrom,C,Schema),
    inferredEdge(X,OP,Y,Instance,Schema),
    nelt(Y,C,Instance,Schema,_),
    Reason = [reason='Some values not from restriction class',
	      element=X,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:minCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    M < N, atom_number(A,M),
    Reason = [reason='Cardinality too small on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:maxCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    N < M, atom_number(A,M),
    Reason = [reason='Cardinality too large on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    N \= M, atom_number(A,M),
    Reason = [reason='Cardinality unequal on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:minQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    rdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,M),
    M < N, atom_number(A,M),
    Reason = [reason='Qualified Cardinality too small on restriction class',
	      element=X,
	      cardinality=A,
	      qualifiedOn=C,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:maxQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    rdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,M),
    N < M, atom_number(A,M),
    Reason = [reason='Qualified Cardinality too large on restriction class',
	      element=X,
	      cardinality=A,
	      qualifiedOn=C,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:qualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    rdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,N),
    N \= M, atom_number(A,M),
    Reason = [reason='Qualified Cardinality unequal on restriction class',
	      element=X,
	      cardinality=A,
	      qualifiedOn=C,
	      class=CR].

%% %% :- rdf_meta class(r,o).
%% %% class(X,Schema) :- rdf(X, rdf:type, rdfs:'Class', Schema).
%% %% class(X,Schema) :- rdf(X, rdf:type, owl:'Class', Schema).
%% %% class(X,Schema) :- rdf(X, rdf:type, owl:'Restriction', Schema).

:- rdf_meta nsubsumes(r,r,r,o,o).
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

:- rdf_meta nelt(r,r,o,o,t).
nelt(X,CP,Instance,Schema,Reason) :-
    rdf(X, rdf:type, CC, Instance),
    (subsumes(CC,CP,Schema) *->
       invalid(X,CC,Instance,Schema,Reason)
     ; Reason = [reason='Subsumption Impossible',
		 element=X,
		 class=CP,
		 instanceClass=CC]).

:- rdf_meta nbasetypeElt(r,r,t).
nbasetypeElt(literal(S),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(S,A),
    Reason = [reason='Expected atom, found term',
	      literal=A].
nbasetypeElt(literal(lang(S,L)),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(S,A),
    Reason = [reason='Expected atom in string section, found term.',
	      literal=A].
nbasetypeElt(literal(lang(S,L)),xsd:string,Reason) :-
    \+ atom(L), term_to_atom(L,A),
    Reason = [reason='Expected atom in language section, found term.',
	      literal=A].
nbasetypeElt(literal(type(T,S)),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(type(T,S),A),
    Reason = [reason='Expected atom, found term as element.',
	      literal=A].
nbasetypeElt(literal(type(T,S)),xsd:string,Reason) :-
    \+ atom(T), term_to_atom(type(T,S),A),
    Reason = [reason='Expected atom, found term as type.',
	      literal=A].
nbasetypeElt(literal(type(T1,S)),T2,Reason) :-
    \+ dataTypeSubsumes(T1,T2), term_to_atom(T1,A), term_to_atom(T2,B),
    Reason = [reason='Could not subsume type1 with type2',
	      type1=A,
	      type2=B].
nbasetypeElt(literal(type(_,S)),xsd:boolean,Reason) :-
    \+ member(S,['true','false','1','0']), term_to_atom(S,A),
    Reason = [reason='Not a well formed boolean.',
	      literal=A].
nbasetypeElt(literal(type(_,S)),xsd:decimal,Reason) :-
    \+ 
nbasetypeElt(xsd:integer). 
nbasetypeElt(xsd:double). 
nbasetypeElt(xsd:float). 
nbasetypeElt(xsd:time).
nbasetypeElt(xsd:dateTime). 
nbasetypeElt(xsd:dateTimeStamp).
nbasetypeElt(xsd:gYear). 
nbasetypeElt(xsd:gMonth). 
nbasetypeElt(xsd:gDay). 
nbasetypeElt(xsd:gYearMonth). 	
nbasetypeElt(xsd:gMonthDay). 
nbasetypeElt(xsd:duration). 
nbasetypeElt(xsd:yearMonthDuration). 
nbasetypeElt(xsd:dayTimeDuration). 
nbasetypeElt(xsd:byte). 
nbasetypeElt(xsd:short). 
nbasetypeElt(xsd:int). 
nbasetypeElt(xsd:long). 
nbasetypeElt(xsd:unsignedByte). 
nbasetypeElt(xsd:unsignedInt). 
nbasetypeElt(xsd:unsignedLong). 
nbasetypeElt(xsd:positiveInteger). 
nbasetypeElt(xsd:nonNegativeInteger). 
nbasetypeElt(xsd:negativeInteger). 
nbasetypeElt(xsd:nonPositiveInteger). 
nbasetypeElt(xsd:base64Binary). 
nbasetypeElt(xsd:anyURI). 
nbasetypeElt(xsd:language). 
nbasetypeElt(xsd:normalizedString). 
nbasetypeElt(xsd:token). 
nbasetypeElt(xsd:'NMTOKEN'). 
nbasetypeElt(xsd:'Name'). 
nbasetypeElt(xsd:'NCName'). 
nbasetypeElt(rdf:'PlainLiteral').
nbasetypeElt(rdf:'Literal').

% The triple (X,P,Y) comes from the Herbrand base.
%:- rdf_meta invalidEdge(r,r,r,o,o,t).
%invalidEdge(X,P,Y,Instance,Schema,Reason) :-
%    rdf(X,P,Y,Instance), % Check to see if we were deleted or added.
%    domain(P),
%    subsumptionPropertiesOf(P,SuperP),
%    subsumptionPropertiesOf(SubP,P).
