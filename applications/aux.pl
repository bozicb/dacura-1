:- module(aux,[nelt/5]).
:- use_module(library(semweb/rdf_db)).
:- use_module(schemaRules).
:- use_module(datatypes).

% Implementation of instance checking.

% Collect the RDF list into a prolog list
% It may be better to treat lists programmatically rather than
% collective them, using a derived predicate like rdflistMembership
:- rdf_meta collect(r,t,o).
collect(rdf:nil,[],_).
collect(X,[H|T],Graph) :-
    rdf(X,rdf:first,H,Graph),
    rdf(X,rdf:rest,Y,Graph),
    collect(Y,T,Graph).

% X is invalid at C for Reason
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

% Run a property axiom chain PropList from X to Y
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

% All available triples under inference
% [ owl:inverseOf, owl:ReflexiveProperty not yet implemented ]
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

% X has cardinality N at property OP
card(X,OP,Y,Instance,Schema,N) :-
    (setof(Y,inferredEdge(X,OP,Y,Instance,Schema), ListX) *-> ListX = L ; L = []),
    length(L,N).

% X has qualified cardinality N at property OP and class C
qualifiedCard(X,OP,Y,C,Instance,Schema,N) :-
    (setof(Y,(inferredEdge(X,OP,Y,Instance,Schema),
	      \+ nelt(Y,C,Instance,Schema,_)),
	   ListX) *-> ListX = L ; L = []),
    length(L,N).

% X is not an element of the restriction CR (for Reason)
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

% Implements class subsumption
:- rdf_meta subsumes(r,r,o).
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

% X is not an element of CP for Reason
:- rdf_meta nelt(r,r,o,o,t).
nelt(X,CP,Instance,Schema,Reason) :-
    rdf(X, rdf:type, CC, Instance),
    (subsumes(CC,CP,Schema) *->
       invalid(X,CC,Instance,Schema,Reason)
     ; Reason = [reason='Subsumption Impossible',
		 element=X,
		 class=CP,
		 instanceClass=CC]).

%nrange(P,R,Schema) :- rdf(P2, rdfs:range, R, Schema), subsumptionPropertiesOf(P,P2,Schema).

% The triple (X,P,Y) comes from the Herbrand base.
%:- rdf_meta invalidEdge(r,r,r,o,o,t).
%% invalidEdge(X,P,Y,Instance,Schema,Reason) :-
%%     rdf(X,P,Y,Instance), % Check to see if we were deleted or added.
%%     subsumptionPropertiesOf(P,SuperP).
%%     \+ domain(SuperP,C,Schema),format(X,X
%%     Reason = [reason='Property has no well defined domain.',
%% 	      object=X
%% invalidEdge(X,P,Y,Instance,Schema,Reason) :-
%%     rdf(X,P,Y,Instance), % Check to see if we were deleted or added.
%%     subsumptionPropertiesOf(P,SuperP).
%%     domain(SuperP,C,Schema),
%%     nelt(X,C,Instance,Scheama,Reason).
%% invalidEdge(X,P,Y,Instance,Schema,Reason) :-
%%     rdf(X,P,Y,Instance), % Check to see if we were deleted or added.
%%     subsumptionPropertiesOf(P,SuperP).
%%     range(SuperP,C,Schema),
%%     nelt(X,C,Instance,Scheama,Reason).
%% invalidEdge(X,P,Y,Instance,Schema,Reason) :-
%%     \+ rdf(X,P,Y,Instance), % Check to see if we were deleted or added.
%%     domain(P),
%%     subsumptionPropertiesOf(P,SuperP).
