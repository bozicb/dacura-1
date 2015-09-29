:- module(abox,[invalidEdge/6]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils). 
:- use_module(datatype).

% X is invalid at C for Reason
:- rdf_meta invalid(r,r,o,o,t).
invalid(X,CC,Instance,Schema,Reason) :-
    subClassOf(CC,CZ,Schema),
    invalid(X,CZ,Instance,Schema,Reason).
invalid(X,CC,_,Schema,Reason) :-
    rdf(CC,rdfs:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    \+ member(X,L),
    Reason = [error=objectInvalidAtClass,
	      message='Not an element of enumeration (oneOf)',
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
    Reason = [error=objectInvalidAtClass,
	      message='No element of union is valid',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema, Reason) :-
    rdf(CC,owl:complementOf,CN,Schema),
    \+ invalid(X,CN,Instance,Schema,_),
    Reason = [error=objectInvalidAtClass,
	      message='Complement is valid',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema,Reason) :-
    restriction(CC,Schema),
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
    Reason = [error=notRestrictionElement,
	      message='No values from restriction class',
	      element=X,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:allValuesFrom,C,Schema),
    inferredEdge(X,OP,Y,Instance,Schema),
    nelt(Y,C,Instance,Schema,_),
    Reason = [error=notRestrictionElement,
	      message='Some values not from restriction class',
	      element=X,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:minCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    M < N, atom_number(A,M),
    Reason = [error=notRestrictionElement,
	      message='Cardinality too small on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:maxCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    N < M, atom_number(A,M),
    Reason = [error=error=notRestrictionElement,
	      message='Cardinality too large on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    rdf(CR,owl:onProperty,OP,Schema),
    rdf(CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    N \= M, atom_number(A,M),
    Reason = [error=notRestrictionElement,
	      message='Cardinality unequal on restriction class',
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
    Reason = [error=notRestrictionElement,
	      message='Qualified Cardinality too small on restriction class',
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
    Reason = [error=notRestrictionElement,
	      message='Qualified Cardinality too large on restriction class',
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
    Reason = [error=notRestrictionElement,
	      message='Qualified Cardinality unequal on restriction class',
	      element=X,
	      cardinality=A,
	      qualifiedOn=C,
	      class=CR].

% X is not an element of CP for Reason
:- rdf_meta nelt(r,r,o,o,t).
nelt(X,CP,Instance,Schema,Reason) :-
    rdf(X, rdf:type, CC, Instance),
    (subsumes(CC,CP,Schema) *->
       invalid(X,CC,Instance,Schema,Reason)
     ; Reason = [error=doesNotSubsume, 
		 message='Subsumption Impossible',
		 element=X,
		 class=CP,
		 instanceClass=CC]).
nelt(X,CP,Instance,Schema,Reason) :-
    nbasetypeElt(X,CP,Reason).

%nrange(P,R,Schema) :- rdf(P2, rdfs:range, R, Schema), subsumptionPropertiesOf(P,P2,Schema).

instanceClass(X, Y, Instance) :- rdf(X, rdf:type, Y, Instance).
orphanInstance(X,C,Instance,Schema) :- instanceClass(X,C,Instance), \+ class(C,Schema).

:- rdf_meta edgeOrphanInstance(r,r,r,o,o,t).
edgeOrphanInstance(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    \+ instanceClass(X, _),
    Reason=[error=edgeOrphanInstance,
	    message='Instance has no class',
	    subject=X,
	    predicate=P,
	    object=Y].
edgeOrphanInstance(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    orphanInstance(X,C,Instance,Schema),
    Reason=[error=edgeOrphanInstance,
	    message='Instance domain class is not valid',
	    subject=X,
	    predicate=P,
	    object=Y,
	    class=C].
edgeOrphanInstance(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    objectProperty(P,Schema),
    \+ instanceClass(Y, _),
    Reason=[error=edgeOrphanInstance,
	    message='Instance has no class',
	    subject=X,
	    predicate=P,
	    object=Y].
edgeOrphanInstance(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    objectProperty(P,Schema),
    orphanInstance(Y,C,Instance,Schema),
    Reason=[error=edgeOrphanInstance,
	    message='Instance has no class',
	    subject=X,
	    predicate=P,
	    object=Y,
	    class=C].
	       
% The triple (X,P,Y) comes from the Herbrand base.
:- rdf_meta invalidEdge(r,r,r,o,o,t).
noPropertyDomain(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    subsumptionPropertiesOf(P,SuperP),
    \+ domain(SuperP,C,Schema),
    Reason = [error=noPropertyDomain,
	      message=='Property has no well defined domain.',
	      subject=X,
	      predicate=SuperP,
	      object=Y].

noPropertyRange(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    subsumptionPropertiesOf(P,SuperP).
    \+ range(SuperP,C,Schema),
    Reason = [error=invalidEdge,
	      message='Property has no well defined range.',
	      subject=X,
	      predicate=SuperP,
	      object=Y].

invalidEdge(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Check to see if we were deleted or added.
    subsumptionPropertiesOf(P,SuperP).
    domain(SuperP,D,Schema),
    nelt(X,D,Instance,Scheama,Reason).
invalidEdge(X,P,Y,Instance,Schema,Reason) :-
    rdf(X,P,Y,Instance), % Added
    subsumptionPropertiesOf(P,SuperP).
    range(SuperP,R,Schema),
    nelt(Y,R,Instance,Scheama,Reason).
invalidEdge(X,P,Y,Instance,Schema,Reason) :-
    %% we need to check range/ domain of deleted predicates to make sure the cardinality
    %% is still respected
    \+ rdf(X,P,Y,Instance), % Deleted
    subsumptionPropertiesOf(P,SuperP),
    restrictionOnProperty(CR,P,Schema),
    neltRestriction(_,CR,Instance,Schema,Reason).

notFunctionalProperty(X,P,Y,Instance,Schema,Reason) :-
    functionalProperty(P,Schema),
    card(X,P,_,Instance,Schema,N),
    (N \= 1 ; N \= 0),
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = [error=functionalPropertyError,
	      subject=X,
	      predicate=P,
	      message=M].

localOrphanProperty(X,P,Y,Instance,Schema,Reason) :-
    \+ property(P,Schema),
    interpolate(['No property class associated with property: ',P,'.'],Message),
    Reason=[error=noInstancePropertyClass,
	    subject=X,
	    predicate=P,
	    object=Y,
	    message=Message].

instanceSubjectBlankNode(X,Instance,_) :- rdf(X,_,_,Instance), rdf_is_bnode(X).
instancePredicateBlankNode(Y,Instance,_) :- rdf(_,Y,_,Instance), rdf_is_bnode(Y).
instanceObjectBlankNode(Z,Instance,_) :- rdf(_,_,Z,Instance), rdf_is_bnode(Z).

instanceBlankNode(Instance,Schema,Reason) :-
    instanceSubjectBlankNode(X,Instance,Schema),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    subject=X].
instanceBlankNode(Instance,Schema,Reason) :-
    instancePredicateBlankNode(X,Instance,Schema),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    predicate=X].
instanceBlankNode(Instance,Schema,Reason) :-
    instanceObjectBlankNode(X,Instance,Schema),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    object=X].		

edgeConstraints(edgeOrphanInstance). 
edgeConstraints(noPropertyDomain).
edgeConstraints(noPropertyRange).
edgeConstraints(invalidEdge).
edgeConstraints(localOrphanProperty).
edgeConstraints(notFunctionalProperty).
edgeConstraints(instanceBlankNode).
