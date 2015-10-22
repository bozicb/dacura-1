:- module(abox,[
		%% Predicates
		
		%%%% IC = Instance Constraints 
		%% Constraints must be pred/6 
		%% Best practice
		noPropertyDomainIC/6,
		noPropertyRangeIC/6,
		instanceBlankNodeIC/6,

		%% OWL DL (Constraint)
		invalidEdgeIC/6,
		edgeOrphanInstanceIC/6,
		notFunctionalPropertyIC/6,
		notInverseFunctionalPropertyIC/6,
		localOrphanPropertyIC/6
	       ]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils). 
:- use_module(datatypes).
:- use_module(xsdParser).
:- use_module(library(uri)).
:- use_module(tbox).

% X is invalid at C for Reason
:- rdf_meta invalid(r,r,o,o,t).
invalid(X,CC,Instance,Schema,Reason) :-
    subClassOf(CC,CZ,Schema),
    invalid(X,CZ,Instance,Schema,Reason).
invalid(X,CC,_,Schema,Reason) :-
    xrdf(CC,rdfs:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    \+ member(X,L),
    Reason = [error=objectInvalidAtClass,
	      message='Not an element of enumeration (oneOf)',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema,Reason) :-
    xrdf(CC,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(C,L),
    invalid(X,C,Instance,Schema, Reason).
invalid(X,CC,Instance,Schema,Reason) :-
    xrdf(CC,owl:disjointUnionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    findall(C, (member(C,L), \+ invalid(X,C,Instance,Schema,_)), Solutions), 
    length(Solutions, N), N \= 1,
    Reason = [error=objectInvalidAtClass,
	      message='More than one branch of disjointUnion is valid.',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema,Reason) :-
    xrdf(CC,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    forall(member(C,L),
	   invalid(X,C,Instance,Schema,_)),
    Reason = [error=objectInvalidAtClass,
	      message='Element is not valid at any class of union',
	      element=X,
	      class=CC].
invalid(X,CC,Instance,Schema, Reason) :-
    xrdf(CC,owl:complementOf,CN,Schema),
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
    xrdf(SOP,rdfs:subPropertyOf,OP,Schema),
    inferredEdge(X,SOP,Y,Instance,Schema),
    inferredEdge(Y,OP,Z,Instance,Schema).

% All available triples under inference
% [ owl:inverseOf, owl:ReflexiveProperty not yet implemented ]
:- rdf_meta inferredEdge(r,r,r,o,o).
inferredEdge(X,OP,Y,Instance,Schema) :-
    property(OP, Schema), % xrdf(OP,rdf:type,owl:'ObjectProperty', Schema),
    xrdf(X,OP,Y,Instance).
inferredEdge(X,OP,Y,Instance,Schema) :-
    xrdf(OP,rdf:type,owl:'TransitiveProperty', Schema),
    inferredTransitiveEdge(X,OP,Y,Instance,Schema).
inferredEdge(X,OP,Y,Instance,Schema) :-
    xrdf(OP,owl:propertyChain,ListObj, Schema),
    collect(ListObj,PropList,Schema),
    runChain(X,PropList,Y,Instance,Schema).
inferredEdge(X,OP,Y,Instance,Schema) :- 
    xrdf(SOP,rdfs:subPropertyOf,OP,Schema),
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
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:someValuesFrom,C,Schema),
    forall(inferredEdge(X,OP,Y,Instance,Schema),
           (\+ nelt(Y,C,Instance,Schema,_))),
    Reason = [error=notRestrictionElement,
	      message='No values from restriction class',
	      element=X,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:allValuesFrom,C,Schema),
    inferredEdge(X,OP,Y,Instance,Schema),
    nelt(Y,C,Instance,Schema,_),
    Reason = [error=notRestrictionElement,
	      message='Some values not from restriction class',
	      element=X,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:minCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    M < N, atom_number(A,M),
    Reason = [error=notRestrictionElement,
	      message='Cardinality too small on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:maxCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    N < M, atom_number(A,M),
    Reason = [error=notRestrictionElement,
	      message='Cardinality too large on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N),
    card(X,OP,_,Instance,Schema,M),
    N \= M, atom_number(A,M),
    Reason = [error=notRestrictionElement,
	      message='Cardinality unequal on restriction class',
	      element=X,
	      cardinality=A,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:minQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    xrdf(CR,owl:onClass,C,Schema),
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
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:maxQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    xrdf(CR,owl:onClass,C,Schema),
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
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:qualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    xrdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N),
    qualifiedCard(X,OP,_,C,Instance,Schema,N),
    N \= M, atom_number(A,M),
    Reason = [error=notRestrictionElement,
	      message='Qualified Cardinality unequal on restriction class',
	      element=X,
	      cardinality=A,
	      qualifiedOn=C,
	      class=CR].
neltRestriction(X,CR,Instance,Schema,Reason) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:hasValue,V,Schema),
    inferredEdge(X,OP,Y,Instance,Schema),
    Y \= V, 
    Reason = [error=notRestrictionElement,
	      message='hasValue constraint violated',
	      element=X,
	      value=V,
	      class=CR].


% X is not an element of CP for Reason
:- rdf_meta nelt(r,r,o,o,t).
nelt(X,CC,Instance,Schema,Reason) :-
    class(CC,Schema),
    invalid(X,CC,Instance,Schema,Reason).
nelt(X,CC,Instance,Schema,Reason) :-
    subsumptionOf(CC,CP,Schema),
    invalid(X,CP,Instance,Schema,Reason).
nelt(X,CP,_,_,Reason) :- 
    baseType(CP),
    nbasetypeElt(X,CP,Reason).
nelt(X,CP,_,Schema,Reason) :-
    customDatatype(CP,Schema),
    xrdf(CP,rdfs:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    \+ member(X,L),
    Reason = [error=dataInvalidAtDatatype,
	      message='Not an element of enumeration (oneOf)',
	      element=X,
	      datatype=CP].
nelt(X,CP,_,Schema,Reason) :-
    customDatatype(CP,Schema),
    xrdf(CP,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(C,L),
    nbasetypeElt(X,C,Reason),
    Reason = [error=dataInvalidAtDatatype,
	      message='Not an element of intersection',
	      element=X,
	      datatype=CP].
nelt(X,CP,_,Schema,Reason) :-
    customDatatype(CP,Schema),
    xrdf(CP,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    forall(member(C,L),
	   nbasetypeElt(X,C,_)),
    Reason = [error=dataInvalidAtDatatype,
	      message='Not an element of union',
	      element=X,
	      class=CP].

%nrange(P,R,Schema) :- xrdf(P2, rdfs:range, R, Schema), subsumptionPropertiesOf(P,P2,Schema).

instanceClass(X, Y, Graph) :- xrdf(X, rdf:type, Y, Graph).

orphanInstance(X,C,Instance,Schema) :- instanceClass(X,C,Instance), \+ class(C,Schema).
orphanInstance(X,C,_,Schema) :- instanceClass(X,C,Schema), \+ class(C,Schema). % Check abox assertions in schema (oneOf uses this)

:- rdf_meta edgeOrphanInstance(r,r,r,o,o,t).
edgeOrphanInstanceIC(X,P,Y,Instance,Schema,Reason) :-
    xrdf(X,P,Y,Instance), % Added
    \+ instanceClass(X, _, Instance),
    \+ instanceClass(X,_,Schema),
    Reason=[error=edgeOrphanInstance,
	    message='Instance has no class',
	    subject=X,
	    predicate=P,
	    object=Y].
edgeOrphanInstanceIC(X,P,Y,Instance,Schema,Reason) :-
    xrdf(X,P,Y,Instance), % Added
    orphanInstance(X,C,Instance,Schema),
    Reason=[error=edgeOrphanInstance,
	    message='Instance domain class is not valid',
	    subject=X,
	    predicate=P,
	    object=Y,
	    class=C].
edgeOrphanInstanceIC(X,P,Y,Instance,Schema,Reason) :-
    objectProperty(P,Schema),	
    xrdf(X,P,Y,Instance), % Added
    \+ instanceClass(Y, _, Instance),
    \+ instanceClass(Y,_,Schema),
    Reason=[error=edgeOrphanInstance,
	    message='Instance has no class',
	    subject=X,
	    predicate=P,
	    object=Y].
edgeOrphanInstanceIC(X,P,Y,Instance,Schema,Reason) :-
    objectProperty(P,Schema),
    xrdf(X,P,Y,Instance), % Added
    orphanInstance(Y,C,Instance,Schema),
    Reason=[error=edgeOrphanInstance,
	    message='Instance has no class',
	    subject=X,
	    predicate=P,
	    object=Y,
	    class=C].
	       
% The triple (X,P,Y) comes from the Herbrand base.
:- rdf_meta invalidEdge(r,r,r,o,o,t).
noPropertyDomainIC(X,P,Y,Instance,Schema,Reason) :-
    property(P,Schema),
    xrdf(X,P,Y,Instance), % Added
    subsumptionPropertiesOf(P,SuperP,Schema),
    \+ domain(SuperP,_,Schema),
    Reason = [error=noPropertyDomain,
	      message='Property has no well defined domain.',
	      subject=X,
	      predicate=SuperP,
	      object=Y].

noPropertyRangeIC(X,P,Y,Instance,Schema,Reason) :-
    property(P,Schema),
    xrdf(X,P,Y,Instance), % Added
    subsumptionPropertiesOf(P,SuperP,Schema),
    \+ range(SuperP,_,Schema),
    Reason = [error=invalidEdge,
	      message='Property has no well defined range.',
	      subject=X,
	      predicate=SuperP,
	      object=Y].

invalidEdgeIC(X,P,Y,Instance,Schema,Reason) :-
    property(P,Schema),
    xrdf(X,P,Y,Instance), % Check to see if we were deleted or added.
    subsumptionPropertiesOf(P,SuperP,Schema),
    domain(SuperP,D,Schema),
    nelt(X,D,Instance,Schema,Reason).
invalidEdgeIC(X,P,Y,Instance,Schema,Reason) :-
    property(P,Schema),
    xrdf(X,P,Y,Instance), % Added
    subsumptionPropertiesOf(P,SuperP,Schema),
    range(SuperP,R,Schema),
    nelt(Y,R,Instance,Schema,Reason).
invalidEdgeIC(X,P,Y,Instance,Schema,Reason) :-
    %% we need to check range/ domain of deleted predicates to make sure the cardinality
    %% is still respected
    \+ xrdf(X,P,Y,Instance), % Deleted
    property(P,Schema),
    subsumptionPropertiesOf(P,SuperP,Schema),
    restrictionOnProperty(CR,SuperP,Schema),
    neltRestriction(_,CR,Instance,Schema,Reason).

notFunctionalPropertyIC(X,P,_,Instance,Schema,Reason) :-
    functionalProperty(P,Schema),
    xrdf(X,P,_,Instance),
    card(X,P,_,Instance,Schema,N),
    N \= 1,
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = [error=functionalPropertyError,
	      subject=X,
	      predicate=P,
	      message=Message].

notInverseFunctionalPropertyIC(X,P,Y,Instance,Schema,Reason) :-
    inverseFunctionalProperty(P,Schema),
    xrdf(_,P,Y,Instance),
    card(_,P,Y,Instance,Schema,N),
    N \= 1,
    interpolate(['Functional Property ',P,' is not functional.'],Message),
    Reason = [error=functionalPropertyError,
	      subject=X,
	      predicate=P,
	      object=Y,
	      message=Message].

localOrphanPropertyIC(X,P,Y,Instance,Schema,Reason) :-
    xrdf(X,P,Y,Instance), \+ P='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    \+ property(P,Schema), 
    interpolate(['No property class associated with property: ',P,'.'],Message),
    Reason=[error=noInstancePropertyClass,
	    subject=X,
	    predicate=P,
	    object=Y,
	    message=Message].

instanceSubjectBlankNode(X,Instance,_) :- xrdf(X,_,_,Instance), rdf_is_bnode(X).
instancePredicateBlankNode(Y,Instance,_) :- xrdf(_,Y,_,Instance), rdf_is_bnode(Y).
instanceObjectBlankNode(Z,Instance,_) :- xrdf(_,_,Z,Instance), rdf_is_bnode(Z).

instanceBlankNodeIC(X,_P,_Y,Instance,Schema,Reason) :-
    instanceSubjectBlankNode(X,Instance,Schema),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    subject=X].
instanceBlankNodeIC(_,X,_,Instance,Schema,Reason) :-
    instancePredicateBlankNode(X,Instance,Schema),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    predicate=X].
instanceBlankNodeIC(_,_,X,Instance,Schema,Reason) :-
    instanceObjectBlankNode(X,Instance,Schema),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    object=X].		

daysInMonth(_,1,31).
daysInMonth(Y,2,D) :- Ans is Y mod 4, Ans = 0 -> D = 29 ; D = 28 .
daysInMonth(_,3,31).
daysInMonth(_,4,30).
daysInMonth(_,5,31).
daysInMonth(_,6,30).
daysInMonth(_,7,31).
daysInMonth(_,8,31).
daysInMonth(_,9,30).
daysInMonth(_,10,31).
daysInMonth(_,11,30).
daysInMonth(_,12,31).

:- rdf_meta nbasetypeElt(r,r,t).
nbasetypeElt(literal(S),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(S,A)
    ->
    Reason = [error=nbasetypeElt,
	      message='Expected atom, found term',
	      literal=A].
nbasetypeElt(literal(lang(S,L)),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(lang(S,L),A)
    ->
    Reason = [error=nbasetypeElt,
	      message='Expected atom in string section, found term.',
	      literal=A].
nbasetypeElt(literal(lang(S,L)),xsd:string,Reason) :-
    \+ atom(L), term_to_atom(lang(S,L),A)
    ->
    Reason = [error=nbasetypeElt,
	      message='Expected atom in language section, found term.',
	      literal=A].
nbasetypeElt(literal(type(T,S)),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(type(T,S),A)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Expected atom, found term as element.',
	      literal=A].
nbasetypeElt(literal(type(T,S)),xsd:string,Reason) :-
    \+ atom(T), term_to_atom(type(T,S),A)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Expected atom, found term as type.',
	      literal=A].
nbasetypeElt(literal(type(T1,_)),T2,Reason) :-
    \+ basetypeSubsumptionOf(T1,T2)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Could not subsume type1 with type2',
	      type1=T1,
	      type2=T2].
nbasetypeElt(literal(type(_,S)),xsd:boolean,Reason) :-
    \+ member(S,['true','false','1','0']), term_to_atom(S,A)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed boolean.',
	      literal=A,
	      type='xsd:boolean'].
nbasetypeElt(literal(type(_,S)),xsd:decimal,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:decimal(_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed decimal.',
	      literal=S,
	      type='xsd:decimal'].
nbasetypeElt(literal(type(_,S)),xsd:integer,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed integer.',
	      literal=S,
	      type='xsd:integer'].
nbasetypeElt(literal(type(_,S)),xsd:double,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:double(_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed double.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:double,Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 53, N > Max
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed double: Mantisa is massive.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:double,Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(_,E,_),C,[]),
    (E > 970 ; E < -1075)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed double: exponent excessive.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:float,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:double(_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed float.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:float,Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 24, N > Max
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed float: mantisa is massive.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:float,Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(_,E,_),C,[]),
    (E > 104 ; E < -149)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed float: exponent excessive.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:time,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:time(_,_,_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:time',
	      literal=S,
	      type='xsd:time'].
nbasetypeElt(literal(type(_,S)),xsd:time,Reason) :-
    atom_codes(S,C), phrase(xsdParser:time(H,M,S,Z,ZH,ZM),C,[]),
    (H > 23 ; M > 59 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:time : parameter out of range.',
	      literal=S,
	      type='xsd:time'].
nbasetypeElt(literal(type(_,S)),xsd:date,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:date(_,_,_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:date.',
	      literal=S,
	      type='xsd:date'].
nbasetypeElt(literal(type(_,S)),xsd:dateTime,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:dateTime(_,_,_,_,_,_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:dateTime.',
	      literal=S,
	      type='xsd:dateTime'].
nbasetypeElt(literal(type(_,S)),xsd:dateTime,Reason) :-
    atom_codes(S,C), phrase(xsdParser:dateTime(SY,Mo,D,H,M,S,Z,ZH,ZM),C,[]),
    (Mo > 12 ; Mo < 1
     ; daysInMonth(SY,Mo,Days), D > Days
     ; D < 1 ; H > 23 ; M > 59
     ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:dateTime : parameter out of range.',
	      literal=S,
	      type='xsd:dateTime'].
nbasetypeElt(literal(type(_,S)),xsd:gYear,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gYear(_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYear',
	      literal=S,
	      type='xsd:gYear'].
nbasetypeElt(literal(type(_,S)),xsd:gYear,Reason) :-
    atom_codes(S,C), phrase(xsdParser:gYear(_,Z,ZH,ZM),C,[]),
    ((\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYear : parameters out of range',
	      literal=S,
	      type='xsd:gYear'].
nbasetypeElt(literal(type(_,S)),xsd:gMonth,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gMonth(_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:Month',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gMonth,Reason) :-
    atom_codes(S,C), phrase(xsdParser:gMonth(M,Z,ZH,ZM),C,[]),
    (M < 12 ; M > 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    -> 
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonth : parameters out of range',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gDay,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gDay(_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonth',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gDay,Reason) :-
    atom_codes(S,C), phrase(xsdParser:gDay(D,Z,ZH,ZM),C,[]),
    (D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonth : parameters out of range',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gYearMonth,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gYearMonth(_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYearMonth',
	      literal=S,
	      type='xsd:gYearMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gYearMonth,Reason) :-
    atom_codes(S,C), phrase(xsdParser:gYearMonth(_,M,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYearMonth : parameters out of range',
	      literal=S,
	      type='xsd:gYearMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gMonthDay,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gMonthDay(_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYearMonth',
	      literal=S,
	      type='xsd:gMonthDay'].
nbasetypeElt(literal(type(_,S)),xsd:gMonthDay,Reason) :-
    atom_codes(S,C), phrase(xsdParser:gMonthDay(M,D,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonthDay : parameters out of range',
	      literal=S,
	      type='xsd:gMonthDay'].
nbasetypeElt(literal(type(_,S)),xsd:duration,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:duration(_,_,_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:duration',
	      literal=S,
	      type='xsd:duration'].
nbasetypeElt(literal(type(_,S)),xsd:yearMonthDuration,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:yearMonthDuration(_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:yearMonthDuration',
	      literal=S,
	      type='xsd:yearMonthDuration'].
nbasetypeElt(literal(type(_,S)),xsd:dayTimeDuration,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:dayTimeDuration(_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:dayTimeDuration',
	      literal=S,
	      type='xsd:dayTimehDuration'].
nbasetypeElt(literal(type(_,S)),xsd:byte,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:byte',
	      literal=S,
	      type='xsd:byte'].
nbasetypeElt(literal(type(_,S)),xsd:byte,Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -128 ; I > 127 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:byte: out of range.',
	      literal=S,
	      type='xsd:byte'].
nbasetypeElt(literal(type(_,S)),xsd:short,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:short',
	      literal=S,
	      type='xsd:short'].
nbasetypeElt(literal(type(_,S)),xsd:short,Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -32768 ; I > 32767 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:short: out of range.',
	      literal=S,
	      type='xsd:short'].
nbasetypeElt(literal(type(_,S)),xsd:int,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:int',
	      literal=S,
	      type='xsd:int'].
nbasetypeElt(literal(type(_,S)),xsd:int,Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -2147483648 ; I > 2147483647 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:int: out of range.',
	      literal=S,
	      type='xsd:int'].
nbasetypeElt(literal(type(_,S)),xsd:long,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:long',
	      literal=S,
	      type='xsd:long'].
nbasetypeElt(literal(type(_,S)),xsd:long,Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -9223372036854775808 ; I > 9223372036854775807 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:long: out of range.',
	      literal=S,
	      type='xsd:long'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedByte,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedByte',
	      literal=S,
	      type='xsd:unsignedByte'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedByte,Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 255 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedByte: out of range.',
	      literal=S,
	      type='xsd:unsignedByte'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedShort,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedShort',
	      literal=S,
	      type='xsd:unsignedShort'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedShort,Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 65535 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedShort: out of range.',
	      literal=S,
	      type='xsd:unsignedShort'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedInt,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedInt',
	      literal=S,
	      type='xsd:unsignedInt'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedInt,Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 4294967295 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedInt: out of range.',
	      literal=S,
	      type='xsd:unsignedInt'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedLong,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedLong',
	      literal=S,
	      type='xsd:unsignedLong'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedLong,Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 18446744073709551615 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedLong: out of range.',
	      literal=S,
	      type='xsd:unsignedLong'].
nbasetypeElt(literal(type(_,S)),xsd:positiveInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:positiveInteger',
	      literal=S,
	      type='xsd:positiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:positiveInteger,Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    I < 1 
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:positiveInteger: out of range.',
	      literal=S,
	      type='xsd:positiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:nonNegativeInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:nonNegativeInteger',
	      literal=S,
	      type='xsd:nonNegativeInteger'].
nbasetypeElt(literal(type(_,S)),xsd:negativeInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:negativeInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:negativeInteger',
	      literal=S,
	      type='xsd:negativeInteger'].
nbasetypeElt(literal(type(_,S)),xsd:nonPositiveInteger,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:nonPositiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:nonPositiveInteger',
	      literal=S,
	      type='xsd:nonPositiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:base64Binary,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:base64Binary,C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:base64Binary',
	      literal=S,
	      type='xsd:base64Binary'].
nbasetypeElt(literal(type(_,S)),xsd:anyURI,Reason) :-
    \+ uri_components(S,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:anyUri',
	      literal=S,
	      type='xsd:anyURI'].
nbasetypeElt(literal(type(_,S)),xsd:language,Reason) :-
    \+ uri_components(xsdParser:language,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:language',
	      literal=S,
	      type='xsd:language'].
nbasetypeElt(literal(type(_,S)),xsd:normalizedString,Reason) :-
    \+ uri_components(xsdParser:normalizedString,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:normalizedString',
	      literal=S,
	      type='xsd:normalizedString'].
nbasetypeElt(literal(type(_,S)),xsd:token,Reason) :-
    \+ uri_components(xsdParser:normalizedString,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:token',
	      literal=S,
	      type='xsd:token'].
nbasetypeElt(literal(type(_,S)),xsd:'NMTOKEN',Reason) :-
    \+ uri_components(xsdParser:nmtoken,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:NMTOKEN',
	      literal=S,
	      type='xsd:NMTOKEN'].
nbasetypeElt(literal(type(_,S)),xsd:'Name',Reason) :-
    \+ uri_components(xsdParser:name,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:Name',
	      literal=S,
	      type='xsd:Name'].
nbasetypeElt(literal(type(_,S)),xsd:'NCName',Reason) :-
    \+ uri_components(xsdParser:ncname,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:NCName',
	      literal=S,
	      type='xsd:NCName'].
nbasetypeElt(literal(type(_,S)),xsd:'NCName',Reason) :-
    \+ uri_components(xsdParser:ncname,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:NCName',
	      literal=S,
	      type='xsd:NCName'].
nbasetypeElt(literal(T),rdf:'PlainLiteral',Reason) :-
    (lang(_,_) \= T ; \+ atom(T))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed rdf:PlainLiteral',
	      literal=T,
	      type='rdf:PlainLiteral'].
nbasetypeElt(X,rdfs:'Literal',Reason) :-
    literal(_) \= X, term_to_atom(X,T)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed rdfs:Literal',
	      literal=T,
	      type='rdfs:Literal'].
