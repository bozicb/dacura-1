:- module(tbox,[class/2, restriction/2, classOrRestriction/2,
		subClassOf/3, unionOf/3, intersectionOf/3, subClassStrict/3,
		subsumes/3, dataProperty/2, objectProperty/2, annotationProperty/2,
		property/2, subPropertyOf/3, subsumptionPropertiesOf/3,
		range/3, 
		% constraints anything greater than /2 is probably wrong
		notUniqueClass/3, notUniqueProperty/3,
		orphanClass/4, classCycle/4,
		notSubPropertyOfProperty/4,
		
		orphanProperty/2, propertyCycle/4,
		noImmediateDomain/2, noImmediateRange/2,
		invalidDomain/2, invalidRange/2,
		domainNotSubsumed/2, rangeNotSubsumed/2,
		schemaSubjectBlankNode/3, noUniqueClassLabel/3
	       ]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils). 
:- use_module(datatype).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Classes 

% All class designations
:- rdf_meta class(r,o).
class(X,Schema) :- rdf(X, rdf:type, rdfs:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Class', Schema).
class(owl:'Thing',_).
class(owl:'Nothing',_).

restriction(R,Schema) :- rdf(R, rdf:type, owl:'Restriction', Schema).

restrictionOnProperty(CR,P,Schema) :- rdf(CR,owl:onProperty,P,Schema), restriction(CR).

classOrRestriction(X,Schema) :- class(X,Schema).
classOrRestriction(X,Schema) :- restriction(X,Schema).

% Uniqueness constraint on classes
notUniqueClass(Y, Schema, Reason) :-
    classOrRestriction(Y, Schema), setof(X, classOrRestriction(X,Schema), L),
    \+ count(Y,L,1),
    interpolate(['The class or restriction ',Y,
		 ' is not a unique. Some existing class has this identifier']
		,Message),
    Reason = [error=notUniqueClass,
	      message=Message,
	      class=Y].

% Collect the RDF list into a prolog list
% It may be better to treat lists programmatically through rdf rather than
% collect them, using a derived predicate like rdfListMembership
:- rdf_meta collect(r,t,o).
collect(rdf:nil,[],_).
collect(X,[H|T],Graph) :-
    rdf(X,rdf:first,H,Graph),
    rdf(X,rdf:rest,Y,Graph),
    collect(Y,T,Graph).

% One step subclassing
:- rdf_meta subClass(r,r,o).
subClassOf(Child,Parent,Schema) :- rdf(Child, rdfs:subClassOf, Parent, Schema).

:- rdf_meta unionOf(r,r,o).
unionOf(CC,CP,Schema) :-
    rdf(CP,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    member(CC,L).

:- rdf_meta intersectionOf(r,r,o).
intersectionOf(CC,CP,Schema) :-
    rdf(CC,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(CP,L).

:- rdf_meta oneOf(r,r,o).
oneOf(X,CC,Schema) :-
    rdf(CC,rdfs:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(X,L).

% transitive strict relation
subClassStrict(X,Y,Schema) :- parentClass(X,Y,Schema). 
subClassStrict(X,Z,Schema) :- rdf(X, rdfs:subClassOf, Y, Schema), subClassOf(Y,Z, Schema).

% Implements class subsumption
:- rdf_meta subsumes(r,r,o).
subsumes(_,owl'Thing',_). % Is this worth throwing in? Might conflict with other constraints
subsumes(owl'Nothing',_,_).
subsumes(CC,CC,_).
subsumes(CC,CP,Schema) :-
    subclassOf(CC,CZ,Schema),
    subsumes(CZ,CP).
subsumes(CC,CP,Schema) :-
    unionOf(CZ,CP,Schema),
    subsumes(CZ,CP).
subsumes(CC,CP,Schema) :-
    intersectionOf(CC,CZ), 
    subsumes(CZ,CP).
subsumes(CC,CP,_) :- % xsd types
    datatypeSubsumes(CC,CP).

orphanClass(X,Y,Schema, Reason) :-
    subClassOf(X,Y,Schema),
    \+ class(Y,Schema),
    interpolate(['The class ',X,' is not a subclass of a valid class ',Y], Message),
    Reason = [error=notSubClassOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
orphanClass(X,Y,Schema, Reason) :-
    intersectionOf(X,Y,Schema),
    \+ class(Y,Schema),
    interpolate(['The class ',X, ' is not an intersection of a valid class ',Y], Message),
    Reason = [error=notSubClassOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
orphanClass(X,Y,Schema, Reason) :-
    unionOf(X,Y,Schema),
    \+ class(Y,Schema),
    interpolate(['The class ',X,' is not a union of a valid class ',Y], Message),
    Reason = [error=notSubClassOfClass,
	      message=Message,
	      child=X,
	      parent=Y].

% subclass cycles
classCycleHelp(C,S,[],_) :- get_assoc(C,S,true), !.
classCycleHelp(C,S,[K|P],Schema) :- class(C,Schema), subClass(K,C,Schema), 
				    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).

classCycle(C,P,Schema,Reason) :-
    empty_assoc(S), classCycleHelp(C,S,P,Schema),
    interpolate(['Class, ',CC,' has a class cycle with path: ', P], Message),
    Reason = [error=classCycle,
	      message=Message,
	      class=CC,
	      path=P].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties

:- rdf_meta dataProperty(r,o).
dataProperty(P,Schema) :-
    rdf(P,rdf:type,owl:'DataProperty',Schema).
dataProperty(rdfs:label,_).
dataProperty(rdfs:comment,_).

annotationProperty(P,Schema) :-
    rdf(P,rdf:type,owl:'AnnotationProperty',Schema).

:- rdf_meta objectProperty(r,o).
objectProperty(P,Schema) :- 
    rdf(P,rdf:type,owl:'objectProperty',Schema).
objectProperty(P,Schema) :- 
    annotationProperty(P,Schema).

functionalProperty(P,Schema) :-
    rdf(P,rdf:type,owl:'FunctionalProperty',Schema).

:- rdf_meta property(r,o).
property(P,Schema) :- dataProperty(P, Schema).
property(P,Schema) :- objectProperty(P,Schema). 

%uniqueProperty(P,Schema) :- property(P,Schema), bagof(P2, property(P2,Schema), L), count(P,L,1).

notUniqueProperty(P,Schema, Reason) :-
    property(P,Schema), bagof(P2, property(P2,Schema), L),
    \+ count(P,L,1),
    interpolate([P,' is not a unique property name, some property with this name already exists'],
		Message),
    Reason=[error=notUniqueProperty,
	    property=P,
	    message=Message].

% One step subproperty relation
:- rdf_meta subPropertyOf(r,r,o).
subPropertyOf(X,Y,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema).

% Transitive reflexive closure of Subproperty relation.
:- rdf_meta subsumptionPropertiesOf(r,r,o). 
subsumptionPropertiesOf(PC,PP,Schema) :-
    subPropertyOf(PC, PZ, Schema),
    subsumptionPropertiesOf(PZ,PP,Schema). 
subsumptionPropertiesOf(PC,PC,Schema).

notSubPropertyOfProperty(X,Y,Schema,Reason) :-
    subPropertyOf(X,Y,Schema),
    \+ property(Y,Schema),
    interpolate([X,' is not a sub-property of ', Y], Message),
    Reason=[error=notSubPropertyOfProperty,
	    child=X,
	    parent=Y,
	    message=Message].

orphanProperty(Schema,Reason) :-
    notSubPropertyOfProperty(_,_,Schema,Reason).

% subProperty cycles 

propertyCycleHelp(P,S,[],_) :- get_assoc(P,S,true), !.
propertyCycleHelp(P,S,[Q|T],Schema) :- property(P,Schema), subProperty(Q,P,Schema), put_assoc(P, S, true, S2), propertyCycleHelp(Q,S2,T,Schema).

propertyCycle(P,PC,Schema,Reason) :-
    empty_assoc(S), propertyCycleHelp(P,S,PC,Schema),
    interpolate(['Property class ', P, ' has a cycle with path: ', PC], Message),
    Reason=[error=propertyClassCycle,
	    property=P,
	    path=PC,
	    message=Message].

:- rdf_meta range(r,r,o).
range(P,R,Schema) :- rdf(P2,rdfs:range, R,Schema). 

noImmediateDomain(Schema,Reason) :-
    property(P,Schema),
    \+ domain(P,_,Schema),
    (dataProperty(P,Schema) -> M='Data property '
     ; annotationProperty(P,Schema) -> M='Annotation property '
     ; objectProperty(P,Schema) -> M='Object property '),
    interpolate([M, P, ' has no specified domain.'], Message),
    Reason = [error=noImmediateDomain,
	      property=P,
	      message = Message].

noImmediateRange(Schema,Reason) :-
    property(P,Schema),
    \+ range(P,_,Schema),
    (dataProperty(P,Schema) -> M='Data property '
     ; annotationProperty(P,Schema) -> M='Annotation property '
     ; objectProperty(P,Schema) -> M='Object property '),
    interpolate([M, P, ' has no specified range.'], Message),
    Reason = [error=noImmediateRange,
	      property=P,
	      message = Message].

invalidDomain(Schema,Reason) :-
    property(P),
    domain(P,D,Schema),
    \+ class(D),
    interpolate(['The property ', P,' has an undefined domain.'],Message),
    Reason=[error=invalidDomain,
	    message=Message,
	    property=P,
	    domain=D].

invalidRange(Schema,Reason) :-
    dataProperty(P),
    range(P,R,Schema),
    \+ baseType(R),
    interpolate(['Data Property Range is not a valid (or implemented) datatype.'], Message),
    Reason=[error=invalidRange,
	    message=Message,
	    property=P,
	    range=R].
invalidRange(Schema,Reason) :-
    objectProperty(P),
    range(P,R,Schema),
    \+ class(R),
    interpolate(['ObjectProperty ', P,'Has an undefined range.'],Message),
    Reason=[error=invalidRange,
	    message=Message,
	    property=P,
	    range=R].

domainNotSubsumed(Schema,Reason) :-
    property(P),
    subsumptionPropertiesOf(P,P2,Schema),
    domain(P,D,Schema), domain(P2,D2,Schema),
    \+ subsumes(D, D2, Schema), 
    interpolate('Invalid domain on property ', P,
		', due to failure of property subsumption.', Message),
    Reason = [error=domainNotSubsumed,
	      message=Message,
	      property=P,
	      parentProperty=P2,
	      range=R,
	      parentRange=R2].

rangeNotSubsumed(Schema,Reason) :-
    property(P),
    subsumptionPropertiesOf(P,P2,Schema),
    range(P,R,Schema), range(P2,R2,Schema),
    \+ subsumes(R, R2, Schema), 
    interpolate('Invalid range on property ', P,
		', due to failure of property subsumption.', Message),
    Reason = [error=rangeNotSubsumed,
	      message=Message,
	      property=P,
	      parentProperty=P2,
	      range=R,
	      parentRange=R2].

schemaSubjectBlankNode(X,Schema) :- rdf(X,_,_,Schema), rdf_is_bnode(X).
schemaPredicateBlankNode(Y,Schema) :- rdf(_,Y,_,Schema), rdf_is_bnode(Y).
schemaObjectBlankNode(Z,Schema) :- rdf(_,_,Z,Schema), rdf_is_bnode(Z).

schemaBlankNode(_,Schema,Reason) :-
    schemaSubjectBlankNode(X,Schema),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    subject=X].
schemaBlankNode(_,Schema,Reason) :-
    schemaPredicateBlankNode(X,Schema),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    predicate=X].
schemaBlankNode(_,Schema,Reason) :-
    schemaObjectBlankNode(X,Schema),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    object=X].

% Labels
label(X,Y,Schema) :- rdf(X, rdfs:label, Y, Schema).

classHasLabel(X,Y,Schema) :- class(X,Schema), label(X,Y,Schema).
%classHasNoLabel(X,Schema) :- class(X,Schema), \+ label(X,_,Schema).

classHasOneLabel(X,Schema) :-
    classHasLabel(X,Label,Schema),
    bagof(label(Y,Label2), classHasLabel(Y,Label2,Schema), L), count(label(X,Label),L,1).

noUniqueClassLabel(X,Schema,Reason) :- 
    \+ classHasOneLabel(X,Label,Schema),
    Reason = [error=duplicateLabelClass, 
	      class=Y, 
	      label=Label2].
