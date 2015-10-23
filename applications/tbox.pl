:- module(tbox,[
	        %%% TBox predicates
	        class/2, restriction/2, classOrRestriction/2,
		subClassOf/3, unionOf/3, intersectionOf/3, subClassStrict/3,
		subsumptionOf/3, strictSubsumptionOf/3, complementOf/3,
		datatypeProperty/2, objectProperty/2, annotationProperty/2,
		property/2, subPropertyOf/3, subsumptionPropertiesOf/3,
		range/3, domain/3, collect/3, functionalProperty/2,
		inverseFunctionalProperty/2, restrictionOnProperty/3,
		datatypeSubsumptionOf/3, basetypeSubsumptionOf/2,
		customDatatype/2,
		
		%%% SC == Schema Constraints
		%%% constraints must be pred/2

		% REQUIRED Best Practice 
		classCycleSC/2,               % Best Practice
		propertyCycleSC/2,            % Best Practice

		% Best practice
		noImmediateDomainSC/2, noImmediateRangeSC/2,      % Best Practice
		schemaBlankNodeSC/2, notUniqueClassLabelSC/2,       % Best Practice
		notUniqueClassSC/2, notUniquePropertySC/2,        % Best Practice
		noImmediateClassSC/2,
		
		% OWL DL (Constraint)
		orphanClassSC/2,              % OWL
		orphanPropertySC/2,           % OWL
		invalidDomainSC/2, invalidRangeSC/2,         % OWL
		domainNotSubsumedSC/2, rangeNotSubsumedSC/2  % OWL
	       ]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils). 
:- use_module(datatypes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OWL DL Syntactic correctness
%%
%% It would be useful to do a complete check on the syntactic
%% correctness of our ontology according to the OWL 2 / RDF mapping

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Classes 

% Check to see if class definitions are immediate (best practices).
:- rdf_meta immediateClass(r,o).
immediateClass(X,Schema) :- xrdf(X, rdf:type, rdfs:'Class', Schema).
immediateClass(X,Schema) :- xrdf(X, rdf:type, owl:'Class', Schema).
immediateClass(owl:'Thing',_).
immediateClass(owl:'Nothing',_).

% All class designations - with inferences.
:- rdf_meta class(r,o).
class(X,Schema) :- immediateClass(X,Schema). 
class(X,Schema) :- subClassOf(X,Y,Schema), class(Y,Schema).
class(X,Schema) :- equivalentClass(X,Y,Schema), class(Y,Schema).

:- rdf_meta restriction(r,o).
restriction(R,Schema) :- xrdf(R, rdf:type, owl:'Restriction', Schema).
restriction(R,Schema) :- subClassOf(R,R2,Schema), restriction(R2,Schema).
restriction(R,Schema) :- equivalentClass(R,R2,Schema), restriction(R2,Schema).

% A class is used without a class definition.
noImmediateClassSC(Schema, Reason) :-
    xrdf(P,rdf:type,owl:'ObjectProperty',Schema),
    domain(P,X,Schema),
    \+ immediateClass(X,Schema), \+ restriction(X,Schema),
    interpolate([X,' is used as a domain for property ',P,' but is not defined'], Message),
    Reason = [error=notDomainClass,
	      message=Message,
	      class=X,
	      property=P].
noImmediateClassSC(Schema, Reason) :-
    xrdf(P,rdf:type,owl:'ObjectProperty',Schema),
    range(P,X,Schema),
    \+ immediateClass(X,Schema), \+ restriction(X,Schema),
    interpolate([X,' is used as a domain for property ',P,' but is not defined'], Message),
    Reason = [error=notDomainClass,
	      message=Message,
	      class=X,
	      property=P].
noImmediateClassSC(Schema, Reason) :-
    subClassOf(X,Y,Schema),
    \+ customDatatype(X,Schema), \+ immediateClass(X,Schema), \+ restriction(X,Schema),
    interpolate(['The class ',Y,' is not a superclass of a defined class ',X], Message),
    Reason = [error=notSuperClassOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Schema, Reason) :-
    subClassOf(X,Y,Schema), 
    \+ customDatatype(Y,Schema), \+ immediateClass(Y,Schema), \+ restriction(Y,Schema),
    interpolate(['The class ',X,' is not a subclass of a defined class ',Y], Message),
    Reason = [error=notSubClassOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Schema, Reason) :-
    intersectionOf(X,Y,Schema),
    \+ immediateClass(X,Schema), \+ restriction(X,Schema),
    interpolate(['The class ',X,' is an intersection of ', Y,' but not a defined class'], Message),
    Reason = [error=notIntersectionOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Schema, Reason) :-
    intersectionOf(X,Y,Schema),
    \+ immediateClass(Y,Schema), \+ restriction(Y,Schema),
    interpolate(['The class ',X,' is not an intersection of a defined class ',Y], Message),
    Reason = [error=notIntersectionOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Schema, Reason) :-
    unionOf(X,Y,Schema),
    \+ immediateClass(Y,Schema), \+ restriction(Y,Schema),
    interpolate(['The class ',X,' is not a union of a defined class ',Y], Message),
    Reason = [error=notUnionOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
noImmediateClassSC(Schema, Reason) :-
    unionOf(X,Y,Schema),
    \+ immediateClass(X,Schema), \+ restriction(X,Schema),
    interpolate(['The class ',X,' is a union of ', Y,' but not a defined class'], Message),
    Reason = [error=notUnionOfClass,
	      message=Message,
	      child=X,
	      parent=Y].

restrictionOnProperty(CR,P,Schema) :- xrdf(CR,owl:onProperty,P,Schema), restriction(CR,Schema).

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

notUniqueClassSC(Schema,Reason) :- notUniqueClass(_,Schema,Reason).

% Collect the RDF list into a prolog list
% It may be better to treat lists programmatically through rdf rather than
% collect them, using a derived predicate like rdfListMembership
:- rdf_meta collect(r,t,o).
collect(rdf:nil,[],_).
collect(X,[H|T],Graph) :-
    xrdf(X,rdf:first,H,Graph),
    xrdf(X,rdf:rest,Y,Graph),
    collect(Y,T,Graph).

% One step subclassing
:- rdf_meta subClassOf(r,r,o).
subClassOf(Child,Parent,Schema) :- xrdf(Child, rdfs:subClassOf, Parent, Schema).

:- rdf_meta unionOf(r,r,o).
unionOf(C,U,Schema) :-
    xrdf(C,owl:unionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    member(U,L).

:- rdf_meta disjointUnionOf(r,r,o).
disjointUnionOf(C,U,Schema) :-
    xrdf(C,owl:disjointUnionOf,ListObj, Schema),
    collect(ListObj,L,Schema),
    member(U,L).

:- rdf_meta intersectionOf(r,r,o).
intersectionOf(C,I,Schema) :-
    xrdf(C,owl:intersectionOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(I,L).

:- rdf_meta oneOf(r,r,o).
oneOf(CC,X,Schema) :-
    xrdf(CC,owl:oneOf,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(X,L).

:- rdf_meta complementOf(r,r,o).
complementOf(CC,CN,Schema) :-
    xrdf(CC,owl:complementOf,CN,Schema).

:- rdf_meta equivalentClass(r,r,o).
equivalentClass(CC,CE,Schema) :-
    xrdf(CC,owl:equivalentClass,ListObj,Schema),
    collect(ListObj,L,Schema),
    member(CE,L).

:- rdf_meta equivalentClass(r,r,o).
anonymousEquivalentClass(C,CE,Schema) :-
    equivalentClass(C,CE,Schema),
    % Exactly one reference to this class, or everything will go to hell.
    (setof(X,xrdf(X,_,CE,Schema), ListX) *-> ListX = L ; L = []),
    length(L,1).

% transitive strict relation
subClassStrict(X,Y,Schema) :- subClassOf(X,Y,Schema). 
subClassStrict(X,Z,Schema) :- subClassOf(X,Y,Schema), subClassStrict(Y,Z, Schema).

% Implements class subsumption
% - complementOf classes do not give subsumption properly yet (unimplemented).
%   Requires anti-subsumption predicate
% - oneOf should probably have individual sets for both CC, CP
:- rdf_meta subsumptionOf(r,r,o).
subsumptionOf(_,owl:'Thing',_). % Is this worth throwing in? Might conflict with other constraints
subsumptionOf(owl:'Nothing',_,_).
subsumptionOf(CC,CC,Schema) :-
    class(CC,Schema).
subsumptionOf(CC,CP,Schema) :-
    subClassOf(CC,CZ,Schema),
    subsumptionOf(CZ,CP,Schema).
subsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),
    unionOf(CZ,CC,Schema),
    subsumptionOf(CZ,CP,Schema).
subsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),	
    disjointUnionOf(CZ,CC,Schema),
    subsumptionOf(CZ,CP,Schema).
subsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),	
    intersectionOf(CC,CZ,Schema), 
    subsumptionOf(CZ,CP,Schema).
subsumptionOf(CC,CP,Schema) :-
    anonymousEquivalentClass(CC,CZ,Schema),
    subsumptionOf(CZ,CP,Schema).
subsumptionOf(CC,CP,Schema) :- % datatypes
    datatype(CC,Schema),
    datatypeSubsumptionOf(CC,CP,Schema).

:- rdf_meta customDatatype(r,o).
customDatatype(X,Schema) :-
    xrdf(X, rdf:type, rdfs:'Datatype',Schema).

:- rdf_meta datatype(r,o).
datatype(X,Schema) :- customDatatype(X,Schema).
datatype(X,_) :- baseType(X).

% implements strict class subsumption (CC < CP) [Needs fully instantiated arguments]
:- rdf_meta strictSubsumptionOf(r,r,o).
strictSubsumptionOf(CC,owl:'Thing',_) :- CC \= owl:'Thing'.
strictSubsumptionOf(owl:'Nothing',CP,_) :- CP \= owl:'Nothing'.
strictSubsumptionOf(CC,CP,Schema) :-
    subClassOf(CC,CP,Schema).
strictSubsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),
    unionOf(CP,CC,Schema).
strictSubsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),
    intersectionOf(CC,CP,Schema).
strictSubsumptionOf(CC,CP,Schema) :-
    subClassOf(CC,CZ,Schema),
    strictSubsumptionOf(CZ,CP,Schema).
strictSubsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),
    unionOf(CZ,CC,Schema),
    strictSubsumptionOf(CZ,CP,Schema).
strictSubsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),
    disjointUnionOf(CZ,CC,Schema),
    strictSubsumptionOf(CZ,CP,Schema).
strictSubsumptionOf(CC,CP,Schema) :-
    class(CC,Schema),
    intersectionOf(CC,CZ,Schema), 
    strictSubsumptionOf(CZ,CP,Schema).
strictSubsumptionOf(CC,CP,Schema) :- % xsd and custom data types
    datatypeStrictSubsumptionOf(CC,CP,Schema).

:- rdf_meta basetypeSubsumptionOf(r,r).
basetypeSubsumptionOf(T,T) :- baseType(T).
basetypeSubsumptionOf(Sub,Super) :-
    baseTypeParent(Sub,Parent), basetypeSubsumptionOf(Parent,Super).

:- rdf_meta datatypeSubsumptionOf(r,r).
datatypeSubsumptionOf(T,T,Schema) :- datatype(T,Schema).
datatypeSubsumptionOf(Sub,Super,Schema) :-
    customDatatype(Sub,Schema),
    unionOf(Super,Sub,Schema).
datatypeSubsumptionOf(Sub,Super,Schema) :-
    customDatatype(Sub,Schema),
    intersectionOf(Sub,Super,Schema).
datatypeSubsumptionOf(Sub,Super,Schema) :-
    % This only works because of the strict hierarchy (no derived union / intersections)
    customDatatype(Sub,Schema),
    complementOf(Sub,CN,Schema),
    \+ datatypeSubsumptionOf(CN,Super,Schema).
datatypeSubsumptionOf(Sub,Super,Schema) :-
    baseTypeParent(Sub,Parent), datatypeSubsumptionOf(Parent,Super,Schema).

:- rdf_meta datatypeStrictSubsumptionOf(r,r).
datatypeStrictSubsumptionOf(Sub,Super,_) :-
    baseTypeParent(Sub,Super).
datatypeStrictSubsumptionOf(Sub,Super,Schema) :-
    % DDD probably need one more clause for each owl custom build property
    baseTypeParent(Sub,Parent),
    datatypeSubsumptionOf(Parent,Super,Schema).

% Defining orphaned classes.
orphanClassSC(Schema, Reason) :-
    subClassOf(X,Y,Schema),
    \+ class(Y,Schema), \+ restriction(Y,Schema),
    interpolate(['The class ',X,' is not a subclass of a valid class ',Y], Message),
    Reason = [error=notSubClassOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
orphanClassSC(Schema, Reason) :-
    intersectionOf(X,Y,Schema),
    \+ class(Y,Schema), \+ restriction(Y,Schema),
    interpolate(['The class ',X, ' is not an intersection of a valid class ',Y], Message),
    Reason = [error=notIntersectionOfClass,
	      message=Message,
	      child=X,
	      parent=Y].
orphanClassSC(Schema, Reason) :-
    unionOf(X,Y,Schema),
    \+ class(Y,Schema), \+ restriction(Y,Schema),
    interpolate(['The class ',X,' is not a union of a valid class ',Y], Message),
    Reason = [error=notUnionOfClass,
	      message=Message,
	      child=X,
	      parent=Y].


% Cycles in subsumption diagram
classCycleHelp(C,S,[],_) :- get_assoc(C,S,true), !.
classCycleHelp(C,S,[K|P],Schema) :-
    subClassOf(K,C,Schema), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).
classCycleHelp(C,S,[K|P],Schema) :-
    unionOf(C,K,Schema), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).
classCycleHelp(C,S,[K|P],Schema) :-
    disjointUnionOf(C,K,Schema), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).
classCycleHelp(C,S,[K|P],Schema) :-
    intersectionOf(K,C,Schema), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).
classCycleHelp(C,S,[K|P],Schema) :-
    equivalentClass(K,C,Schema),
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).
classCycleHelp(C,S,[K|P],Schema) :-
    complementOf(K,C,Schema), 
    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).

classCycle(C,P,Schema,Reason) :-
    empty_assoc(S), classCycleHelp(C,S,P,Schema),
    interpolate(['Class, ',C,' has a class cycle with path: ', P], Message),
    Reason = [error=classCycle,
	      message=Message,
	      class=C,
	      path=P].

classCycleSC(Schema,Reason) :- classCycle(_,_,Schema,Reason), !. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties

:- rdf_meta rdfsProperty(r).
rdfsProperty(rdfs:label).
rdfsProperty(rdfs:comment).
rdfsProperty(rdfs:seeAlso).

:- rdf_meta rdfProperty(r,o).
rdfProperty(P,Schema) :- 
    xrdf(P,rdf:type,rdf:'Property',Schema).
rdfProperty(P,_) :- rdfsProperty(P).
	   
:- rdf_meta datatypeProperty(r,o).
datatypeProperty(P,Schema) :-
    xrdf(P,rdf:type,owl:'DatatypeProperty',Schema).
datatypeProperty(P,Schema) :- rdfProperty(P,Schema).

:- rdf_meta annotationProperty(r,o).
annotationProperty(P,Schema) :-
    xrdf(P,rdf:type,owl:'AnnotationProperty',Schema).

:- rdf_meta functionalProperty(r,o).
functionalProperty(P,Schema) :-
    xrdf(P,rdf:type,owl:'FunctionalProperty',Schema).

:- rdf_meta inverseFunctionalProperty(r,o).
inverseFunctionalProperty(P,Schema) :-
    xrdf(P,rdf:type,owl:'InverseFunctionalProperty',Schema).

:- rdf_meta objectProperty(r,o).
objectProperty(P,Schema) :- 
    xrdf(P,rdf:type,owl:'ObjectProperty',Schema).
objectProperty(P,Schema) :- rdfProperty(P,Schema).

:- rdf_meta property(r,o).
property(P,Schema) :-
    % Don't predicate over annotations (even if they are otherwise declared as properties.
    annotationProperty(P,Schema) *-> fail
    ; (datatypeProperty(P, Schema)
       ; objectProperty(P,Schema)
       ; rdfProperty(P,Schema)).

%uniqueProperty(P,Schema) :- property(P,Schema), bagof(P2, property(P2,Schema), L), count(P,L,1).

notUniqueProperty(P,Schema,Reason) :-
    property(P,Schema), bagof(P2, property(P2,Schema), L),
    \+ count(P,L,1),
    interpolate([P,' is not a unique property name, some property with this name already exists'],
		Message),
    Reason=[error=notUniqueProperty,
	    property=P,
	    message=Message].

propertyTypeOverloadSC(P,Schema,Reason) :- 
    datatypeProperty(P,Schema), objectProperty(P,Schema),
    interpolate([P,' is an objectProperty and a datatypeProperty'], Message),
    Reason=[error=propertyTypeOverload,
	    property=P,
	    message=Message].

annotationOverloadSC(P,Schema,Reason) :-
    (datatypeProperty(P,Schema) ; objectProperty(P,Schema) ; rdfProperty(P,Schema)),
    annotationProperty(P,Schema),
    interpolate([P,' is defined as a property and defined as an annotationProperty'], Message),
    Reason=[error=annotationOverloadSC,
	    property=P,
	    message=Message].
    
notUniquePropertySC(Schema,Reason) :-
    notUniqueProperty(_,Schema, Reason).

% One step subproperty relation
:- rdf_meta subPropertyOf(r,r,o).
subPropertyOf(X,Y,Schema) :- xrdf(X,rdfs:subPropertyOf,Y,Schema).

% Transitive reflexive closure of Subproperty relation.
:- rdf_meta subsumptionPropertiesOf(r,r,o). 
subsumptionPropertiesOf(PC,PP,Schema) :-
    subPropertyOf(PC, PZ, Schema),
    subsumptionPropertiesOf(PZ,PP,Schema). 
subsumptionPropertiesOf(PC,PC,_).

strictSubsumptionPropertiesOf(PC,PP,Schema) :-
    subPropertyOf(PC, PP, Schema).
strictSubsumptionPropertiesOf(PC,PP,Schema) :-
    subPropertyOf(PC, PZ, Schema),
    subsumptionPropertiesOf(PZ,PP,Schema).


orphanProperty(X,Y,Schema,Reason) :-
    subPropertyOf(X,Y,Schema),
    \+ property(Y,Schema), \+ annotationProperty(Y,Schema),
    interpolate([X,' is not a sub-property of a valid property ', Y], Message),
    Reason=[error=notSubPropertyOfProperty,
	    child=X,
	    parent=Y,
	    message=Message].

orphanPropertySC(Schema,Reason) :-
    orphanProperty(_,_,Schema,Reason).

% subProperty cycles 

propertyCycleHelp(P,S,[],_) :- get_assoc(P,S,true), !.
propertyCycleHelp(P,S,[Q|T],Schema) :-
    property(P,Schema), subPropertyOf(Q,P,Schema), put_assoc(P, S, true, S2),
    propertyCycleHelp(Q,S2,T,Schema).

propertyCycle(P,PC,Schema,Reason) :-
    empty_assoc(S), propertyCycleHelp(P,S,PC,Schema),
    interpolate(['Property class ', P, ' has a cycle with path: ', PC], Message),
    Reason=[error=propertyClassCycle,
	    property=P,
	    path=PC,
	    message=Message].

propertyCycleSC(Schema,Reason) :- propertyCycle(_,_,Schema,Reason).

:- rdf_meta range(r,r,o).
range(P,R,Schema) :- xrdf(P,rdfs:range,R,Schema).

:- rdf_meta domain(r,r,o).
domain(P,D,Schema) :- xrdf(P,rdfs:domain,D,Schema).

noImmediateDomainSC(Schema,Reason) :-
    property(P,Schema), \+ rdfsProperty(P),
    \+ domain(P,_,Schema),
    (datatypeProperty(P,Schema) -> M='Data property '
     ; annotationProperty(P,Schema) -> M='Annotation property '
     ; objectProperty(P,Schema) -> M='Object property '
     ; rdfProperty(P,Schema) -> M='Rdf Property'),
    interpolate([M, P, ' has no specified domain.'], Message),
    Reason = [error=noImmediateDomain,
	      property=P,
	      message = Message].

noImmediateRangeSC(Schema,Reason) :-
    property(P,Schema), \+ rdfsProperty(P),
    \+ range(P,_,Schema),
    (datatypeProperty(P,Schema) -> M='Data property '
     ; annotationProperty(P,Schema) -> M='Annotation property '
     ; objectProperty(P,Schema) -> M='Object property '
     ; rdfProperty(P,Schema) -> M='Rdf Property'),
    interpolate([M, P, ' has no specified range.'], Message),
    Reason = [error=noImmediateRange,
	      property=P,
	      message = Message].

invalidDomainSC(Schema,Reason) :-
    property(P,Schema),
    domain(P,D,Schema),
    \+ class(D,Schema),
    interpolate(['The property ', P,' has an undefined domain.'],Message),
    Reason=[error=invalidDomain,
	    message=Message,
	    property=P,
	    domain=D].

invalidRangeSC(Schema,Reason) :-
    datatypeProperty(P,Schema),
    range(P,R,Schema),
    \+ datatype(R,Schema), \+ rdfProperty(P,Schema),
    interpolate(['DataProperty Range ', R, ' is not a valid (or implemented) datatype for property ', P,'.'], Message),
    Reason=[error=invalidRange,
	    message=Message,
	    property=P,
	    range=R].
invalidRangeSC(Schema,Reason) :-
    objectProperty(P,Schema),
    range(P,R,Schema),
    \+ class(R,Schema), \+ rdfProperty(P,Schema),
    interpolate(['ObjectProperty Range ',R,' is not a valid range for property ',P,'.'],Message),
    Reason=[error=invalidRange,
	    message=Message,
	    property=P,
	    range=R].
invalidRangeSC(Schema,Reason) :-
    rdfProperty(P,Schema),
    range(P,R,Schema),
    \+ class(R,Schema), \+ baseType(R),
    interpolate(['rdf:Property range ',R,' is not a valid range for property ',P,'.'],Message),
    Reason=[error=invalidRange,
	    message=Message,
	    property=P,
	    range=R].

% Logging / Turn off for production
:- use_module(library(http/http_log)).

domainNotSubsumedSC(Schema,Reason) :-
    property(P,Schema),
    strictSubsumptionPropertiesOf(P,P2,Schema),
    domain(P,D,Schema), domain(P2,D2,Schema), % DDD too many solutions
    %http_log_stream(Log),
    %current_output(Log),
    %findall(X, subsumptionOf(D,X,Schema), L),
    %nl(Log), nl(Log), write(Log, 'Subsumptions: '), write_canonical(Log, L), nl(Log), nl(Log),
    %nl(Log), nl(Log), write(Log, 'Listing: '), write_canonical(Log, L), nl(Log), nl(Log),    
    \+ subsumptionOf(D, D2, Schema),
    interpolate(['Invalid domain on property ', P,
		 ', due to failure of domain subsumption.'], Message),
    Reason = [error=domainNotSubsumed,
	      message=Message,
	      property=P,
	      parentProperty=P2,
	      domain=D,
	      parentDomain=D2].

rangeNotSubsumedSC(Schema,Reason) :-
    property(P,Schema),
    strictSubsumptionPropertiesOf(P,P2,Schema),
    range(P,R,Schema), range(P2,R2,Schema), % DDD too many solutions
    \+ subsumptionOf(R, R2, Schema), 
    interpolate(['Invalid range on property ', P,
		 ', due to failure of range subsumption.'], Message),
    Reason = [error=rangeNotSubsumed,
	      message=Message,
	      property=P,
	      parentProperty=P2,
	      range=R,
	      parentRange=R2].

schemaSubjectBlankNode(X,Schema) :- xrdf(X,_,_,Schema), rdf_is_bnode(X).
schemaPredicateBlankNode(Y,Schema) :- xrdf(_,Y,_,Schema), rdf_is_bnode(Y).
schemaObjectBlankNode(Z,Schema) :- xrdf(_,_,Z,Schema), rdf_is_bnode(Z).

schemaBlankNodeSC(Schema,Reason) :-
    schemaSubjectBlankNode(X,Schema),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    subject=X].
schemaBlankNodeSC(Schema,Reason) :-
    schemaPredicateBlankNode(X,Schema),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    predicate=X].
schemaBlankNodeSC(Schema,Reason) :-
    schemaObjectBlankNode(X,Schema),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    object=X].

% Labels
label(X,Y,Schema) :- xrdf(X, rdfs:label, Y, Schema).

classHasLabel(X,Y,Schema) :- class(X,Schema), label(X,Y,Schema).
%classHasNoLabel(X,Schema) :- class(X,Schema), \+ label(X,_,Schema).

classHasOneLabel(X,Schema) :-
    classHasLabel(X,Label,Schema),
    bagof(label(Y,Label2), classHasLabel(Y,Label2,Schema), L), count(label(X,Label),L,1).

notUniqueClassLabel(X,Schema,Reason) :- 
    \+ classHasOneLabel(X,Schema),
    interpolate(['Class ', X,' does not have exactly one lable.'], Message),
    Reason = [error=duplicateLabelClass, 
	      class=X,
	      message=Message].

notUniqueClassLabelSC(Schema,Reason) :- notUniqueClassLabel(_,Schema,Reason).
