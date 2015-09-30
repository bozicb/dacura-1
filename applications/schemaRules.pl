:- module(schemaRules,[testSchema/1,
		       runSchemaUpdate/3,
		       runInstanceUpdate/3,
		       runFullValidation/2,
		       runSchemaValidation/2]).

:- use_module(library(semweb/rdf_db), except([rdf/4, rdf_retractall/4])).
:- use_module(transactionGraph).
:- use_module(library(semweb/turtle)). 
:- use_module(utils). 

%% setup namespaces related to rdf, rdfs and owl 
%%%
%% ALL of these are predefined!!!! 
%:- rdf_register_ns(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
%:- rdf_register_ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
%:- rdf_register_ns(owl, 'http://www.w3.org/2002/07/owl#').

%%%%%%%%%%%%%%%%%%%%
% Schema constraints

%%%%%%%%%%%%%%%%%%%%
% classes
:- rdf_meta class(r,o).
class(X,Schema) :- rdf(X, rdf:type, rdfs:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Restriction', Schema).
%%%% class(rdf:'Alt',_). disgusting hack, removed DDD
%%%% class(rdf:'Bag',_).
% Skos concept? for class as well?

% DDD not negation has no value?
uniqueClass(Y,Schema) :- class(Y, Schema), setof(X, class(X, Schema), L), count(Y,L,1).

notUniqueClass(Y, Schema, Reason) :-
    class(Y, Schema), setof(X, class(X,Schema), L),
    \+ count(Y,L,1),
    interpolate(['The class ',Y,' is not a unique. Some existing class has this identifier']
		,Message),
    Reason = [error=notUniqueClass,
	      message=Message,
	      class=Y].

duplicateClasses(L, Schema) :- setof(JSON, notUniqueClassJSON(JSON,_,Schema), L).

% subclasses 

:- rdf_meta subClass(r,r,o).
subClass(_,owl:'Thing', _).
subClass(X,Y, Schema) :- rdf(X, rdfs:subClassOf, Y, Schema).

subClassOf(X,Y,Schema) :- rdf(X, rdfs:subClassOf, Y, Schema).
subClassOf(X,Z,Schema) :- rdf(X, rdfs:subClassOf, Y, Schema), subClassOf(Y,Z, Schema).

% DDD Not negation has no value?
subClassOfClass(X,Y,Schema) :- subClassOf(X,Y,Schema), class(Y,Schema).

orphanSubclass(X,Y,Schema, Reason) :-
    subClassOf(X,Y,Schema),
    \+ class(Y,Schema),
    interpolate(['The class ',X,'is not a subclass of a valid class ',Y], Message),
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
    interpolate(['Class, ',C,' has a class cycle with path: ', P], Message),
    Reason = [error=classCycle,
	      message=Message,
	      class=C,
	      path=P].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties.

:- rdf_meta property(r,o).
property(rdfs:label,_).
property(rdfs:comment,_).
property(P,Schema) :- rdf(P,rdf:type,owl:'ObjectProperty',Schema).
property(P,Schema) :- rdf(P,rdf:type,owl:'DataProperty',Schema).
property(P,Schema) :- rdf(P,rdf:type,owl:'AnnotationProperty',Schema).

uniqueProperty(P,Schema) :- property(P,Schema), bagof(P2, property(P2,Schema), L), count(P,L,1).

notUniqueProperty(P,Schema, Reason) :-
    property(P,Schema), bagof(P2, property(P2,Schema), L),
    \+ count(P,L,1),
    interpolate([P,' is not a unique property name, some property with this name already exists'],
		Message),
    Reason=[error=notUniqueProperty,
	    property=P,
	    message=Message].

% subProperties.

subProperty(X,Y,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema).

subPropertyOf(X,Y,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema).
subPropertyOf(X,Z,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema), subPropertyOf(Y,Z,Schema). 

subsumptionPropertiesOf(PC,PP,Schema) :- subPropertyOf(PC, PP, Schema).
subsumptionPropertiesOf(PC,PC,Schema) :- property(PC,Schema).

subPropertyOfProperty(X,Y,Schema) :- subPropertyOf(X,Y,Schema), property(Y,Schema).

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

%%%% orphanSubProperties(L,Schema) :- setof(JSON, notSubPropertyOfPropertyJSON(JSON,_,_,Schema),L).

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


%%%% Core types	
%% xsd:string	Character strings (but not all Unicode character strings)
%% xsd:boolean	true, false
%% xsd:decimal	Arbitrary-precision decimal numbers
%% xsd:integer	Arbitrary-size integer numbers
%% IEEE floating-point numbers	
%% xsd:double	64-bit floating point numbers incl. ±Inf, ±0, NaN
%% xsd:float	32-bit floating point numbers incl. ±Inf, ±0, NaN
%% Time and date	xsd:date	Dates (yyyy-mm-dd) with or without timezone
%% xsd:time	Times (hh:mm:ss.sss…) with or without timezone
%% xsd:dateTime	Date and time with or without timezone
%% xsd:dateTimeStamp	Date and time with required timezone
%% Recurring and partial dates	
%% xsd:gYear	Gregorian calendar year
%% xsd:gMonth	Gregorian calendar month
%% xsd:gDay	Gregorian calendar day of the month
%% xsd:gYearMonth	Gregorian calendar year and month
%% xsd:gMonthDay	Gregorian calendar month and day
%% xsd:duration	Duration of time
%% xsd:yearMonthDuration	Duration of time (months and years only)
%% xsd:dayTimeDuration	Duration of time (days, hours, minutes, seconds only)
%% Limited-range integer numbers	
%% xsd:byte	-128…+127 (8 bit)
%% xsd:short	-32768…+32767 (16 bit)
%% xsd:int	-2147483648…+2147483647 (32 bit)
%% xsd:long	-9223372036854775808…+9223372036854775807 (64 bit)
%% xsd:unsignedByte	0…255 (8 bit)
%% xsd:unsignedShort	0…65535 (16 bit)
%% xsd:unsignedInt	0…4294967295 (32 bit)
%% xsd:unsignedLong	0…18446744073709551615 (64 bit)
%% xsd:positiveInteger	Integer numbers >0
%% xsd:nonNegativeInteger	Integer numbers ≥0
%% xsd:negativeInteger	Integer numbers <0
%% xsd:nonPositiveInteger	Integer numbers ≤0
%% Encoded binary data	xsd:hexBinary	Hex-encoded binary data
%% xsd:base64Binary	Base64-encoded binary data
%% Miscellaneous
%% XSD types	
% xsd:anyURI	Absolute or relative URIs and IRIs
%% xsd:language	Language tags per [BCP47]
%% xsd:normalizedString	Whitespace-normalized strings
%% xsd:token	Tokenized strings
%% xsd:NMTOKEN	XML NMTOKENs
%% xsd:Name	XML Names
%% xsd:NCName	XML NCNames

:- rdf_meta baseType(r).
baseType(xsd:string). 
baseType(xsd:boolean). 
baseType(xsd:decimal). 
baseType(xsd:integer). 
baseType(xsd:double). 
baseType(xsd:float). 
baseType(xsd:time).
baseType(xsd:dateTime). 
baseType(xsd:dateTimeStamp).
baseType(xsd:gYear). 
baseType(xsd:gMonth). 
baseType(xsd:gDay). 
baseType(xsd:gYearMonth). 	
baseType(xsd:gMonthDay). 
baseType(xsd:duration). 
baseType(xsd:yearMonthDuration). 
baseType(xsd:dayTimeDuration). 
baseType(xsd:byte). 
baseType(xsd:short). 
baseType(xsd:integer). 
baseType(xsd:long). 
baseType(xsd:unsignedByte). 
baseType(xsd:unsignedInt). 
baseType(xsd:unsignedLong). 
baseType(xsd:positiveInteger). 
baseType(xsd:nonNegativeInteger). 
baseType(xsd:negativeInteger). 
baseType(xsd:nonPositiveInteger). 
baseType(xsd:base64Binary). 
baseType(xsd:anyURI). 
baseType(xsd:language). 
baseType(xsd:normalizedString). 
baseType(xsd:token). 
baseType(xsd:'NMTOKEN'). 
baseType(xsd:'Name'). 
baseType(xsd:'NCName'). 
baseType(rdf:'PlainLiteral').
baseType(rdf:'Literal').

:- rdf_meta type(r,o).
type(X,_) :- baseType(X), !.
type(owl:'Thing',_).
type(X,Schema) :- class(X,Schema). 

% range / domain

:- rdf_meta range(r,r,t,o).
range(P,R,Schema) :- rdf(P2, rdfs:range, R, Schema), subsumptionPropertiesOf(P,P2,Schema).

:- rdf_meta domain(r,r,t,o).
domain(P,D,Schema) :- rdf(P2, rdfs:domain, D, Schema), subsumptionPropertiesOf(P,P2,Schema).

validRange(P,R,Schema) :- range(P,R,Schema), type(R,Schema).
validDomain(P,D,Schema) :- domain(P,D,Schema), type(D,Schema).

notValidRange(P,R,Schema) :- range(P,R,Schema), \+ validRange(P,R,Schema).
notValidDomain(P,D,Schema) :- domain(P,D,Schema), \+ validDomain(P,D,Schema). 

%%%% DDD no longer used
uniqueValidRange(P,R,Schema) :- range(P,R,Schema), findall(R2, validRange(P,R2,Schema), L), length(L,1).

uniqueValidDomain(P,D,Schema) :- domain(P,D,Schema), findall(D2, validRange(P,D2,Schema), L), length(L,1).

notUniqueValidRange(P,R,Schema) :- range(P,R,Schema), findall(R2, validRange(P,R2,Schema), L), \+ length(L,1).

notUniqueValidDomain(P,D,Schema) :- domain(P,D,Schema), findall(D2, validDomain(P,D2,Schema), L), \+ length(L,1).

% does this do too much?
invalidRange(Schema,Reason) :-
    notValidRange(Y,R,Schema),
    interpolate(['The class ',Y,' does not constitute a valid range'],Message),
    Reason=[error=notValidRange,
	    property=Y,
	    message=Message,
	    range=R].
			 
invalidDomain(Schema,Reason) :-
    notvalidDomain(Y,D,Schema),
    interpolate(['The class ',Y,' does not constitute a valid range'],Message),
    Reason=[error=notValidDomain,
	    property=Y,
	    message=Message,
	    domain=D].

%%%%%%%%%%%%%%%%%%%%%%%
%% Instance constraints

instanceClass(X, Y, Instance) :- rdf(X, rdf:type, Y, Instance).
	      
instance(X,Instance) :- instanceClass(X,_,Instance).

instanceHasClass(X,C,Instance,Schema) :- instanceClass(X,C,Instance), class(C,Schema).

orphanInstance(X,C,Instance,Schema) :- instanceClass(X,C,Instance), \+ class(C,Schema).

noOrphans(Instance,Schema) :- \+ orphanInstance(_,_,Instance,Schema).

localOrphanInstance(X,Instance,Schema,Reason) :-
    orphanInstance(X,C,Instance,Schema),
    interpolate(['No valid class corresponding to instance: ',X], Message),
    Reason=[error=localOrphanInstance,
	    message=Message,
	    class=C,
	    instance=X].

instanceProperty(X,P,Instance) :-
    instance(X,Instance),
    rdf(X, P, _,Instance),
    \+ P='http://www.w3.org/1999/02/22-rdf-syntax-ns#type'. 

instanceHasPropertyClass(X,P,Instance,Schema) :-
    instanceProperty(X,P,Instance),
    property(P,Schema).

orphanPropertyClass(X,P,Instance,Schema) :- instanceProperty(X,P,Instance), \+ property(P,Schema).

localOrphanProperty(X,Instance,Schema,Reason) :-
    orphanPropertyClass(X,P,Instance,Schema),
    interpolate(['No class associated with instance ',X,' and property: ',P],Message),
    Reason=[error=noInstancePropertyClass,
	    instance=X,
	    property=P,
	    message=Message].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Instance Type Checking constraints 

% ranges have more possible targets as they can be literals. 
:- rdf_meta typeCheckRange(r,r,o,o).
% TODO: Needs checking of the form of constant
typeCheckRange(T,literal(type(T,_)),_,_) :- baseType(T), !.
typeCheckRange(_,literal(type(_,_)),_,_) :- !, false.
typeCheckRange(xsd:string,literal(S),_,_) :- atom(S).
typeCheckRange(xsd:string,literal(lang(S,_)),_,_) :- atom(S), !.
typeCheckRange(xsd:string,_,_,_) :- !, false.
typeCheckRange(rdfs:'PlainLiteral',literal(S),_,_) :- atom(S).
typeCheckRange(rdfs:'PlainLiteral',literal(lang(S,_)),_,_) :- atom(S), !.
typeCheckRange(rdfs:'PlainLiteral',_,_,_) :- !, false.
typeCheckRange(rdfs:'Literal',literal(S),_,_) :- atom(S). % is this valid?
typeCheckRange(rdfs:'Literal',literal(lang(S,_)),_,_) :- atom(S), !. % is this valid?
typeCheckRange(rdfs:'PlainLiteral',_,_,_) :- !, false.
typeCheckRange(T,V,Instance,Schema) :-
    instanceHasClass(V,T,Instance,Schema),
    !.  % this probably also needs to check class constraints and base type descriptions
typeCheckRange(T,V,Instance,Schema) :- subClass(S,T,Schema), typeCheckRange(S,V,Instance,Schema).

:- rdf_meta typeCheckDomain(r,t). 
typeCheckDomain(T,V,Instance,Schema) :-
    instanceHasClass(V,T,Instance,Schema),
    !. % this probably also needs to check class constraints

typeCheckDomain(T,V,Instance,Schema) :- subClass(S,T,Schema), typeCheckDomain(S,V,Instance,Schema).

invalidInstanceRange(X, P, R, VA, Instance, Schema) :- 
    rdf_resource(X), 
    instanceProperty(X,P,Instance),
    range(P,R,Schema),
    rdf(X,P,V,Instance),
    render(V,VA),
    \+ typeCheckRange(R,V,Instance,Schema).

localInvalidInstanceRanges(X,L,Instance,Schema) :-
    setof(json([error=invalidInstanceRange,
		instance=X, 
		property=P, 
		range=R, 
		value=V]),
	  invalidInstanceRange(X,P,R,V,Instance,Schema),
	  L).

invalidInstanceDomain(X, P, D, Instance, Schema) :- 
    rdf_resource(X),
    instanceProperty(X,P,Instance), 
    domain(P,D,Schema),
    \+ typeCheckDomain(D,X,Instance,Schema).

localInvalidInstanceDomains(X,L,Instance,Schema) :-
    setof(json([error=invalidInstanceDomain,
		instance=X, 
		property=P, 
		domain=D]),
	  invalidInstanceDomain(X,P,D,Instance,Schema),
	  L).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Check Cardinalities

:- rdf_meta cardinalityRestriction(r,r,t).
maxCardinalityRestriction(Restriction,Property,Cardinality,Schema) :-
    rdf(Restriction,rdf:type,owl:'Restriction',Schema),
    rdf(Restriction,owl:onProperty, Property, Schema),
    rdf(Restriction,owl:maxCardinality, literal(type(xsd:nonNegativeInteger, CardStr))),
    atom_number(CardStr,Cardinality).
minCardinalityRestriction(Restriction,Property,Cardinality,Schema) :-
    rdf(Restriction,rdf:type,owl:'Restriction',Schema),
    rdf(Restriction,owl:onProperty, Property, Schema),
    rdf(Restriction,owl:minCardinality, literal(type(xsd:nonNegativeInteger, CardStr))),
    atom_number(CardStr,Cardinality).

rangeCardinalityTooLarge(X,Range,Property,Size,Instance,Schema) :-
    subClassOf(Range,Super,Schema),
    maxCardinalityRestriction(Super,Property,Cardinality,Schema),
    ( setof(Value, rdf(X,Property,Value,Instance), ListX) *-> ListX = List ; List = [] ),
    length(List,Size),
    Size > Cardinality.

rangeCardinalityTooSmall(X,Range,Property,Size,Instance,Schema) :-
    subClassOf(Range,Super,Schema),
    minCardinalityRestriction(Super,Property,Cardinality,Schema),
    ( setof(Value, rdf(X,Property,Value,Instance), ListX) *-> ListX = List ; List = [] ),
    length(List,Size),
    Size > Cardinality.

localCardinalityTooLarge(X,L,Instance,Schema) :-
    setof(json([error=cardinalityTooLarge,
		instance=X,
		range=Range,
		size=Size,
		property=Property]),
	  rangeCardinalityTooLarge(X,Range,Property,Size,Instance,Schema),
	  L).

localCardinalityTooSmall(X,L,Instance,Schema) :-
    setof(json([error=cardinalityTooLarge,
		instance=X,
		range=Range,
		size=Size,
		property=Property]),
	  rangeCardinalityTooSmall(X,Range,Property,Size,Instance,Schema),
	  L).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Blank nodes

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


instanceSubjectBlankNode(X,Instance,_) :- rdf(X,_,_,Instance), rdf_is_bnode(X).
instancePredicateBlankNode(Y,Instance,_) :- rdf(_,Y,_,Instance), rdf_is_bnode(Y).
instanceObjectBlankNode(Z,Instance,_) :- rdf(_,_,Z,Instance), rdf_is_bnode(Z).

instanceBlankNode(Instance,Schema,Reason) :-
    instanceSubjectBlankNode(X,Instance,Schema),
    interpolate(['The subject ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    subject=X].
instanceBlankNode(Schema,Reason,Reason) :-
    instancePredicateBlankNode(X,Instance,Schema),
    interpolate(['The predicate ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    predicate=X].
instanceBlankNode(Schema,Reason,Reason) :-
    instanceObjectBlankNode(X,Instance,Schema),
    interpolate(['The object ', X, ' is a blank node'],Message),
    Reason=[error=instanceBlankNode,
	    message=Message,
	    object=X].		

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Labels 

%%% DDD this does not seem to be called...

classHasLabel(X,Y,Schema) :- class(X,Schema), rdf(X, rdfs:label, Y, Schema).
classHasNoLabel(X,Schema) :- class(X,Schema), \+ rdf(X, rdfs:label, _, Schema).

classHasOneLabel(X,Schema) :- classHasLabel(X,Label,Schema), bagof(label(Y,Label2), classHasLabel(Y,Label2,Schema), L), count(label(X,Label),L,1).

noUniqueClassLabel(X,Schema,Reason) :- 
    \+ classHasOneLabel(X,Label,Schema),
    Reason = [error=duplicateLabelClass, 
	      class=Y, 
	      label=Label2].

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% More instance checking 

localCheckInstanceHasClass(X,[json([error=instanceHasNoClass, 
				    instance=X])],Instance,_) :-
    \+ instanceClass(X,_,Instance).

localCheckInstanceClass(X,[json([error=orphanInstance, 
				 instance=X,
				 class=C])],Instance,Schema) :- 
    orphanInstance(X,C,Instance,Schema).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DB Schema / Instance Checker

% These must pass for us to continue otherwise we'll get infinite computations. 
preTestSchema(classCycle).
preTestSchema(propertyCycle).

testSchema(notUniqueClass).
testSchema(notUniqueProperty).
testSchema(orphanSubClass).
testSchema(orphanSubProperty). 
testSchema(schemaBlankNode).
% This needs to be fixed to deal with subsumption correctly
testSchema(invalidRange). 
testSchema(invalidDomain).

:- rdf_meta runInsert(r,r,o,?).
runInsert([XI,YI,ZI,G]) :-
    transactionGraph:insert(XI,YI,ZI,G).

:- rdf_meta runDelete(r,r,o,?).
runDelete([XI,YI,ZD,G]) :-
    transactionGraph:delete(XI,YI,ZD,G).

% obsolete
runUpdate([XU,YU,ZU,Action,G]) :-
    transactionGraph:update(XU,YU,ZU,Action,G). 

runDelta(Delta,Witnesses) :-
    getKey(deletes, Delta, Deletes, []),
    exclude(schemaRules:runDelete,Deletes,Witnesses), !, % do not backtrack!
    getKey(inserts, Delta, Inserts, []),
    maplist(schemaRules:runInsert,Inserts), !. % do not backtrack!

% The form of Pragma is as follows: 
% {'tests' : [test1, test2, ... testn] ... 
% }

preSchemaTest(Pragma,Schema,Reason) :-
    preTestSchema(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)),
    call(Test, Schema, Reason).

schemaTest(Pragma,Schema,Reason) :-
    testSchema(Test),
    nl,write(Test),nl,	
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)),
    call(Test, Schema, Reason).

runSchemaValidation(Pragma,Witnesses) :-
    getKey(schema, Pragma, Schema, 'schema'),
    findall(Reason, schemaRules:preSchemaTest(Pragma, Schema, Reason), Witnesses) -> true
    ; findall(Reason, schemaRules:schemaTest(Pragma, Schema, Reason), Witnesses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DB Instance Checking. 

%% Local testing for violation of specific known elements in update.
% must have argument list (X,Json,Instance,Schema) :-
testLocal(localCheckInstanceHasClass).
testLocal(localCheckInstanceClass).
testLocal(localInvalidInstanceRanges).
testLocal(localInvalidInstanceDomains).
testLocal(localOrphanInstances).
testLocal(localOrphanProperties). 
testLocal(localCardinalityTooLarge).
testLocal(localCardinalityTooSmall).

instanceValidator(Delta,Pragma,W) :-
    % obtain change information
    getKey(schema, Pragma, Schema, 'schema'),
    getKey(instance, Pragma, Instance, 'instance'), 
    getKey(inserts, Delta, Inserts, []),
    getKey(deletes, Delta, Deletes, []),
    % write('Validating with instance: '), write(Instance), nl,
    (member([X,_,_,Instance], Inserts)
     % don't check instance integrity on things that don't exist [rdf(X,_,_,Instance)]
     ; member([X,_,_,Instance], Deletes), rdf(X,_,_,Instance)), 
    
    testLocal(Test),

    member(tests=TList,Pragma), 

    (all=TList 
     *-> true
     ;  member(Test, TList)), 

    call(Test, X, W, Instance, Schema).

failure_witness([],[]).
failure_witness(DeleteFailures,DeleteWitness) :-
    convert_quads(JSONDeleteFailures,DeleteFailures),
    DeleteWitness = [json([error=deleteFailures,
			   deletes=JSONDeleteFailures,
			   message="Failed to delete all triples"])].

runInstanceUpdate(Delta, Pragma, Witnesses) :-
    % first perform update.
    runDelta(Delta,DeleteFailures),
    failure_witness(DeleteFailures,DeleteWitness),
    (setof(json(W), schemaRules:instanceValidator(Delta,Pragma,W), ValidationWitnessesX)
	  *-> ValidationWitnesses = ValidationWitnessesX ; ValidationWitnesses=[]),
    append(ValidationWitnesses, DeleteWitness, Witnesses),
    getKey(instance, Pragma, Instance, 'instance'), 
    (member(commit='true',Pragma),
     Witnesses = [],
     commit(Instance)	   
     ; rollback(Instance)
    ).

runInstanceValidation(Delta,Pragma,Witnesses) :-
    findall(json(W), schemaRules:instanceValidator(Delta,Pragma,W), Witnesses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Instance / Schema Updates
runSchemaUpdate(Delta, Pragma, Witnesses) :-
    runDelta(Delta,DeleteFailures), % first perform update
    failure_witness(DeleteFailures,DeleteWitness),
    % getKey(schema, Pragma, Schema, 'schema'),
    % getKey(instance, Pragma, Instance, 'instance'),
    runInstanceValidation(Delta,Pragma,Witnesses1),
    runSchemaValidation(Pragma,Witnesses2),
    append(Witnesses1,Witnesses2,Witnesses3),
    append(Witnesses3,DeleteWitness, Witnesses),

    getKey(schema, Pragma, Schema, 'schema'),
    getKey(schema, Pragma, Instance, 'instance'),
    (member(commit='true',Pragma),
     Witnesses = [], 
     commit(Instance), 
     commit(Schema) 
     ; rollback(Instance),
       rollback(Schema)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Full validation (Instance / Schema)
% TODO: Change this to local instance validation with free parameters!!!
% Maybe fixed?
fullInstanceValidator(Pragma,W) :- 
    testLocal(Test),
    member(tests=TList,Pragma),
    (all=TList 
     *-> true
     ;  member(Test, TList)), 

    getKey(schema, Pragma, Schema, 'schema'),
    getKey(instance, Pragma, Instance, 'instance'), 

    call(Test, _, W, Instance, Schema).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schema validation 
runFullValidation(Pragma,Witnesses) :- 
    runSchemaValidation(Pragma,SchemaWitnesses), 
    
    % schema updates can be empty and will check all, but instances can not!
    findall(json(W), fullInstanceValidator(Pragma,W), InstanceWitnesses),
    
    append(SchemaWitnesses,InstanceWitnesses,Witnesses).
