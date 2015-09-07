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
class(X,Schema) :- rdf(X, rdf:type, rdfs:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Class', Schema).
class(X,Schema) :- rdf(X, rdf:type, owl:'Restriction', Schema).
class(X,Schema) :- rdf(X, rdf:type, rdf:'Alt', Schema).
class(X,Schema) :- rdf(X, rdf:type, rdf:'Bag', Schema).
% Skos concept? for class as well?

% DDD not negation has no value?
uniqueClass(Y,Schema) :- class(Y, Schema), setof(X, class(X, Schema), L), count(Y,L,1).

notUniqueClass(Y, Schema) :- class(Y, Schema), setof(X, class(X,Schema), L), \+ count(Y,L,1).

notUniqueClassJSON(JSON,Y,Schema) :-
    notUniqueClass(Y,Schema),
    atom_string(Y,Ystr),
    string_concat(Ystr," is not a unique class.  Some existing class has this identifier.",
		  Message),
    JSON = json([error=notUniqueClass,
		 class=Y,
		 message=Message]). 

duplicateClasses(L, Schema) :- setof(JSON, notUniqueClassJSON(JSON,_,Schema), L).

% subclasses 

subClass(X,Y, Schema) :- rdf(X, rdfs:subClassOf, Y, Schema).

subClassOf(X,Y,Schema) :- rdf(X, rdfs:subClassOf, Y, Schema).
subClassOf(X,Z,Schema) :- rdf(X, rdfs:subClassOf, Y, Schema), subClassOf(Y,Z, Schema).

% DDD Not negation has no value?
subClassOfClass(X,Y,Schema) :- subClassOf(X,Y,Schema), class(Y,Schema).

notSubClassOfClass(X,Y,Schema) :- subClassOf(X,Y,Schema), \+ class(Y,Schema).

notSubClassOfClassJSON(JSON,X,Y,Schema) :-
    notSubClassOfClass(X,Y,Schema),
    atom_string(X,Xstr),
    atom_string(Y,Ystr),
    string_concat(Xstr, " is not a subclass of some valid class named ", M),
    string_concat(M,Ystr,M2),
    string_concat(M2,".",Message),
    JSON=json([error=notSubClassOfClass,
	       child=X,
	       parent=Y,
	       message=Message]).
    
orphanSubClasses(L,Schema) :- setof(JSON, notSubClassOfClassJSON(JSON,_,_,Schema),L).

% subclass cycles
classCycleHelp(C,S,[],_) :- get_assoc(C,S,true), !.
classCycleHelp(C,S,[K|P],Schema) :- class(C,Schema), subClass(K,C,Schema), 
				    put_assoc(C,S,true,S2), classCycleHelp(K,S2,P,Schema).

classCycle(C,P,Schema) :- empty_assoc(S), classCycleHelp(C,S,P,Schema). 

classCycleJSON(JSON,CC,P,Schema) :-
    classCycle(CC,P,Schema),
    atom_string(CC,CCstr),
    term_string(P,Pstr),
    string_concat("Class ", CCstr, M1),
    string_concat(M1, " has a class cycle with path: ", M2),
    string_concat(M2,Pstr,Message),
    JSON=json([error=classCycle,
	       class=CC,
	       path=P,
	       message=Message]).

classCycles(L,Schema) :- setof(JSON, classCycleJSON(JSON,_,_,Schema), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% properties.

:- rdf_meta property(r,o).
property(rdfs:label,_).
property(rdfs:comment,_).
property(P,Schema) :- rdf(P,rdf:type,owl:'ObjectProperty',Schema).
property(P,Schema) :- rdf(P,rdf:type,owl:'DataProperty',Schema).
property(P,Schema) :- rdf(P,rdf:type,owl:'AnnotationProperty',Schema).

uniqueProperty(P,Schema) :- property(P,Schema), bagof(P2, property(P2,Schema), L), count(P,L,1).

notUniqueProperty(P,Schema) :- property(P,Schema), bagof(P2, property(P2,Schema), L), \+ count(P,L,1).

notUniquePropertyJSON(JSON,P,Schema) :-
    notUniqueProperty(P,Schema),
    atom_string(P,Pstr),
    string_concat(Pstr, " is not a unique property name, there is already a property with this name.", Message),
    JSON=json([error=notUniqueProperty,
	       property=P,
	       message=Message]). 
    
duplicateProperties(L,Schema) :- setof(JSON, notUniquePropertyJSON(JSON,_,Schema), L).

% subProperties.

subProperty(X,Y,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema).

subPropertyOf(X,Y,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema).
subPropertyOf(X,Z,Schema) :- rdf(X,rdfs:subPropertyOf,Y,Schema), subPropertyOf(Y,Z,Schema). 

subsumptionPropertiesOf(PC,PP,Schema) :- subPropertyOf(PC, PP, Schema).
subsumptionPropertiesOf(PC,PC,Schema) :- property(PC,Schema).

subPropertyOfProperty(X,Y,Schema) :- subPropertyOf(X,Y,Schema), property(Y,Schema).

notSubPropertyOfProperty(X,Y,Schema) :- subPropertyOf(X,Y,Schema), \+ property(Y,Schema).

notSubPropertyOfPropertyJSON(JSON,X,Y,Schema) :-
    notSubPropertyOfProperty(X,Y,Schema),
    atom_string(X,Xstr),
    atom_string(Y,Ystr),
    string_concat(Xstr," is not a sub-property of ",M1),
    string_concat(M1,Ystr,M2),
    string_concat(M2,".",Message),
    JSON=json([error=notSubPropertyOfProperty,
	       child=X,
	       parent=Y,
	       message=Message]).

orphanSubProperties(L,Schema) :- setof(JSON, notSubPropertyOfPropertyJSON(JSON,_,_,Schema),L).

% subProperty cycles 

propertyCycleHelp(P,S,[],_) :- get_assoc(P,S,true), !.
propertyCycleHelp(P,S,[Q|T],Schema) :- property(P,Schema), subProperty(Q,P,Schema), put_assoc(P, S, true, S2), propertyCycleHelp(Q,S2,T,Schema).

propertyCycle(P,PC,Schema) :- empty_assoc(S), propertyCycleHelp(P,S,PC,Schema). 

propertyCycleJSON(JSON,P,PC,Schema) :-
    propertyCycle(P,PC,Schema),
    atom_string(P,Pstr),
    term_string(PC,PCstr),
    string_concat("Property Class ", Pstr, M1),
    string_concat(M1, " has a property class cycle with path: ", M2),
    string_concat(M2,PCstr,Message),
    JSON=json([error=propertyClassCycle,
	       property=P,
	       path=PC,
	       message=Message]).


propertyCycles(L,Schema) :- setof(JSON, propertyCycleJSON(JSON,_,_,Schema),L).

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
baseType(xsd:int). 
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

:- rdf_meta type(r).
type(X,_) :- baseType(X), !. 
type(X,Schema) :- class(X,Schema). 

% range / domain

:- rdf_meta range(r,r,t,o).
range(P,R,Schema) :- rdf(P2, rdfs:range, R, Schema), subsumptionPropertiesOf(P,P2,Schema).

:- rdf_meta domain(r,r,t,o).
domain(P,D,Schema) :- rdf(P2, rdfs:domain, D, Schema), subsumptionPropertiesOf(P,P2,Schema).

validRange(P,R,Schema) :- range(P,R,Schema), type(R,Schema).
validDomain(P,D,Schema) :- domain(P,D,Schema), type(D,Schema).

uniqueValidRange(P,R,Schema) :- range(P,R,Schema), findall(R2, validRange(P,R2,Schema), L), length(L,1).

uniqueValidDomain(P,D,Schema) :- domain(P,D,Schema), findall(D2, validRange(P,D2,Schema), L), length(L,1).

notUniqueValidRange(P,R,Schema) :- range(P,R,Schema), findall(R2, validRange(P,R2,Schema), L), \+ length(L,1).

notUniqueValidDomain(P,D,Schema) :- domain(P,D,Schema), findall(D2, validDomain(P,D2,Schema), L), \+ length(L,1).

% does this do too much? 
invalidRange(L,Schema) :- setof(json([error=notUniqueValidRange, 
				      property=Y,
				      range=R]), notUniqueValidRange(Y,R,Schema), L).

invalidDomain(L,Schema) :- setof(json([error=notUniqueValidDomain, 
				       property=Y, 
				       domain=D]), notUniqueValidDomain(Y,D,Schema), L).

%%%%%%%%%%%%%%%%%%%%%%%
%% Instance constraints

instanceClass(X, Y, Instance) :- rdf(X, rdf:type, Y, Instance).

instance(X,Instance) :- instanceClass(X,_,Instance).

instanceHasClass(X,C,Instance,Schema) :- instanceClass(X,C,Instance), class(C,Schema).

orphanInstance(X,C,Instance,Schema) :- instanceClass(X,C,Instance), \+ class(C,Schema).

noOrphans(Instance,Schema) :- \+ orphanInstance(_,_,Instance,Schema).

localOrphanInstances(X,L,Instance,Schema) :-
    setof(json([error=orphanInstance,
		instance=X, 
		class=C]),
	  orphanInstance(X,C,Instance,Schema),
	  L).

instanceProperty(X,P,Instance) :-
    instance(X,Instance),
    rdf(X, P, _,Instance),
    \+ P='http://www.w3.org/1999/02/22-rdf-syntax-ns#type'. 

instanceHasPropertyClass(X,P,Instance,Schema) :-
    instanceProperty(X,P,Instance),
    property(P,Schema).

noInstancePropertyClass(X,P,Instance,Schema) :- instanceProperty(X,P,Instance), \+ property(P,Schema).

localOrphanProperties(X,L,Instance,Schema) :- setof(json([error=noInstancePropertyClass,
							  instance=X,
							  property=Y]),
						    noInstancePropertyClass(X,Y,Instance,Schema),
						    L). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Instance Type Checking constraints 

% ranges have more possible targets as they can be literals. 
:- rdf_meta typeCheckRange(r,t).
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

schemaBlankNode(X,Schema) :- rdf(X,_,_,Schema), rdf_is_bnode(X).
schemaBlankNode(Y,Schema) :- rdf(_,Y,_,Schema), rdf_is_bnode(Y).
schemaBlankNode(Z,Schema) :- rdf(_,_,Z,Schema), rdf_is_bnode(Z).

schemaBlankNodes(L,Schema) :- setof(json([error=schemaBlankNode,
					  blank=X]),
				    schemaBlankNode(X,Schema), L). 


instanceBlankNode(X,Instance,_) :- rdf(X,_,_,Instance), rdf_is_bnode(X).
instanceBlankNode(Y,Instance,_) :- rdf(_,Y,_,Instance), rdf_is_bnode(Y).
instanceBlankNode(Z,Instance,_) :- rdf(_,_,Z,Instance), rdf_is_bnode(Z).

instanceBlankNodes(L,Instance,Schema) :- setof(json([error=instanceBlankNode,
						     blank=X]),
					       instanceBlankNode(X,Instance,Schema), L). 

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Labels 

%%% DDD this does not seem to be called...

classHasLabel(X,Y,Schema) :- class(X,Schema), rdf(X, rdfs:label, Y, Schema).
classHasNoLabel(X,Schema) :- class(X,Schema), \+ rdf(X, rdfs:label, _, Schema).

classHasOneLabel(X,Schema) :- classHasLabel(X,Label,Schema), bagof(label(Y,Label2), classHasLabel(Y,Label2,Schema), L), count(label(X,Label),L,1).

duplicateLabelClasses(X, Schema) :- 
    classHasLabel(X,Label,Schema), 
    bagof(json([error=duplicateLabelClass, 
		class=Y, 
		label=Label2]), classHasLabel(Y,Label2,Schema), L), 
    \+ count(label(X,Label),L,1).

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
preTestSchema(classCycles).
preTestSchema(propertyCycles).

testSchema(duplicateClasses).
testSchema(duplicateProperties).
testSchema(orphanSubClasses).
testSchema(orphanSubProperties). 
testSchema(schemaBlankNodes).
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

schemaTest(Pragma,W,Schema) :-
    preTestSchema(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)),
    call(Test, W, Schema),
    (W = [] *-> fail ; true, !). % Fail early for these special tests (cycles!!)    
schemaTest(Pragma,W,Schema) :-
    testSchema(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)),
    call(Test, W, Schema).

runSchemaValidation(Pragma,Witnesses) :-
    getKey(schema, Pragma, Schema, 'schema'),
    findall(W, schemaRules:schemaTest(Pragma, W, Schema), Witnesses).

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
    (setof(W, schemaRules:instanceValidator(Delta,Pragma,W), ValidationWitnessesX)
	  *-> ValidationWitnesses = ValidationWitnessesX ; ValidationWitnesses=[]),
    append(ValidationWitnesses, DeleteWitness, Witnesses),
    getKey(instance, Pragma, Instance, 'instance'), 
    (member(commit='true',Pragma),
     Witnesses = [],
     commit(Instance)	   
     ; rollback(Instance)
    ).

runInstanceValidation(Delta,Pragma,Witnesses) :-
    findall(W, schemaRules:instanceValidator(Delta,Pragma,W), Witnesses).

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
    findall(W, fullInstanceValidator(Pragma,W), InstanceWitnesses),
    
    append(SchemaWitnesses,InstanceWitnesses,Witnesses).
