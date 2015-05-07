
:- module(schemaRules,[demoDB/0, demoDB/1, demoDB/2, demoDB/3, 
		       populateDB/1, 
		       checkDB/1, 
		       loadAndCheckDB/3, 
		       test/1,
		       runSchemaUpdate/3,
		       runInstanceUpdate/3,
		       runFullValidation/2,
		       tests/0]).

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

%classes
class(X) :- rdf(X, rdf:type, rdfs:'Class', schema).
class(X) :- rdf(X, rdf:type, owl:'Class', schema). 

uniqueClass(Y) :- class(Y), bagof(X, class(X), L), count(Y,L,1).

notUniqueClass(Y) :- class(Y), bagof(X, class(X), L), \+ count(Y,L,1).

duplicateClasses(L) :- setof(json([error=notUniqueClass, 
				   class=Y]), notUniqueClass(Y), L).

% subclasses 

subClass(X,Y) :- rdf(X, rdfs:subClassOf, Y, schema).

subClassOf(X,Y) :- rdf(X, rdfs:subClassOf, Y, schema).
subClassOf(X,Z) :- rdf(X, rdfs:subClassOf, Y, schema), subClassOf(Y,Z).

subClassOfClass(X,Y) :- subClassOf(X,Y), class(Y).

notSubClassOfClass(X,Y) :- subClassOf(X,Y), \+ class(Y).

orphanSubClasses(L) :- setof(json([error=notSubClassOfClass, 
				   child=X, 
				   parent=Y]), notSubClassOfClass(X,Y),L).

% subclass cycles
classCycleHelp(C,S,[]) :- get_assoc(C,S,true), !.
classCycleHelp(C,S,[K|P]) :- class(C), subClass(K, C), 
			     put_assoc(C, S, true, S2), classCycleHelp(K,S2,P).

classCycle(C,P) :- empty_assoc(S), classCycleHelp(C,S,P). 

classCycles(L) :- setof(json([error=classCycle, 
			      class=CC,
			      path=P]), classCycle(CC,P), L).

% properties.
:- rdf_meta property(r).
property(rdfs:label).
property(rdfs:comment).
property(P) :- rdf(P, rdf:type, owl:'ObjectProperty', schema).
property(P) :- rdf(P, rdf:type, owl:'DataProperty', schema).

uniqueProperty(P) :- property(P), bagof(P2, property(P2), L), count(P,L,1).

notUniqueProperty(P) :- property(P), bagof(P2, property(P2), L), \+ count(P,L,1).

duplicateProperties(L) :- setof(json([error=notUniqueProperty, 
				      property=P]), notUniqueProperty(P), L).

% subProperties.

subProperty(X,Y) :- rdf(X, rdfs:subPropertyOf, Y, schema).

subPropertyOf(X,Y) :- rdf(X, rdfs:subPropertyOf, Y, schema).
subPropertyOf(X,Z) :- rdf(X, rdfs:subPropertyOf, Y, schema), subPropertyOf(Y, Z). 

subPropertyOfProperty(X,Y) :- subPropertyOf(X,Y), property(Y).

notSubPropertyOfProperty(X,Y) :- subPropertyOf(X,Y), \+ property(Y).

orphanSubProperties(L) :- setof(json([error=notSubPropertyOfProperty, 
				      child=X,
				      parent=Y]), notSubPropertyOfProperty(X,Y),L).

% subProperty cycles 

propertyCycleHelp(P,S,[]) :- get_assoc(P,S,true), !.
propertyCycleHelp(P,S,[Q|T]) :- property(P), subProperty(Q, P), put_assoc(P, S, true, S2), propertyCycleHelp(Q,S2,T).

propertyCycle(P,PC) :- empty_assoc(S), propertyCycleHelp(P,S,PC). 

propertyCycles(L) :- setof(json([error=propertyCycle, 
				 property=P,
				 path=PC]), propertyCycle(P,PC),L).

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
type(X) :- baseType(X), !. 
type(X) :- class(X). 

% range / domain

:- rdf_meta range(r,r,t,o).
range(P,R) :- rdf(P, rdfs:range, R, schema).

:- rdf_meta domain(r,r,t,o).
domain(P,D) :- rdf(P, rdfs:domain, D, schema). 

validRange(P,R) :- range(P,R), type(R).
validDomain(P,D) :- domain(P,D), type(D).

uniqueValidRange(P,R) :- range(P,R), findall(R2, validRange(P,R2), L), length(L,1).

uniqueValidDomain(P,D) :- domain(P,D), findall(D2, validRange(P,D2), L), length(L,1).

notUniqueValidRange(P,R) :- range(P,R), findall(R2, validRange(P,R2), L), \+ length(L,1).

notUniqueValidDomain(P,D) :- domain(P,D), findall(D2, validDomain(P,D2), L), \+ length(L,1).

% does this do too much? 
invalidRange(L) :- setof(json([error=notUniqueValidRange, 
			       property=Y,
			       range=R]), notUniqueValidRange(Y,R), L).

invalidDomain(L) :- setof(json([error=notUniqueValidDomain, 
				property=Y, 
				domain=D]), notUniqueValidDomain(Y,D), L).

%%%%%%%%%%%%%%%%%%%%%%%
%% Instance constraints

instanceClass(X, Y) :- rdf(X, rdf:type, Y, instance).

instance(X) :- instanceClass(X,_).

instanceHasClass(X,C) :- instanceClass(X, C), class(C).

orphanInstance(X,C) :- instanceClass(X,C), \+ class(C).

noOrphans :- \+ orphanInstance(_,_).

orphanInstances(L) :- setof(json([error=orphanInstance,
				  instance=X, 
				  class=C]),orphanInstance(X,C), L).

instanceProperty(X,P) :- instance(X), rdf(X, P, _), \+ P='http://www.w3.org/1999/02/22-rdf-syntax-ns#type'. 

instanceHasPropertyClass(X,P) :- instanceProperty(X,P), property(P).

noInstancePropertyClass(X,P) :- instanceProperty(X,P), \+ property(P).

orphanProperties(L) :- setof(json([error=noInstancePropertyClass,
				   instance=X,
				   property=Y]), noInstancePropertyClass(X,Y), L). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Instance Type Checking constraints 

% ranges have more possible targets as they can be literals. 
:- rdf_meta typeCheckRange(r,t).
typeCheckRange(T,literal(type(T,_))) :- baseType(T), !.
typeCheckRange(_,literal(type(_,_))) :- !, false.
typeCheckRange(xsd:string,literal(S)) :- atom(S).
typeCheckRange(xsd:string,literal(lang(S,_))) :- atom(S), !.
typeCheckRange(xsd:string, _) :- !, false.
typeCheckRange(rdfs:'PlainLiteral',literal(S)) :- atom(S).
typeCheckRange(rdfs:'PlainLiteral',literal(lang(S,_))) :- atom(S), !.
typeCheckRange(rdfs:'PlainLiteral',_) :- !, false.
typeCheckRange(rdfs:'Literal',literal(S)) :- atom(S). % is this valid?
typeCheckRange(rdfs:'Literal',literal(lang(S,_))) :- atom(S), !. % is this valid?
typeCheckRange(rdfs:'PlainLiteral',_) :- !, false.
typeCheckRange(T,V) :- instanceHasClass(V,T), !.  % this probably also needs to check class constraints
typeCheckRange(T,V) :- subClass(S,T), typeCheckRange(S,V).

:- rdf_meta typeCheckDomain(r,t). 
typeCheckDomain(T,V) :- instanceHasClass(V,T), !. % this probably also 
                                                  % needs to check class constraints

typeCheckDomain(T,V) :- subClass(S,T), typeCheckDomain(S,V).

invalidInstanceRange(X, P, R, VA) :- 
    rdf_resource(X), 
    instanceProperty(X,P),
    range(P, R),
    rdf(X,P,V, instance),
    render(V,VA),
    \+ typeCheckRange(R,V).

invalidInstanceRanges(L) :- setof(json([error=invalidInstanceRange,
					instance=X, 
					property=P, 
					range=R, 
					value=V]), invalidInstanceRange(X,P,R,V), L).

invalidInstanceDomain(X, P, D) :- 
    rdf_resource(X),
    instanceProperty(X,P), 
    domain(P, D),
    \+ typeCheckDomain(D,X).

invalidInstanceDomains(L) :- setof(json([error=invalidInstanceDomain,
					 instance=X, 
					 property=P, 
					 domain=D]), invalidInstanceDomain(X,P,D), L).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Blank nodes

blankNode(X) :- (G = instance ; G = schema), rdf(X,_,_,G), rdf_is_bnode(X).
blankNode(Y) :- (G = instance ; G = schema), rdf(_,Y,_,G), rdf_is_bnode(Y).
blankNode(Z) :- (G = instance ; G = schema), rdf(_,_,Z,G), rdf_is_bnode(Z).

blankNodes(L) :- setof(json([error=blankNode,
			     blank=X]), blankNode(X), L). 

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Labels 

classHasLabel(X,Y) :- class(X), rdf(X, rdfs:label, Y, schema).
classHasNoLabel(X) :- class(X), \+ rdf(X, rdfs:label, _, schema).

classHasOneLabel(X) :- classHasLabel(X,Label), bagof(label(Y,Label2), classHasLabel(Y,Label2), L), count(label(X,Label),L,1).

duplicateLabelClasses(X) :- 
    classHasLabel(X,Label), 
    bagof(json([error=duplicateLabelClass, 
		class=Y, 
		label=Label2]), classHasLabel(Y,Label2), L), 
    \+ count(label(X,Label),L,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instance checking 

checkInstanceClass(X, json([error=orphanInstance, 
			    instance=X,
			    class=C])) :- 
    orphanInstance(X,C).

checkPropertyDomain(X, json([error=invalidInstanceDomain, 
			     instance=X, 
			     property=P, 
			     domain=D])) :- 
    invalidInstanceDomain(X, P, D).

checkPropertyRange(X, json([error=invalidInstanceRange, 
			    instance=X, 
			    property=P, 
			    range=R, 
			    value=V])) :- 
    invalidInstanceRange(X, P, R, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instance Data Generator

:- use_module(library(assoc)). 

% These can be elaborated as desired.
:- rdf_meta baseTypeGenerator(r,t).
baseTypeGenerator(xsd:string, literal(xsd:string,'This is a string')). 
baseTypeGenerator(xsd:boolean, literal(xsd:boolean, 'true')). 
baseTypeGenerator(xsd:decimal, literal(xsd:decimal, '10.5')). 
baseTypeGenerator(xsd:integer, literal(xsd:integer, '1')). 
baseTypeGenerator(xsd:double, literal(xsd:double, '13423432.2342343')). 
baseTypeGenerator(xsd:float, literal(xsd:float, '134432.23443')).
baseTypeGenerator(xsd:time, literal(xsd:time, '09:00:00')).
baseTypeGenerator(xsd:dateTime, literal(xsd:dateTime, '2002-05-30T09:00:00')). 
baseTypeGenerator(xsd:dateTimeStamp, literal(xsd:dateTimeStamp, '2004-04-12T13:20:00-05:00')).
baseTypeGenerator(xsd:gYear, literal(xsd:gYear, '2000')). 
baseTypeGenerator(xsd:gMonth, literal(xsd:gMonth, '05')). 
baseTypeGenerator(xsd:gDay, literal(xsd:gDay, '01')). 
baseTypeGenerator(xsd:gYearMonth, literal(xsd:gYearMonth, '2000-05')).
baseTypeGenerator(xsd:gMonthDay, literal(xsd:gMonthDay, '05-01')).
baseTypeGenerator(xsd:duration, literal(xsd:duration, '0001-01-01T01:01:01-01:01')). 
baseTypeGenerator(xsd:yearMonthDuration, literal(xsd:yearMonthDuration, '01-01')). 
baseTypeGenerator(xsd:dayTimeDuration, literal(xsd:dayTimeDuration, '01-01:01:01')). 
baseTypeGenerator(xsd:byte, literal(xsd:byte, '12')). 
baseTypeGenerator(xsd:short, literal(xsd:short, '12')). 
baseTypeGenerator(xsd:int, literal(xsd:int, '12')). 
baseTypeGenerator(xsd:long, literal(xsd:long, '12')). 
baseTypeGenerator(xsd:unsignedByte, literal(xsd:unsignedByte, '12')). 
baseTypeGenerator(xsd:unsignedInt, literal(xsd:unsignedInt, '12')). 
baseTypeGenerator(xsd:unsignedLong, literal(xsd:unsignedLong, '12')). 
baseTypeGenerator(xsd:positiveInteger, literal(xsd:positiveInteger, '12')). 
baseTypeGenerator(xsd:nonNegativeInteger, literal(xsd:nonNegativeInteger, '12')). 
baseTypeGenerator(xsd:negativeInteger, literal(xsd:negativeInteger, '-12')). 
baseTypeGenerator(xsd:nonPositiveInteger, literal(xsd:nonPositiveInteger, '0')). 
baseTypeGenerator(xsd:base64Binary, literal(xsd:base64Binary, 'ASDF')). 
baseTypeGenerator(xsd:anyURI, literal(xsd:anyURI, 'http://example.com')). 
baseTypeGenerator(xsd:language, literal(xsd:language, 'en')). 
baseTypeGenerator(xsd:normalizedString, literal(xsd:normalizedString, 'Some string')). 
baseTypeGenerator(xsd:token, literal(xsd:token, 'SomeToken')). 
baseTypeGenerator(xsd:'NMTOKEN', literal(xsd:'NMTOKEN', 'ASDF')). 
baseTypeGenerator(xsd:'Name', literal(xsd:'Name', 'myElement')). 
baseTypeGenerator(xsd:'NCName', literal(xsd:'NCName', 'myElement')). 
baseTypeGenerator(rdf:'PlainLiteral', literal('This is a literal')).

:- rdf_meta (t,o).
nameIt(R,Name) :-
    uri_components(R, uri_components(_,_,Path,Res,Anchor)),
    (ground(Anchor) *-> Anchor = BaseName
     ; ground(Res) *-> Res = BaseName
     ; ground(Path) *-> 
	     path_end(Path,End),
	     End = BaseName 
     ; false),
    atom_concat('instance-', BaseName, A),
    gensym(A, Name). 

classRoot(X) :- class(X), \+ subClassOf(X,_).

classPropertyClass(C,P,Z) :- domain(P,C), range(P,Z), class(C), property(P), class(Z).
%classPropertyClass(C,P,Z) :- subClassOf(C, K), classPropertyClass(K, P, Z). 
%classPropertyClass(C,P,Z) :- subClassOf(Z, K), classPropertyClass(C, P, K).

classPropertyLiteral(C,P,LiteralType) :- 
    domain(P,C), range(P,LiteralType), class(C), property(P), 
    baseType(LiteralType). 

generateLinks(_,X,[rdf(X, 'http://www.w3.org/2000/01/rdf-schema#label', literal('Rubbish'))],_).
generateLinks(C,X,[rdf(X,P,V)],_) :- 
    classPropertyLiteral(C,P,LiteralType), 
    baseTypeGenerator(LiteralType,V).
generateLinks(C,X,[ rdf(Y,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',K), 
		    rdf(X, P, Y) | O],S) :- 
    classPropertyClass(C,P,K),
    (get_assoc(K, S, Y) 
     ->  O=[]  % remove cycles by reusing instances of encountered classes.
     ; nameIt(K,Y),
       put_assoc(K, S, Y, S2),
       bagof(R, generateLinks(K, Y, R, S2), L), 
       flatten(L, O)
    ).
generateLinks(C,X,[rdf(Y,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',K), 
		   rdf(X, P, Y) | O],S) :- 
    classPropertyClass(C,P,Super),
    subClassOf(K, Super), 
    class(K), % possible to have ill defined subclasses!
    (get_assoc(K, S, Y)
     ->  O=[]  % remove cycles by reusing instances of encountered classes.
     ; nameIt(K,Y), 
       put_assoc(K, S, Y, S2),
       bagof(R, generateLinks(K, Y, R, S2), L), 
       flatten(L, O)
    ).

generateClosed(C,[rdf(X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C)|O],S) :- 
    class(C),
    nameIt(C,X),
    put_assoc(C, S, X, S2),
    bagof(R, generateLinks(C, X, R, S2), L), 
    flatten(L,O).
generateClosed(C,[rdf(X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',Sub)|O],S) :- 
    subClassOf(Sub,C), %possible to have ill defined subclasses!
    class(Sub),
    nameIt(Sub,X),
    put_assoc(Sub, S, X, S2),
    bagof(R, generateLinks(Sub, X, R, S2), L), 
    flatten(L,O).

generate(L) :- empty_assoc(S), classRoot(C), generateClosed(C, L, S). 

generateN(N,L) :- findnsols(N, L1, generate(L1), LL), flatten(LL, L).

:- use_module(library(apply)).

:- rdf_meta addToDB(t).
addToDB(rdf(X,Y,Z)) :- rdf_assert(X,Y,Z,instance). 

% N specifies number of times to decend the class hierarchy rather than 
% the number of classes or triples, M is the number of solutions to look for 
% in the class hierarchy.  This is convenient as consistency 
% is a global property which can't easily be maintained without total traversal.  
populateDB(0, _) :- !.
populateDB(N, M) :- N2 is N-1, generateN(N,L), maplist(addToDB, L), populateDB(N2, M). 

% sorry about the magic numbers...
populateDB(N) :- HierarchyTravesals=30, populateDB(N, HierarchyTravesals). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Carefully Corrupting the DB.

corrupt_class :- 
    class(X), rdf_assert(X, rdf:type, owl:'Class', schema). % create duplicates

corrupt_instance :- 
    gensym('rubbish', X),
    class(Y),
    rdf_assert(X, rdf:type, Y), 
    property(P), 
    gensym('rubbish_target', Z),
    rdf_assert(X, P, Z).

% Corrupt the database.  This should excercise the reasoner. 
corruptDB(0).
corruptDB(N) :- 
    M is N-1, 
    corrupt_class,
    corrupt_instance, 
    corruptDB(M).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Testing

%% Schema and Instance

demoDB :-  
    rdf_retractall(_, _, _, instance), 
    rdf_retractall(_, _, _, schema), 
    rdf_load('testData/plants.rdf', [graph(instance)]), 
    rdf_load('testData/plant-onto.rdf', [graph(schema)]).

demoDB(Schema) :- 
    rdf_retractall(_, _, _, instance), 
    rdf_retractall(_, _, _, schema), 
    rdf_load(Schema, [graph(schema)]). 

demoDB(Schema,Instance) :-
    rdf_retractall(_, _, _, instance), 
    rdf_retractall(_, _, _, schema), 
    rdf_load(Schema, [graph(schema)]), 
    rdf_load(Instance, [graph(instance)]).

demoDB(Schema,Instance,Options) :- 
    rdf_retractall(_, _, _, instance), 
    rdf_retractall(_, _, _, schema), 
    rdf_load(Schema, [graph(schema)|Options]),     
    rdf_load(Instance, [graph(instance)|Options]). 


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DB Schema / Instance Checker

%! test(+Test:atom) is det.
%! test(?Test:atom) is nondet.
test(classCycles).
test(propertyCycles). 
test(duplicateClasses).
test(duplicateProperties).
test(orphanSubClasses).
test(orphanSubProperties). 
test(orphanInstances).
test(orphanProperties). 
test(blankNodes).
test(invalidRange). 
test(invalidDomain). 
test(invalidInstanceRanges).
test(invalidInstanceDomains).

%! testMessage(+Test:atom, -Message:atom) is det.
%! testMessage(?Test:atom, -Message:atom) is det.
% this package prefixing is a ridiculous hack to deal with metapredicate handling.
testMessage(schemaRules:classCycles, 'Cycles in class hierarchy: ') :- !.
testMessage(schemaRules:propertyCycles, 'Cycles in property hierarchy: ') :- !. 
testMessage(schemaRules:duplicateClasses, 'Duplicate classes: ') :- !.
testMessage(schemaRules:duplicateProperties, 'Duplicate properties: ') :- !.
testMessage(schemaRules:orphanSubClasses, 'Orphaned subclasses: ') :- !.
testMessage(schemaRules:orphanSubProperties, 'Orphaned subproperties: ') :- !.
testMessage(schemaRules:orphanInstances, 'Orphaned instances: ') :- !.
testMessage(schemaRules:orphanProperties, 'Missing class for properties: ') :- !. 
testMessage(schemaRules:blankNodes, 'Blank Nodes found: ') :- !.
testMessage(schemaRules:invalidRange, 'Property with non-unique or invalid range found: ') :- !. 
testMessage(schemaRules:invalidDomain, 'Property with non-unique or invalid domain found: ') :- !. 
testMessage(schemaRules:invalidInstanceRange, 'Instance data has incorrect type: ') :- !. 
testMessage(_,'Unknown test').

runInsert([XI,YI,ZI,G]) :- 
    insert(XI,YI,ZI,G).

runDelete([XI,YI,ZD,G]) :-
    delete(XI,YI,ZD,G). 

% obsolete
runUpdate([XU,YU,ZU,Action,G]) :-
    update(XU,YU,ZU,Action,G). 

runDelta(Delta) :-
    getKey(inserts, Delta, Inserts, []),
    getKey(deletes, Delta, Deletes, []),
    maplist(runDelete,Inserts),
    maplist(runDelete,Deletes).
    
% The form of Pragma is as follows: 
% {'tests' : [test1, test2, ... testn] ... 
% }

runSchemaUpdate(Delta, Pragma, Witnesses) :- 
    runDelta(Delta), % first perform update
    test(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)), 
    (bagof(W, call(Test, W), Witnesses) 
     ; Witnesses = [], 
       (member(commit='true',Pragma), commit(instance), 
	commit(schema) 
	; true)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DB Instance Checking. 

instanceTest(schemaRules:checkInstanceClass).
instanceTest(schemaRules:checkPropertyRange). 
instanceTest(schemaRules:checkPropertyDomain). 

instanceValidator(Delta,Pragma,W) :-
    % obtain change information
    getKey(inserts, Delta, Inserts, []),
    getKey(deletes, Delta, Deletes, []),
    
    (member([X,_,_,instance], Inserts) 
     ; member([X,_,_,instance], Deletes)),
    
    instanceTest(Test),

    member(tests=TList,Pragma), 

    (all=TList 
     *-> true
     ;  member(Test, TList)), 

    call(Test, X, W).

runInstanceUpdate(Delta, Pragma, Witnesses) :- 
    % first perform update.
    runDelta(Delta), 
    (bagof(W, instanceValidator(Delta,Pragma, W), Witnesses)
     ; Witnesses = [], 
       (member(commit='true',Pragma), commit(instance)
	; true)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Full validation.
fullInstanceValidator(Pragma,W) :- 
    instanceTest(Test),
    member(tests=TList,Pragma), 
    (all=TList 
     *-> true
     ;  member(Test, TList)), 
    
    % rdf(X,Y,Z,instance),
    rdf_resource(X), 
    call(Test, X, W).

runFullValidation(Pragma,Witnesses) :- 
    runSchemaUpdate([],Pragma,SchemaWitnesses), 
    
    % schema updates can be empty and will check all, but instances can not!
    (bagof(W, fullInstanceValidator(Pragma,W), InstanceWitnesses) 
     ; InstanceWitnesses = []),
    
    append(SchemaWitnesses,InstanceWitnesses,Witnesses).
		  

:- meta_predicate validate(1,?).
validate(Test, Stream) :- 
    call(Test, C),
    nl(Stream),	
    testMessage(Test, Message),
    write(Test),nl,
    write(Stream, Message),
    nl(Stream),	 
    write(Stream, C), 
    nl(Stream), 
    fail.

% validate will always fail, we want to iterate over choice points in T
% to accumulate all I/O side effects.
runValidate(Stream) :-
    test(Test), 
    validate(Test,Stream). 
runValidate(_).

:- meta_predicate runTest(1).
runTest(Test) :- 
    atom_concat('testData/', Test, TestBegin), 
    atom_concat(TestBegin, '.ttl', TestFile), 
    demoDB(TestFile, 'testData/instance.ttl'), 
    call(Test, _), !, fail.
runTest(Test) :-
    atom_concat('Failed test ', Test, Fail),
    write(Fail), nl, fail.

% runTest will always fail, we want to iterate over choice points in T
% to accumulate all I/O side effects.
tests :- 
    test(Test),
    runTest(Test).
tests.

stringStream(Handle, Stream) :-
    new_memory_file(Handle),
    open_memory_file(Handle, write, Stream).

streamString(Handle, String) :-
    open_memory_file(Handle, read, R, [free_on_close(true)]),
    read_string(R, _, String).

loadAndCheckDB(Schema,Instance,Output) :-
    % clear out the triple store.
    % load rdf
    rdf_load(Schema, [graph(schema)]),
    rdf_load(Instance, [graph(instance)]),
    checkDB(Output).

checkDB(Output) :-
    %% Setup output string stream 
    stringStream(Handle,Stream),
    write(Stream, '***** Starting check of DB *****'),
    nl(Stream),	 
    runValidate(Stream),
    nl(Stream),
    write(Stream, 'Finished checking DB!'),
    close(Stream), 
    streamString(Handle, Output).


