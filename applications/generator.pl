:- module(generator,[]).

:- use_module(schemaRules). 

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


