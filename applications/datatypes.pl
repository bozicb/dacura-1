:- module(datatypes,[]).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsdParser).

datatypeSubumes(_,_).

%% 1. January - 31 days
%% 2. February - 28 days; 29 days in Leap Years
%% 3. March - 31 days
%% 4. April - 30 days
%% 5. May - 31 days
%% 6. June - 30 days
%% 7. July - 31 days
%% 8. August - 31 days
%% 9. September - 30 days
%% 10. October - 31 days
%% 11. November - 30 days
%% 12. December - 31 days
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
    \+ atom(S), term_to_atom(S,A),
    ->
    Reason = [reason='Expected atom, found term',
	      literal=A].
nbasetypeElt(literal(lang(S,L)),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(lang(S,L),A),
    ->
    Reason = [reason='Expected atom in string section, found term.',
	      literal=A].
nbasetypeElt(literal(lang(S,L)),xsd:string,Reason) :-
    \+ atom(L), term_to_atom(lang(S,L),A),
    ->
    Reason = [reason='Expected atom in language section, found term.',
	      literal=A].
nbasetypeElt(literal(type(T,S)),xsd:string,Reason) :-
    \+ atom(S), term_to_atom(type(T,S),A),
    Reason = [reason='Expected atom, found term as element.',
	      literal=A].
nbasetypeElt(literal(type(T,S)),xsd:string,Reason) :-
    \+ atom(T), term_to_atom(type(T,S),A),
    ->
    Reason = [reason='Expected atom, found term as type.',
	      literal=A].
nbasetypeElt(literal(type(T1,_)),T2,Reason) :-
    \+ dataTypeSubsumes(T1,T2), term_to_atom(T1,A), term_to_atom(T2,B),
    ->
    Reason = [reason='Could not subsume type1 with type2',
	      type1=A,
	      type2=B].
nbasetypeElt(literal(type(_,S)),xsd:boolean,Reason) :-
    \+ member(S,['true','false','1','0']), term_to_atom(S,A),
    ->
    Reason = [reason='Not a well formed boolean.',
	      literal=A,
	      type='xsd:boolean'].
nbasetypeElt(literal(type(_,S)),xsd:decimal,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:decimal,C,[])),
    ->
    Reason = [reason='Not a well formed decimal.',
	      literal=S,
	      type='xsd:decimal'].
nbasetypeElt(literal(type(_,S)),xsd:integer, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer,C,[])),
    ->
    Reason = [reason='Not a well formed integer.',
	      literal=S,
	      type='xsd:integer'].
nbasetypeElt(literal(type(_,S)),xsd:double, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:double(_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed double.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:double, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 53, N > Max
    ->
    Reason = [reason='Not a well formed double: Mantisa is massive.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:double, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,E,T),C,[]),
    (E > 970 ; E < -1075),
    ->
    Reason = [reason='Not a well formed double: exponent excessive.',
	      literal=S,
	      type='xsd:double'].
nbasetypeEltx(literal(type(_,S)),xsd:float,Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:double(_,_,_),C,[])),
    ->
    Reason = [reason='Not a well formed float.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:float, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 24, N > Max
    ->
    Reason = [reason='Not a well formed float: mantisa is massive.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:float, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(_,E,_),C,[]),
    (E > 104 ; E < -149)
    ->
    Reason = [reason='Not a well formed float: exponent excessive.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:time, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:time(_,_,_,_,_,_),C,[]))
    ->
    Reason = [reason='Not a well formed xsd:time',
	      literal=S,
	      type='xsd:time'].
nbasetypeElt(literal(type(_,S)),xsd:time, Reason) :-
    atom_codes(S,C), phrase(xsdParser:time(H,M,S,Z,ZH,ZM),C,[]),
    (H > 23 ; M > 59 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [reason='Not a well formed xsd:time : parameter out of range.',
	      literal=S,
	      type='xsd:time'].
nbasetypeElt(literal(type(_,S)),xsd:dateTime, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:dateTime(_,_,_,_,_,_,_,_,_),C,[])),
    ->
    Reason = [reason='Not a well formed xsd:dateTime.',
	      literal=S,
	      type='xsd:dateTime'].
nbasetypeElt(literal(type(_,S)),xsd:dateTime, Reason) :-
    atom_codes(S,C), phrase(xsdParser:dateTime(SY,Mo,D,H,M,S,Z,ZH,ZM),C,[]),
    (Mo > 12 ; Mo < 1
     ; daysInMonth(SY,Mo,Days), D > Days
     ; D < 1 ; H > 23 ; M > 59
     ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [reason='Not a well formed xsd:dateTime : parameter out of range.',
	      literal=S,
	      type='xsd:dateTime'].
nbasetypeElt(literal(type(_,S)),xsd:gYear, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gYear(_,_,_,_),C,[])),
    ->
    Reason = [reason='Not a well formed xsd:gYear',
	      literal=S,
	      type='xsd:gYear'].
nbasetypeElt(literal(type(_,S)),xsd:gYear, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gYear(_,Z,ZH,ZM),C,[]),
    ((\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [reason='Not a well formed xsd:gYear : parameters out of range',
	      literal=S,
	      type='xsd:gYear'].
nbasetypeElt(literal(type(_,S)),xsd:gMonth, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gMonth(_,_,_,_),C,[])),
    ->
    Reason = [reason='Not a well formed xsd:Month',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gMonth, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gMonth(M,Z,ZH,ZM),C,[]),
    (M < 12 ; M > 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    -> 
    Reason = [reason='Not a well formed xsd:gMonth : parameters out of range',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gDay, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gDay(_,_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:gMonth',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gDay, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gDay(D,Z,ZH,ZM),C,[]),
    (D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [reason='Not a well formed xsd:gMonth : parameters out of range',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gYearMonth, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gYearMonth(_,_,_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:gYearMonth',
	      literal=S,
	      type='xsd:gYearMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gYearMonth, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gYearMonth(Y,M,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [reason='Not a well formed xsd:gYearMonth : parameters out of range',
	      literal=S,
	      type='xsd:gYearMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gMonthDay, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gMonthDay(_,_,_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:gYearMonth',
	      literal=S,
	      type='xsd:gMonthDay'].
nbasetypeElt(literal(type(_,S)),xsd:gMonthDay, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gMonthDay(M,D,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [reason='Not a well formed xsd:gMonthDay : parameters out of range',
	      literal=S,
	      type='xsd:gMonthDay'].
nbasetypeElt(literal(type(_,S)),xsd:duration, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:duration(_,_,_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:duration',
	      literal=S,
	      type='xsd:duration'].
nbasetypeElt(literal(type(_,S)),xsd:yearMonthDuration, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:yearMonthDuration(_,_,_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:yearMonthDuration',
	      literal=S,
	      type='xsd:yearMonthDuration'].
nbasetypeElt(literal(type(_,S)),xsd:dayTimeDuration, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:dayTimeDuration(_,_,_,_,_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:dayTimeDuration',
	      literal=S,
	      type='xsd:dayTimehDuration'].
nbasetypeElt(literal(type(_,S)),xsd:byte, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:byte',
	      literal=S,
	      type='xsd:byte'].
nbasetypeElt(literal(type(_,S)),xsd:byte, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -128 ; I > 127 )
    ->   
    Reason = [reason='Not a well formed xsd:byte: out of range.',
	      literal=S,
	      type='xsd:byte'].
nbasetypeElt(literal(type(_,S)),xsd:short, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:short',
	      literal=S,
	      type='xsd:short'].
nbasetypeElt(literal(type(_,S)),xsd:short, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -32768 ; I > 32767 )
    ->   
    Reason = [reason='Not a well formed xsd:short: out of range.',
	      literal=S,
	      type='xsd:short'].
nbasetypeElt(literal(type(_,S)),xsd:int, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:int',
	      literal=S,
	      type='xsd:int'].
nbasetypeElt(literal(type(_,S)),xsd:int, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -2147483648 ; I > 2147483647 )
    ->   
    Reason = [reason='Not a well formed xsd:int: out of range.',
	      literal=S,
	      type='xsd:int'].
nbasetypeElt(literal(type(_,S)),xsd:long, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:long',
	      literal=S,
	      type='xsd:long'].
nbasetypeElt(literal(type(_,S)),xsd:long, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -9223372036854775808 ; I > 9223372036854775807 )
    ->   
    Reason = [reason='Not a well formed xsd:long: out of range.',
	      literal=S,
	      type='xsd:long'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedByte, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:unsignedByte',
	      literal=S,
	      type='xsd:unsignedByte'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedByte, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 255 )
    ->   
    Reason = [reason='Not a well formed xsd:unsignedByte: out of range.',
	      literal=S,
	      type='xsd:unsignedByte'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedShort, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:unsignedShort',
	      literal=S,
	      type='xsd:unsignedShort'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedShort, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 65535 )
    ->   
    Reason = [reason='Not a well formed xsd:unsignedShort: out of range.',
	      literal=S,
	      type='xsd:unsignedShort'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedInt, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:unsignedInt',
	      literal=S,
	      type='xsd:unsignedInt'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedInt, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 4294967295 )
    ->   
    Reason = [reason='Not a well formed xsd:unsignedInt: out of range.',
	      literal=S,
	      type='xsd:unsignedInt'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedLong, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:unsignedLong',
	      literal=S,
	      type='xsd:unsignedLong'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedLong, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 18446744073709551615 )
    ->   
    Reason = [reason='Not a well formed xsd:unsignedLong: out of range.',
	      literal=S,
	      type='xsd:unsignedLong'].
nbasetypeElt(literal(type(_,S)),xsd:positiveInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:positiveInteger',
	      literal=S,
	      type='xsd:positiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:positiveInteger, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    I < 1 
    ->   
    Reason = [reason='Not a well formed xsd:positiveInteger: out of range.',
	      literal=S,
	      type='xsd:positiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:nonNegativeInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:nonNegativeInteger',
	      literal=S,
	      type='xsd:nonNegativeInteger'].
nbasetypeElt(literal(type(_,S)),xsd:negativeInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:negativeInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:negativeInteger',
	      literal=S,
	      type='xsd:negativeInteger'].
nbasetypeElt(literal(type(_,S)),xsd:nonPositiveInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:nonPositiveInteger(_),C,[]))
    ->   
    Reason = [reason='Not a well formed xsd:nonPositiveInteger',
	      literal=S,
	      type='xsd:nonPositiveInteger'].

%% %% nbasetypeElt(xsd:base64Binary).
%% %% nbasetypeElt(xsd:anyURI). 
%% %% nbasetypeElt(xsd:language). 
%% %% nbasetypeElt(xsd:normalizedString). 
%% %% nbasetypeElt(xsd:token). 
%% %% nbasetypeElt(xsd:'NMTOKEN'). 
%% %% nbasetypeElt(xsd:'Name'). 
%% %% nbasetypeElt(xsd:'NCName'). 
%% %% nbasetypeElt(rdf:'PlainLiteral').
%% %% nbasetypeElt(rdf:'Literal').
