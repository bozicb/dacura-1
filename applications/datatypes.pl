:- module(datatypes,[datatypeSubsumptionOf/2,		     
		     datatypeStrictSubsumptionOf/2,
		     nbasetypeElt/3, baseType/1]).
:- use_module(library(semweb/rdf_db)).
:- use_module(xsdParser).
:- use_module(library(uri)).

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
%% xsd:integer	-2147483648…+2147483647 (32 bit)
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
baseType(rdfs:'Literal').

% We visually represent the heirarchy with whitespace.
:- rdf_meta baseTypeParent(r,r).
baseTypeParent(xsd:anySimpleType,rdfs:'Literal').
baseTypeParent(xsd:string,xsd:anySimpleType).
 baseTypeParent(xsd:normalizedString, xsd:string).
   baseTypeParent(xsd:token, xsd:normalizedString).
      baseTypeParent(xsd:language, xsd:token).
      baseTypeParent(xsd:'NMTOKEN', xsd:token).
      baseTypeParent(xsd:'Name', xsd:token).
        baseTypeParent(xsd:'NCName', xsd:'Name').
          baseTypeParent(xsd:'ID',xsd:'NCName'). % unimplemented.
          baseTypeParent(xsd:'IDREF',xsd:'NCName'). % unimplemented.
          baseTypeParent(xsd:'ENTITY',xsd:'NCName'). % unimplemexgot linented.
baseTypeParent(xsd:decimal,xsd:anySimpleType).
  baseTypeParent(xsd:integer,xsd:decimal).
    baseTypeParent(xsd:nonPositiveInteger,xsd:integer).
      baseTypeParent(xsd:negativeInteger,xsd:nonPositiveInteger).
    baseTypeParent(xsd:long,xsd:intger).
      baseTypeParent(xsd:integer,xsd:long).
        baseTypeParent(xsd:short,xsd:integer).
          baseTypeParent(xsd:integer,xsd:byte).
    baseTypeParent(xsd:nonNegativeInteger,xsd:integer).
      baseTypeParent(xsd:unsignedLong,xsd:nonNegativeInteger). 
        baseTypeParent(xsd:unsignedInt,xsd:unsginedLong).
          baseTypeParent(xsd:unsignedShort,xsd:unsignedInt).
            baseTypeParent(xsd:unsignedByte,xsd:unsignedShort).
      baseTypeParent(xsd:positiveInteger,xsd:nonNegativeInteger).
baseTypeParent(xsd:'NOTATION',xsd:anySimpleType). % unimplemented.
baseTypeParent(xsd:'QName',xsd:anySimpleType). % unimplemented.
baseTypeParent(xsd:float,xsd:anySimpleType).
baseTypeParent(xsd:double,xsd:anySimpleType).
baseTypeParent(xsd:boolean,xsd:anySimpleType).
baseTypeParent(xsd:base64Binary,xsd:anySimpleType).
baseTypeParent(xsd:hexBinary,xsd:anySimpleType).
baseTypeParent(xsd:anyURI,xsd:anySimpleType).
baseTypeParent(xsd:date,xsd:anySimpleType).
baseTypeParent(xsd:time,xsd:anySimpleType).
baseTypeParent(xsd:dateTime,xsd:anySimpleType).
baseTypeParent(xsd:gYear,xsd:anySimpleType).
baseTypeParent(xsd:gYearMonth,xsd:anySimpleType).
baseTypeParent(xsd:gMonth,xsd:anySimpleType).
baseTypeParent(xsd:gMonthDay,xsd:anySimpleType).
baseTypeParent(xsd:gDay,xsd:anySimpleType).
baseTypeParent(xsd:duration,xsd:anySimpleType).
  baseTypeParent(xsd:dayTimeDuration,xsd:duration).
  baseTypeParent(xsd:yearMonthDuration,xsd:duration).

:- rdf_meta datatypeSubsumptionOf(r,r).
datatypeSubsumptionOf(T,T).
datatypeSubsumptionOf(Sub,Super) :- baseTypeParent(Sub,Parent), datatypeSubsumptionOf(Parent,Super).

:- rdf_meta datatypeStrictSubsumptionOf(r,r).
datatypeStrictSubsumptionOf(Sub,Super) :- baseTypeParent(Sub,Super).
datatypeStrictSubsumptionOf(Sub,Super) :- baseTypeParent(Sub,Parent), datatypeSubsumptionOf(Parent,Super).

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
    \+ datatypeSubsumptionOf(T1,T2)
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
nbasetypeElt(literal(type(_,S)),xsd:integer, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed integer.',
	      literal=S,
	      type='xsd:integer'].
nbasetypeElt(literal(type(_,S)),xsd:double, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:double(_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed double.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:double, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 53, N > Max
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed double: Mantisa is massive.',
	      literal=S,
	      type='xsd:double'].
nbasetypeElt(literal(type(_,S)),xsd:double, Reason) :-
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
nbasetypeElt(literal(type(_,S)),xsd:float, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(M,_,_),C,[]),
    abs(M, N), Max is 2 ^ 24, N > Max
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed float: mantisa is massive.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:float, Reason) :-
    atom_codes(S,C), phrase(xsdParser:double(_,E,_),C,[]),
    (E > 104 ; E < -149)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed float: exponent excessive.',
	      literal=S,
	      type='xsd:float'].
nbasetypeElt(literal(type(_,S)),xsd:time, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:time(_,_,_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:time',
	      literal=S,
	      type='xsd:time'].
nbasetypeElt(literal(type(_,S)),xsd:time, Reason) :-
    atom_codes(S,C), phrase(xsdParser:time(H,M,S,Z,ZH,ZM),C,[]),
    (H > 23 ; M > 59 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59 )
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:time : parameter out of range.',
	      literal=S,
	      type='xsd:time'].
nbasetypeElt(literal(type(_,S)),xsd:dateTime, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:dateTime(_,_,_,_,_,_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:dateTime.',
	      literal=S,
	      type='xsd:dateTime'].
nbasetypeElt(literal(type(_,S)),xsd:dateTime, Reason) :-
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
nbasetypeElt(literal(type(_,S)),xsd:gYear, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gYear(_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYear',
	      literal=S,
	      type='xsd:gYear'].
nbasetypeElt(literal(type(_,S)),xsd:gYear, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gYear(_,Z,ZH,ZM),C,[]),
    ((\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYear : parameters out of range',
	      literal=S,
	      type='xsd:gYear'].
nbasetypeElt(literal(type(_,S)),xsd:gMonth, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gMonth(_,_,_,_),C,[]))
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:Month',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gMonth, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gMonth(M,Z,ZH,ZM),C,[]),
    (M < 12 ; M > 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    -> 
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonth : parameters out of range',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gDay, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gDay(_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonth',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gDay, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gDay(D,Z,ZH,ZM),C,[]),
    (D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonth : parameters out of range',
	      literal=S,
	      type='xsd:gMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gYearMonth, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gYearMonth(_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYearMonth',
	      literal=S,
	      type='xsd:gYearMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gYearMonth, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gYearMonth(_,M,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYearMonth : parameters out of range',
	      literal=S,
	      type='xsd:gYearMonth'].
nbasetypeElt(literal(type(_,S)),xsd:gMonthDay, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:gMonthDay(_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gYearMonth',
	      literal=S,
	      type='xsd:gMonthDay'].
nbasetypeElt(literal(type(_,S)),xsd:gMonthDay, Reason) :-
    atom_codes(S,C), phrase(xsdParser:gMonthDay(M,D,Z,ZH,ZM),C,[]),
    (M > 12 ; M < 1 ; D < 1 ; D > 31 ; (\+ member(Z,[1,-1])) ; ZH > 6 ; ZM > 59)
    ->
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:gMonthDay : parameters out of range',
	      literal=S,
	      type='xsd:gMonthDay'].
nbasetypeElt(literal(type(_,S)),xsd:duration, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:duration(_,_,_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:duration',
	      literal=S,
	      type='xsd:duration'].
nbasetypeElt(literal(type(_,S)),xsd:yearMonthDuration, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:yearMonthDuration(_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:yearMonthDuration',
	      literal=S,
	      type='xsd:yearMonthDuration'].
nbasetypeElt(literal(type(_,S)),xsd:dayTimeDuration, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:dayTimeDuration(_,_,_,_,_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:dayTimeDuration',
	      literal=S,
	      type='xsd:dayTimehDuration'].
nbasetypeElt(literal(type(_,S)),xsd:byte, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:byte',
	      literal=S,
	      type='xsd:byte'].
nbasetypeElt(literal(type(_,S)),xsd:byte, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -128 ; I > 127 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:byte: out of range.',
	      literal=S,
	      type='xsd:byte'].
nbasetypeElt(literal(type(_,S)),xsd:short, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:short',
	      literal=S,
	      type='xsd:short'].
nbasetypeElt(literal(type(_,S)),xsd:short, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -32768 ; I > 32767 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:short: out of range.',
	      literal=S,
	      type='xsd:short'].
nbasetypeElt(literal(type(_,S)),xsd:int, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:int',
	      literal=S,
	      type='xsd:int'].
nbasetypeElt(literal(type(_,S)),xsd:int, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -2147483648 ; I > 2147483647 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:int: out of range.',
	      literal=S,
	      type='xsd:int'].
nbasetypeElt(literal(type(_,S)),xsd:long, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:integer(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:long',
	      literal=S,
	      type='xsd:long'].
nbasetypeElt(literal(type(_,S)),xsd:long, Reason) :-
    atom_codes(S,C), phrase(xsdParser:integer(I),C,[]),
    (I < -9223372036854775808 ; I > 9223372036854775807 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:long: out of range.',
	      literal=S,
	      type='xsd:long'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedByte, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedByte',
	      literal=S,
	      type='xsd:unsignedByte'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedByte, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 255 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedByte: out of range.',
	      literal=S,
	      type='xsd:unsignedByte'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedShort, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedShort',
	      literal=S,
	      type='xsd:unsignedShort'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedShort, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 65535 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedShort: out of range.',
	      literal=S,
	      type='xsd:unsignedShort'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedInt, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedInt',
	      literal=S,
	      type='xsd:unsignedInt'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedInt, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 4294967295 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedInt: out of range.',
	      literal=S,
	      type='xsd:unsignedInt'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedLong, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedLong',
	      literal=S,
	      type='xsd:unsignedLong'].
nbasetypeElt(literal(type(_,S)),xsd:unsignedLong, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    (I < 0 ; I > 18446744073709551615 )
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:unsignedLong: out of range.',
	      literal=S,
	      type='xsd:unsignedLong'].
nbasetypeElt(literal(type(_,S)),xsd:positiveInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:positiveInteger',
	      literal=S,
	      type='xsd:positiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:positiveInteger, Reason) :-
    atom_codes(S,C), phrase(xsdParser:positiveInteger(I),C,[]),
    I < 1 
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:positiveInteger: out of range.',
	      literal=S,
	      type='xsd:positiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:nonNegativeInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:positiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:nonNegativeInteger',
	      literal=S,
	      type='xsd:nonNegativeInteger'].
nbasetypeElt(literal(type(_,S)),xsd:negativeInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:negativeInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:negativeInteger',
	      literal=S,
	      type='xsd:negativeInteger'].
nbasetypeElt(literal(type(_,S)),xsd:nonPositiveInteger, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:nonPositiveInteger(_),C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:nonPositiveInteger',
	      literal=S,
	      type='xsd:nonPositiveInteger'].
nbasetypeElt(literal(type(_,S)),xsd:base64Binary, Reason) :-
    \+ (atom_codes(S,C), phrase(xsdParser:base64Binary,C,[]))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:base64Binary',
	      literal=S,
	      type='xsd:base64Binary'].
nbasetypeElt(literal(type(_,S)),xsd:anyURI, Reason) :-
    \+ uri_components(S,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:anyUri',
	      literal=S,
	      type='xsd:anyURI'].
nbasetypeElt(literal(type(_,S)),xsd:language, Reason) :-
    \+ uri_components(xsdParser:language,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:language',
	      literal=S,
	      type='xsd:language'].
nbasetypeElt(literal(type(_,S)),xsd:normalizedString, Reason) :-
    \+ uri_components(xsdParser:normalizedString,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:normalizedString',
	      literal=S,
	      type='xsd:normalizedString'].
nbasetypeElt(literal(type(_,S)),xsd:token, Reason) :-
    \+ uri_components(xsdParser:normalizedString,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:token',
	      literal=S,
	      type='xsd:token'].
nbasetypeElt(literal(type(_,S)),xsd:'NMTOKEN', Reason) :-
    \+ uri_components(xsdParser:nmtoken,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:NMTOKEN',
	      literal=S,
	      type='xsd:NMTOKEN'].
nbasetypeElt(literal(type(_,S)),xsd:'Name', Reason) :-
    \+ uri_components(xsdParser:name,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:Name',
	      literal=S,
	      type='xsd:Name'].
nbasetypeElt(literal(type(_,S)),xsd:'NCName', Reason) :-
    \+ uri_components(xsdParser:ncname,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:NCName',
	      literal=S,
	      type='xsd:NCName'].
nbasetypeElt(literal(type(_,S)),xsd:'NCName', Reason) :-
    \+ uri_components(xsdParser:ncname,_)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed xsd:NCName',
	      literal=S,
	      type='xsd:NCName'].
nbasetypeElt(literal(T),rdf:'PlainLiteral', Reason) :-
    (lang(_,_) \= T ; \+ atom(T))
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed rdf:PlainLiteral',
	      literal=T,
	      type='rdf:PlainLiteral'].
nbasetypeElt(X,rdf:'Literal', Reason) :-
    literal(_) \= X, term_to_atom(X,T)
    ->   
    Reason = [error=nbasetypeElt, 
	      message='Not a well formed rdf:Literal',
	      literal=T,
	      type='rdf:Literal'].
