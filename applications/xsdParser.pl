:- module(xsdParser,[decimal/3, digits/3, integer/3, double/5,
		     positiveInteger/3, nonPositiveInteger/3, nonNegativeInteger/3,
		     unsignedDecimal/3, year/3, dateTime/11,
		     gYear/6, gYearMonth/7, gMonth/6, gMonthDay/7,
		     gDay/6, duration/9, yearMonthDuration/5,
		     dayTimeDuration/7,
		     base64Binary/2, language/2]).

%:- use_module(iso_639).
:- use_module(iana).

sign(1) --> "+".
sign(-1) --> "-".
sign(1) --> "".


digit("0") --> "0".
digit("1") --> "1".
digit("2") --> "2".
digit("3") --> "3".
digit("4") --> "4".
digit("5") --> "5".
digit("6") --> "6".
digit("7") --> "7".
digit("8") --> "8".
digit("9") --> "9".

twoDigitNatural(N) --> digit(A), digit(B), { string_concat(A,B,C), number_string(N,C) } . 

fourDigitNatural(N) --> digit(A), digit(B), digit(C), digit(D),
			{ string_concat(A,B,S1), string_concat(S1,C,S2), string_concat(S2,D,S3), number_string(N,S3) } . 
    
digits(S) --> digit(S) .
digits(T) --> digit(X), digits(S),
	      { string_concat(X, S, T) } .

natural(N) --> digits(S),
	       { number_string(N,S) }.

fullstop --> ".".

integer(I) --> sign(S), natural(N),
	       { I is N * S } .

positiveInteger(I) --> natural(I) .
positiveInteger(I) --> "+", natural(I) .

negativeInteger(I) --> "-", natural(N) , { N > 0, I is N * -1 }.

nonPositiveInteger(I) --> "-", natural(N) , { I is N * -1 } .

nonNegativeInteger(I) --> natural(I) .
nonNegativeInteger(I) --> "+", natural(I) .
nonNegativeInteger(0) --> "-0" .

decimal(M) --> integer(I), fullstop, digits(S),
	       { string_concat("0.", S, T), number_string(E,T), M is I + E } .
decimal(M) --> integer(M) .

unsignedDecimal(M) --> natural(I), fullstop, digits(S),
		       { string_concat("0.", S, T), number_string(E,T), M is I + E } .
unsignedDecimal(M) --> natural(M) .

exp --> "e" .
exp --> "E" .

double(0,0,nan) --> "NAN" .
double(S,0,inf) --> sign(S), "INF" .
double(M,1,double) --> decimal(M) .
double(M,E,double) --> decimal(M), exp, integer(E) .

timeZone(1,0,0) --> "Z" .
timeZone(1,ZH,ZM) --> "+", twoDigitNatural(ZH), ":", twoDigitNatural(ZM) .
timeZone(-1,ZH,ZM) --> "-", twoDigitNatural(ZH), ":", twoDigitNatural(ZM) .
timeZone(1,0,0) --> "" .

% Hour, Minute, Second, ZoneSign, ZoneHour, ZoneMinute
time(H,M,S,Z,ZH,ZM) --> twoDigitNatural(H), ":", twoDigitNatural(M), ":", twoDigitNatural(S),
			timeZone(Z,ZH,ZM) .

year(SY) --> sign(S), fourDigitNatural(Y),
	     { SY is S * Y }.

dateTime(SY,Mo,D,H,M,S,Z,ZH,ZM) -->
    year(SY), "-", twoDigitNatural(Mo), "-", twoDigitNatural(D),
    "T", time(H,M,S,Z,ZH,ZM) .

gYear(Y,Z,ZH,ZM) --> year(Y), timeZone(Z,ZH,ZM) .

gYearMonth(Y,M,Z,ZH,ZM) --> year(Y), "-", twoDigitNatural(M), timeZone(Z,ZH,ZM) .

gMonth(M,Z,ZH,ZM) --> "-", twoDigitNatural(M), timeZone(Z,ZH,ZM) .

gMonthDay(Mo,D,Z,ZH,ZM) --> "-", twoDigitNatural(Mo), "-", twoDigitNatural(D), timeZone(Z,ZH,ZM) .

gDay(D,Z,ZH,ZM) --> "--", twoDigitNatural(D), timeZone(Z,ZH,ZM) .


maybeYear(Y) --> natural(Y), "Y" .
maybeYear(0) --> "" .

maybeMonth(M) --> natural(M), "M" .
maybeMonth(0) --> "" .

maybeDay(D) --> natural(D), "D" .
maybeDay(-1) --> "" .

maybeHour(H) --> natural(H), "H" .
maybeHour(-1) --> "" .

maybeMinute(M) --> natural(M), "M" .
maybeMinute(-1) --> "" .

maybeSecond(S) --> unsignedDecimal(S), "S" .
maybeSecond(-1) --> "" .

maybeTime(H,M,S) --> "T", maybeHour(MH), maybeMinute(MM), maybeSecond(MS),
		     { (MH < 0, MM < 0, MS < 0)
		       -> fail
		       ; (MH < 0 -> H = 0 ; MH = H),
			 (MM < 0 -> M = 0 ; MM = M),
			 (MS < 0 -> S = 0 ; MS = S) } .
maybeTime(0,0,0) --> "" .

duration(Sign,Y,Mo,D,H,M,S) --> sign(Sign), "P", maybeYear(Y), maybeMonth(Mo), maybeDay(D),
				maybeTime(H,M,S) .

yearMonthDuration(Sign,Y,Mo) --> sign(Sign), "P", maybeYear(Y), maybeMonth(Mo) .

dayTimeDuration(Sign,D,H,M,S) --> sign(Sign), "P", maybeDay(D), maybeTime(H,M,S) .

%  Base64 encoding
space --> " " .
space --> "\n" .
space --> "\t" .
space --> "\r" .

base64char --> "+" .
base64char --> "/" .

charRange(First,Last,[H|T],T) :- atom_codes(First,[Start]), atom_codes(Last,[End]),
				 H >= Start, H =< End .

alphaUpper --> charRange('A','Z') .

alphaLower --> charRange('a','z') .

alpha --> alphaUpper .
alpha --> alphaLower .

alphas --> alphas, alpha .
alphas --> alpha .

whitespace --> space, whitespace .
whitespace --> "" .
    
base64elt --> alpha . 
base64elt --> base64char .
base64elt --> digit(_) .

oneOf(Chars,[H|T],T) :- atom_codes(Chars,Codes),member(H,Codes) .

base64elt1 --> oneOf('AEIMQUYcgkosw048') .
base64elt2 --> oneOf('AQgw') .

equals --> "=" .

base64Terminates --> whitespace, base64elt,
		     whitespace, base64elt,
		     whitespace, base64elt,
		     whitespace, base64elt .
base64Terminates --> whitespace, base64elt,
		     whitespace, base64elt2,
		     whitespace, equals, 
		     whitespace, equals .
base64Terminates --> whitespace, base64elt,
		     whitespace, base64elt,
		     whitespace, base64elt1,
		     whitespace, equals .

base64Binary --> whitespace, base64elt,
		 whitespace, base64elt,
		 whitespace, base64elt,
		 whitespace, base64elt ,
		 base64Binary .
base64Binary --> base64Terminates .

iso_639_base([H1,H2,H3|T],T) :- atom_codes(A,[H1,H2,H3]), iso_639_3(A,_), !.
iso_639_base([H1,H2,H3|T],T) :- atom_codes(A,[H1,H2,H3]), iso_639_2(A,_).

language --> "en-US" .
language --> "en-GB" .
language --> iso_639_base .
language --> iana(_) . % IANA
language --> "x-", alphas . % unregistered

