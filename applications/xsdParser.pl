:- module(xsdParser,[decimal/3, digits/3, integer/3, double/5]).

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

nonNegativeInteger(I) --> natural(I) .
nonNegativeInteger(I) --> "+", natural(I) .
nonNegativeInteger(0) --> "-0" .

decimal(M) --> integer(M) .
decimal(M) --> integer(I), fullstop, digits(S),
	       { string_concat("0.", S, T), number_string(E,T), M is I + E } .

unsignedDecimal(M) --> natural(M) .
unsignedDecimal(M) --> natural(I), fullstop, digits(S),
		       { string_concat("0.", S, T), number_string(E,T), M is I + E } .

exp --> "e" .
exp --> "E" .

double(0,0,nan) --> "NAN" .
double(S,0,inf) --> sign(S), "INF" .
double(M,1,double) --> decimal(M) .
double(M,E,double) --> decimal(M), exp, integer(E) .

timeZone(1,0,0) --> "Z" .
timeZone(1,0,0) --> "+", twoDigitNatural(ZH), ":", twoDigitNatural(ZM) .
timeZone(-1,0,0) --> "-", twoDigitNatural(ZH), ":", twoDigitNatural(ZM) .
timeZone(1,0,0) --> "" .

% Hour, Minute, Second, ZoneSign, ZoneHour, ZoneMinute
time(H,M,S,Z,ZH,ZM) --> twoDigitNaturals(H), ":", twoDigitNaturals(M), ":" twoDigitNaturals(S) ,
			timeZone(Z,ZH,ZM) .

year(SY) --> sign(S), fourDigitNatural(Y),
	     { SY is S * Y }.

dateTime(SY,Mo,D,H,M,S,Z,ZH,ZM) -->
    year(SY), "-", twoDigitNatural(Mo), "-", twoDigitNatural(D),
    "T", time(H,M,S,Z,ZH,ZM) ,
    {SY is S * Y}.

gYear(Y,Z,ZH,ZM) --> year(Y), timeZone(Z,ZH,ZM) .

gYearMonth(Y,M,Z,ZH,ZM) --> year(Y), "-", twoDigitNatural(Mo), timeZone(Z,ZH,ZM) .

gMonth(M,Z,ZH,ZM) --> "-", twoDigitNatural(M), timeZone(Z,ZH,ZM) .

gMonthDay(Mo,D,Z,ZH,ZM) --> "-", twoDigitNatural(Mo), "-", twoDigitNaturl(D), timeZone(Z,ZH,ZM) .

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

duration(S,Y,M,D,0,0,0) --> sign(S), "P", maybeYear(Y), maybeMonth(M), maybeDay(D) .
duration(S,Y,M,D,H,M,S) --> sign(S), "P", maybeYear(Y), maybeMonth(M), maybeDay(D),
			    "T", maybeHour(H), maybeMinute(M), maybeSecond(S),
			    { ( M < 0 ; D < 0

			  
