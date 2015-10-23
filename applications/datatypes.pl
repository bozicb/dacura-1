:- module(datatypes,[baseType/1, baseTypeParent/2]).
:- use_module(library(semweb/rdf_db)).
:- use_module(utils).

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
baseType(xsd:anySimpleType).
baseType(xsd:string). 
baseType(xsd:boolean). 
baseType(xsd:decimal). 
baseType(xsd:double). 
baseType(xsd:float). 
baseType(xsd:time).
baseType(xsd:date).  % Unimplemented!!! DDD
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
baseType(xsd:'NOTATION'). % unimplemented.
baseType(xsd:'QName'). % unimplemented.
baseType(xsd:'ID'). % unimplemented.
baseType(xsd:'IDREF'). % unimplemented.
baseType(xsd:'ENTITY'). % unimplemented.
baseType(rdf:'XMLLiteral'). % Not fullly implemented
baseType(rdf:'PlainLiteral').
baseType(rdfs:'Literal').

% We visually represent the heirarchy with whitespace.
:- rdf_meta baseTypeParent(o,o).
baseTypeParent(rdfs:'XMLLiteral',rdfs:'Literal').
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
          baseTypeParent(xsd:'ENTITY',xsd:'NCName'). % unimplemented.
baseTypeParent(xsd:decimal,xsd:anySimpleType).
  baseTypeParent(xsd:integer,xsd:decimal).
    baseTypeParent(xsd:nonPositiveInteger,xsd:integer).
      baseTypeParent(xsd:negativeInteger,xsd:nonPositiveInteger).
    baseTypeParent(xsd:long,xsd:integer).
      baseTypeParent(xsd:short,xsd:long).
        baseTypeParent(xsd:byte,xsd:short).
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
