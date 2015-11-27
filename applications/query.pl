:- module(query, [classFrame/3,allEntities/2]).

:- use_module(utils).
:- use_module(library(http/http_log)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(semweb/rdf_persistency)).
:- use_module(datatypes).
:- use_module(tbox).
:- use_module(abox).
:- use_module(transactionGraph).
	     
% We should be creating stubs from the underlying graph
% which means we need some way to query it.

/******************************************************

Structure of template description

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

Lists of frames are interpreted as disjoint union.

FRAME = FRAMESPEC | CLASS
FRAMESPEC = PROPSPEC | [FRAMESPEC, ..., FRAMESPEC] 
PROPSPEC = {prop=propname, object=FRAME, restriction=RESTSPEC}
RESTSPEC = [and, REST1, ... RESTN] | [or, REST1, ... RESTN] | [disj, REST1, RESTN] | [not, REST] 
| {mincard=N, valuesFrom=Class} | {maxcard=N, valuesFrom=Class} 
| {card=N, valuesFrom=Class} | {hasValue=value} | {allValuesFrom=Class} | {someValuesFrom=Class}

*******************************************************/

entity(Class,Schema) :-
    subsumptionOf(Class, 'http://dacura.cs.tcd.ie/data/seshat#Entity', Schema). 

allEntities(Schema,AE) :-
    uniqueSolns(E,query:entity(E,Schema),AE).

mostSpecificPropertiesHelper([],_,_,[]).
mostSpecificPropertiesHelper([P|Rest],Properties,Schema,Out) :-
    mostSpecificPropertiesHelper(Rest,Properties,Schema,Ok),
    member(P2,Properties),
    (strictSubsumptionPropertiesOf(P2,P,Schema) *-> Out = Ok
     ; Out = [P|Ok]).

mostSpecificProperties(Ps1,Schema,Ps2) :-
    mostSpecificPropertiesHelper(Ps1,Ps1,Schema,Ps2).

classProperties(Class, Schema, Properties) :-
    uniqueSolns(P,tbox:anyDomain(P,Class,Schema),Properties).

:- rdf_meta hasFormula(r,o).
hasFormula(Class,Schema) :-
    subClassOf(Class,_,Schema)
    ; intersectionOf(Class,_,Schema)
    ; unionOf(Class,_,Schema)
    ; disjointUnionOf(Class,_,Schema).

:- rdf_meta restrictionType(r,t,?).
restrictionType(CR,restriction([uri=CR,property=OP,someValuesFrom=C]),Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:someValuesFrom,C,Schema).
restrictionType(CR,restriction([uri=CR,property=OP,allValuesFrom=C]),Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:allValuesFrom,C,Schema).
restrictionType(CR,restriction([uri=CR,property=OP,minCardinality=N]),Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:minCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N).
restrictionType(CR,restriction([uri=CR,property=OP,maxCardinality=N]),Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:maxCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N).
restrictionType(CR,restriction([uri=CR,property=OP,cardinality=N]),Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:cardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    atom_number(CardStr,N).
restrictionType(CR,restriction([uri=CR,property=OP,minQualifiedCardinality=N,onClass=C]), Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:minQualifiedCardinality,literal(type(xsd:nonNegativeInteger, CardStr)),Schema),
    xrdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N).
restrictionType(CR,restriction([uri=CR,property=OP,maxQualifiedCardinality=N,onClass=C]), Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:maxQualifiedCardinality,literal(type(xsd:nonNegativeInteger,CardStr)),Schema),
    xrdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N).
restrictionType(CR,restriction([uri=CR,property=OP,qualifiedCardinality=N,onClass=C]), Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:qualifiedCardinality,literal(type(xsd:nonNegativeInteger,CardStr)),Schema),
    xrdf(CR,owl:onClass,C,Schema),
    atom_number(CardStr,N).
restrictionType(CR,restriction([uri=CR,property=OP,hasValue=V]), Schema) :-
    xrdf(CR,owl:onProperty,OP,Schema),
    xrdf(CR,owl:hasValue,V,Schema).

:- rdf_meta classFormula(o,r,t).
classFormula(Schema, Class, (Class<SuperFormula)) :-
    class(Class,Schema),
    subClassOf(Class,Y,Schema),
    classFormula(Schema, Y, SuperFormula).
classFormula(Schema, Class, (Class=and(Solns))) :-
    class(Class,Schema),
    setof(Sol,Y^(tbox:intersectionOf(Class,Y,Schema),
		 query:classFormula(Schema,Y,Sol)),
	  Solns).
classFormula(Schema,Class,(Class=or(Solns))) :-
    class(Class,Schema),
    setof(Sol,Y^(tbox:unionOf(Class,Y,Schema),
		 query:classFormula(Schema,Y,Sol)),
	  Solns).
classFormula(Schema,Class,(Class=xor(Solns))) :-
    class(Class,Schema),
    setof(Sol,Y^(tbox:disjointUnionOf(Class,Y,Schema),
		 query:classFormula(Schema,Y,Sol)),
	  Solns).
classFormula(Schema,Class,RType) :-
    restriction(Class,Schema),
    restrictionType(Class,RType,Schema).
classFormula(Schema,Class,class(Class)) :-
    class(Class,Schema),
    \+ restriction(Class,Schema),
    \+ hasFormula(Class,Schema).

:- rdf_meta propertyFrame(o,r,t).
propertyFrame(Schema,P,[type=objectProperty,property=P,
			range=R,
			frame=F,restriction=true]) :-
    mostSpecificRange(P,R,Schema),
    class(R,Schema),
    \+ entity(R,Schema),
    classFrame(R,Schema,F), !.
propertyFrame(Schema,P,[type=objectProperty,property=P,
			range=R,
			frame=[type=entity,class=R],restriction=true]) :-
    mostSpecificRange(P,R,Schema),
    class(R,Schema),
    entity(R,Schema), !.
propertyFrame(Schema,P,[type=datatypeProperty,property=P,
			range=R,restriction=true]) :-
    mostSpecificRange(P,R,Schema),
    datatype(R,Schema), !.
propertyFrame(_,P,[type=distortedFrame, property=P,
			message="The property has insufficient schema information"]).

disjointUnionFrames(Frames,[type=choice, frames=Frames]).

unionFrames(Frames,Frame) :-
    foldl(union,Frames,[],Frame).

intersectionProperty(R,S,[restriction=[type=and,operands=[T,U]]|R2]) :-
    member(Type,[datatypeProperty,objectProperty]),
    member(type=Type,R),
    member(type=Type,S),
    member(restriction=T,R),
    member(restriction=U,S),
    select(restriction=T,R,R2).
intersectionProperty(R,S,[restriction=[type=and,operands=[R,T]]|S2]) :-
    member(type=restriction,R),
    member(Type,[datatypeProperty,objectProperty]),
    member(type=Type,S),
    member(restriction=T,S),
    select(restriction=T,S,S2).
intersectionProperty(R,S,[restriction=[type=and,operands=[S,T]]|R2]) :-
    member(type=restriction,S),
    member(Type,[datatypeProperty,objectProperty]),
    member(type=Type,R),
    member(restriction=T,R),
    select(restriction=T,R,R2).

:- rdf_meta supProperty(r,r,r).
infimumProperty(PropA,PropB,Schema,PropA) :-
    strictSubsumptionPropertiesOf(PropA,PropB,Schema).
infimumProperty(PropA,PropB,Schema,PropB) :-
    strictSubsumptionPropertiesOf(PropB,PropA,Schema).
infimumProperty(PropA,PropA,_,PropA).

renameProperty(R,PropertyOld,PropertyNew,[property=PropertyNew|Rp]) :-
    select(property=PropertyOld,R,Rp).

intersectionFrame(_,FrameA,[],FrameA).
intersectionFrame(_,[],FrameB,FrameB).
intersectionFrame(Schema,[R|FrameA],FrameB,[T|FrameC]) :-
    member(property=PropertyA,R),
    member(S,FrameB),
    member(property=PropertyB,S),
    infimumProperty(PropertyA,PropertyB,Schema,PropertyC),
    renameProperty(R,PropertyA,PropertyC,Rp),
    renameProperty(S,PropertyB,PropertyC,Sp),
    intersectionProperty(Rp,Sp,T),
    select(S,FrameB,FrameB2) -> 
	intersectionFrame(Schema,FrameA,FrameB2,FrameC).
intersectionFrame(Schema,[R|FrameA],FrameB,[R|FrameC]) :-
    member(property=Property,R),
    member(S,FrameB),
    \+ member(property=Property,S) -> 
	intersectionFrame(Schema,FrameA,FrameB,FrameC).
	
intersectionFrames(Schema,[A|Frames],Frame) :-
    foldl(intersectionFrame(Schema),Frames,A,Frame).

:- rdf_meta classFrame(+,t,t,t).
traverseClassFormula(_,_,class('http://www.w3.org/2002/07/owl#Thing'),[type=thing]) :- !.
traverseClassFormula(Schema,Properties,class(C),Frame) :-
    classProperties(C,Schema,Properties2),
    union(Properties,Properties2,Properties3),
    maplist(propertyFrame(Schema),Properties3,Frame), !.
traverseClassFormula(Schema,Properties,C<D,Frame) :-
    classProperties(C,Schema,Properties2),
    union(Properties,Properties2,Properties3),
    traverseClassFormula(Schema,Properties3,D,Frame), !.
traverseClassFormula(Schema,Properties,C=xor(L),Frame) :-
    classProperties(C,Schema,Properties2),
    union(Properties,Properties2,Properties3),
    maplist(traverseClassFormula(Schema,Properties3),L,Frames),
    disjointUnionFrames(Frames,Frame), !.
traverseClassFormula(Schema,Properties,C=and(L),Frame) :-
    classProperties(C,Schema,Properties2),
    union(Properties,Properties2,Properties3),
    maplist(traverseClassFormula(Schema,Properties3),L,Frames),
    intersectionFrames(Schema,Frames,Frame), !.
traverseClassFormula(Schema,Properties,C=or(L),Frame) :-
    classProperties(C,Schema,Properties2),
    union(Properties,Properties2,Properties3),
    maplist(traverseClassFormula(Schema,Properties3),L,Frames),
    unionFrames(Frames,Frame) , !.
traverseClassFormula(Schema,Properties,restriction(L),Frame) :-
    maplist(propertyFrame(Schema),Properties,F),
    intersectionFrame(Schema,[[type=restriction|L]],F,Frame), !.
traverseClassFormula(_,Properties,F,[type=failure, message="mangled frame",
					 formula=F, properties=Properties]).
    
:- rdf_meta classFrame(r,o,t).
classFrame(Class, Schema, Frame) :-
    classFormula(Schema,Class,Formula) -> 
	traverseClassFormula(Schema, [], Formula, Frame)
    ; Frame = [].
