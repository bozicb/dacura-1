:- module(utils,[getKey/4, count/3, path_end/2, render/2, convert_triples/2, literal_convert/2]).

% convenience functions
getKey(Key,Assoc,Val,Default) :- 
    member(Key=Val, Assoc) *-> 
	  true 
    ; Val = Default.

count(_,[], 0).
count(A,[B|L],C) :- count(A,L,K), (A=B -> C is K+1 ; C=K).

path_end(P,E) :- 
    atom_string(P,S), 
    split_string(S, "/", "", L), 
    last(L,ES), 
    atom_string(E,ES).

% Takes an element of unknown type and renders it as an atom 
% suitable for reporting.
render(X,R) :- 
    write_to_chars(X, Y), 
    atom_string(R, Y).

convert_triples([],[]). 
convert_triples([[X1,Y1,Z1,G]|T1], [[X2,Y2,Z2,G]|T2]) :-
    literal_convert(X1,X2),
    literal_convert(Y1,Y2),
    literal_convert(Z1,Z2),
    convert_triples(T1,T2).

literal_convert(X, Y) :-
    (json([A,B]) = X,
     ((Y = literal(type(Type,Data)),
       member(type=Type, [A,B]),
       member(data=Data, [A,B]))
      ; (Y = literal(lang(Lang,Data)),
	 member(lang=Lang, [A,B]),
	 member(data=Data, [A,B]))))
    ; X = Y.
%% 
%%     (json(Assoc) = X *-> 
%% 		     (member(type=Type, Assoc) *->
%% 			    (member(data=Data, Assoc),
%% 			     Y = literal(type(Type,Data)))
%% 		      ; member(lang=Lang, Assoc) *->
%% 			      (member(data=Data, Assoc),
%% 			       Y = literal(lang(Data, Lang)))
%% 		      ; X = Y)
%%      ; X = Y),
%%     literal_convert(T1,T2).
       
	     

