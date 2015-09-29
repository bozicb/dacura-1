:- module(utils,[getKey/4, count/3, path_end/2, render/2, convert_quads/2, json_to_literal/2]).

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

:- use_module(library(http/http_log)).

json_to_literal(json([type=Type,data=Data]),literal(type(Type,Data))) :- !.
json_to_literal(json([data=Data,type=Type]),literal(type(Type,Data))) :- !.
json_to_literal(json([lang=Lang,data=Data]),literal(lang(Lang,Data))) :- !.
json_to_literal(json([data=Data,lang=Lang]),literal(lang(Lang,Data))) :- !.
json_to_literal(X,X).

convert_quads([],[]).
convert_quads([[X1,Y1,Z1,G]|T1], [[X2,Y2,Z2,G]|T2]) :-
%    http_log_stream(Log),	
%    write(Log,'['),
%    write_canonical(Log,X1), write(Log,','),
%    write_canonical(Log,Y1), write(Log,','),
%    write_canonical(Log,Z1), write(Log,','),
%    write(Log,G),
%    write(Log,']\n'),
    json_to_literal(X1,X2),
    json_to_literal(Y1,Y2),
    json_to_literal(Z1,Z2),
%    write(Log,'['),
%    write_canonical(Log,X2), write(Log,','),
%    write_canonical(Log,Y2), write(Log,','),
%    write_canonical(Log,Z2), write(Log,','),
%    write(Log,G),
%    write(Log,']\n'),
    convert_quads(T1,T2).

%errorProcedure :-
%    throw(basalt("Foo fluppers!")).

%testError(errorProcedure).

interpolate([],'').
interpolate([H|T],S) :-
    atom(H),
    interpolate(T,Rest),
    atom_concat(H,Rest,S) , !.
interpolate([H|T],S) :-
    string(H), atom_string(C,H),
    interpolate(T,Rest),
    atom_concat(C,Rest,S) , !.
interpolate([H|T],S) :- 
    ground(H), term_to_atom(H,C),
    interpolate(T,Rest) ,
    atom_concat(C,Rest,S) , !.
