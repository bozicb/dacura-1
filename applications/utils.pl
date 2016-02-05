:- module(utils,[
	         getKey/4, count/3, path_end/2, render/2, convert_quads/2, json_to_literal/2,
		 interpolate/2, uniqueSolns/3,
		 fixup_literals/2, jsonify/2,
		 % Meta-programming utils
		 converse/4,
		 lambda/3, lambda/5, lambda/7
		]).

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

fixup_literals(H,J) :- jsonify(H,J).
/* DDD Test using jsonify....
fixup_literals([],[]).
fixup_literals(json(L1),json(L2)) :-
    fixup_literals(L1,L2).
fixup_literals([json(H1)|T1], [json(H2)|T2]) :-
    fixup_literals(H1,H2),
    fixup_literals(T1,T2).
fixup_literals([X=Y1|T1],[X=Y2|T2]) :- 
    json_to_literal(Y2,Y1),
    fixup_literals(T1,T2).
*/

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

%:- meta uniqueSolns
uniqueSolns(Template,Predicate,Collection) :-
    (setof(Template, Predicate, CollectionX)
     -> Collection=CollectionX
     ; Collection=[]).


% convert lists into json
jsonify(literal(type(Type,Data)),json([type=Type,data=Data])) :- !.
jsonify(literal(type(Type,Data)),json([data=Data,type=Type])) :- !.
jsonify(literal(lang(Lang,Data)),json([lang=Lang,data=Data])) :- !.
jsonify(literal(lang(Lang,Data)),json([data=Data,lang=Lang])) :- !.
jsonify(A,A) :-
    (atom(A) ; number(A)), !.
jsonify(A=B,A=Bp) :-
    jsonify(B,Bp).
jsonify(L,Lp) :-
    is_list(L),
    \+ member(_=_,L), % is a simple array
    maplist(jsonify,L,Lp).
jsonify(L,json(Lp)) :-
    is_list(L),
    \+ \+ member(_=_,L), % is a valid object
    maplist(jsonify,L,Lp).

%%% Meta-programming

% Flip order of args
converse(F, X, Y, FYX) :-
    call(F, Y, X, FYX).

lambda( A1, GoalTemplate, X1 ) :-
  copy_term( A1-GoalTemplate, X1-Goal ),
  call( Goal ).

lambda( A1, A2, GoalTemplate, X1, X2 ) :-
  copy_term( A1-A2-GoalTemplate, X1-X2-Goal ),
  call( Goal ).

lambda( A1, A2, A3, GoalTemplate, X1, X2, X3) :-
  copy_term( A1-A2-A3-GoalTemplate, X1-X2-X3-Goal ),
  call( Goal ).
