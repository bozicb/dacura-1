:- module(utils,[getKey/4, count/3, path_end/2, render/2]).

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
