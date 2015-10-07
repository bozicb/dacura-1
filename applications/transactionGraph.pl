:- module(transactionGraph,[xrdf/4, insert/4, update/5, delete/4, commit/1, rollback/1]).

:- use_module(library(semweb/rdf_db), except([rdf/4])). 

% How to implement post-condition transactions

neg(schema, 'neg-schema') :- !.
neg(instance, 'neg-instance') :- !.
neg(A1,A2) :- atom_concat('neg-', A1, A2).

pos(schema, 'pos-schema') :- !.
pos(instance, 'pos-instance') :- !.
pos(A1,A2) :- atom_concat('pos-', A1, A2).

:- rdf_meta xrdf(r,r,t,?).
xrdf(X,Y,Z,G) :- (pos(G,GP), rdf_db:rdf(X,Y,Z,GP)    % If in positive graph, return results
                  *-> true                           % 
		  ; (neg(G,GN), rdf_db:rdf(X,Y,Z,GN) % If it's not negative
	             *-> false                       % If it *is* negative, fail
		     ; rdf_db:rdf(X,Y,Z,G))).        % otherwise bind the rdf result.

% you can safely ignore rdf_meta for understanding this programme
% it only affects namespace prefix handling.
:- rdf_meta insert(r,r,t,?).
insert(X,Y,Z,G) :- 
    pos(G,G2),
    % positive pos graph
    rdf_assert(X,Y,Z,G2), 
    % retract from the negative graph, if it exists.
    (neg(G,G3), rdf_db:rdf(X,Y,Z,G3), rdf_retractall(X,Y,Z,G3) ; true).

:- rdf_meta delete(r,r,t,?).
delete(X,Y,Z,G) :- 
    pos(G,G2), % delete from pos graph
    rdf_db:rdf(X,Y,Z,G2),
    rdf_retractall(X,Y,Z,G2), 
    false.
delete(X,Y,Z,G) :- % assert negative
    rdf_db:rdf(X,Y,Z,G),
    neg(G, G2),
    % write('Asserting negative fact: '),
    % write([X,Y,Z,G2]),nl,
    rdf_assert(X,Y,Z,G2).	

new_triple(_,Y,Z,subject(X2),X2,Y,Z).
new_triple(X,_,Z,predicate(Y2),X,Y2,Z).
new_triple(X,Y,_,object(Z2),X,Y,Z2).

:- rdf_meta update(r,r,o,o,?).
update(X,Y,Z,G,_) :-  
    rdf_db:rdf(X,Y,Z,G), 
    neg(G,G3),
    rdf_assert(X,Y,Z,G3), fail. % delete previous subject if it exists and try update
update(X,Y,Z,G,Action) :- 
    pos(G,G2),
    (rdf_db:rdf(X,Y,Z,G2) -> rdf_update(X,Y,Z,G2,Action) % exists in pos graph 
     ; new_triple(X,Y,Z,Action,X2,Y2,Z2),
       rdf_assert(X2,Y2,Z2,G2)). % doesn't yet exist in pos graph (insert). 

commit(G) :-
    pos(G,GP), 
    findall([XP,YP,ZP,GP], rdf_db:rdf(XP,YP,ZP,GP), LP),
    maplist(insert_positive, LP),
    rdf_retractall(_,_,_,GP), !,
    neg(G,GN),
    % write("Negative Graph: "),
    % write(GN),nl,
    findall([XN,YN,ZN,GN], rdf_db:rdf(XN,YN,ZN,GN), LN),
    % write('Retracting: '), nl,
    % write(LN),
    maplist(retract_negative, LN),
    rdf_retractall(_,_,_,GN), !.

rollback(G) :-
    pos(G,GP), 
    rdf_retractall(_,_,_,GP), !,
    neg(G,GN), 
    rdf_retractall(_,_,_,GN), !.

insert_positive([X,Y,Z,GP]) :-
    pos(G,GP),
    rdf_db:rdf_assert(X,Y,Z,G).
    
retract_negative([X,Y,Z,GN]) :- 
    neg(G,GN), 
    rdf_retractall(X,Y,Z,G).

