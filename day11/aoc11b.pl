:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

lines([]) --> (eos | "\n\n"), !.
lines([X|Xs]) --> line(X), "\n", lines(Xs).

line([]) --> [].
line([X|Xs]) --> char(X), line(Xs).

char(0) --> "L".
char(9) --> ".".
char(1) --> "#".

read_file(File, Input) :-
    once(phrase_from_file(lines(Input), File)).

state(Grid, Row, Col, State) :-
	length(Grid, Row_Dim),
	Grid = [X|_],
	length(X, Col_Dim),
    between(1, Row_Dim, Row),
    between(1, Col_Dim, Col),
    nth1(Row, Grid, RL),
    nth1(Col, RL, State).
/*state(_, _, _, 0).*/

look(_, n, Row, _, 0) :- Row=1, !.
look(Grid, n, Row1, Col, 0) :- Row is Row1-1, state(Grid, Row, Col, 0), !.
look(Grid, n, Row1, Col, S) :- Row is Row1-1, state(Grid, Row, Col, 9), look(Grid, n, Row, Col, S), !.

look(Grid, s, Row, _, 0) :- length(Grid,Row), !.
look(Grid, s, Row1, Col, 0) :- Row is Row1+1, state(Grid, Row, Col, 0), !.
look(Grid, s, Row1, Col, S) :- Row is Row1+1, state(Grid, Row, Col, 9), look(Grid, s, Row, Col, S), !.

look(_, e, _, Col, 0) :- Col=1, !.
look(Grid, e, Row, Col1, 0) :- Col is Col1-1, state(Grid, Row, Col, 0), !.
look(Grid, e, Row, Col1, S) :- Col is Col1-1, state(Grid, Row, Col, 9), look(Grid, e, Row, Col, S), !.

look(Grid, w, _, Col, 0) :- Grid=[X|_], length(X,Col), !.
look(Grid, w, Row, Col1, 0) :- Col is Col1+1, state(Grid, Row, Col, 0), !.
look(Grid, w, Row, Col1, S) :- Col is Col1+1, state(Grid, Row, Col, 9), look(Grid, w, Row, Col, S), !.

look(_, ne, Row, _, 0) :- Row=1, !.
look(_, ne, _, Col, 0) :- Col=1, !.
look(Grid, ne, Row1, Col1, 0) :- Row is Row1-1, Col is Col1-1, state(Grid, Row, Col, 0), !.
look(Grid, ne, Row1, Col1, S) :- Row is Row1-1, Col is Col1-1, state(Grid, Row, Col, 9), look(Grid, ne, Row, Col, S), !.

look(_, nw, Row, _, 0) :- Row=1, !.
look(Grid, nw, _, Col, 0) :- Grid=[X|_], length(X,Col), !.
look(Grid, nw, Row1, Col1, 0) :- Row is Row1-1, Col is Col1+1, state(Grid, Row, Col, 0), !.
look(Grid, nw, Row1, Col1, S) :- Row is Row1-1, Col is Col1+1, state(Grid, Row, Col, 9), look(Grid, nw, Row, Col, S), !.

look(Grid, se, Row, _, 0) :- length(Grid,Row), !.
look(_, se, _, Col, 0) :- Col=1, !.
look(Grid, se, Row1, Col1, 0) :- Row is Row1+1, Col is Col1-1, state(Grid, Row, Col, 0), !.
look(Grid, se, Row1, Col1, S) :- Row is Row1+1, Col is Col1-1, state(Grid, Row, Col, 9), look(Grid, se, Row, Col, S), !.

look(Grid, sw, Row, _, 0) :- length(Grid,Row), !.
look(Grid, sw, _, Col, 0) :- Grid=[X|_], length(X,Col), !.
look(Grid, sw, Row1, Col1, 0) :- Row is Row1+1, Col is Col1+1, state(Grid, Row, Col, 0), !.
look(Grid, sw, Row1, Col1, S) :- Row is Row1+1, Col is Col1+1, state(Grid, Row, Col, 9), look(Grid, sw, Row, Col, S), !.

look(_, _, _, _, 1).

neigh_active(Grid, Row, Col, Sum) :-
    look(Grid, n , Row, Col, N),
    look(Grid, ne, Row, Col, NE),
    look(Grid, e , Row, Col, E),
    look(Grid, se, Row, Col, SE),
    look(Grid, s , Row, Col, S),
    look(Grid, sw, Row, Col, SW),
    look(Grid, w , Row, Col, W),
    look(Grid, nw, Row, Col, NW),
    Sum is N + NE + E + SE + S + SW + W + NW.

% seat is currently empty
next_state(Grid, Row, Col, 0, NS) :-
    neigh_active(Grid, Row, Col, Total),
    (Total =:= 0 -> NS = 1 ; NS = 0).
% seat is currently taken
next_state(Grid, Row, Col, 1, NS) :-
    neigh_active(Grid, Row, Col, Total),
    (Total >= 5 -> NS = 0 ; NS = 1).

% floor stays floor
formstate(Grid, Row, Col, 9) :-
	nth1(Row, Grid, RL),
    nth1(Col, RL, 9),
    !.
formstate(Grid, Row, Col, NS) :-
    state(Grid, Row, Col, CS),
    next_state(Grid, Row, Col, CS, NS),
    !.

formrow(Grid, Row, NS) :-
	Grid = [X|_],
	length(X, Dim),
	numlist(1, Dim, Col),
	maplist(formstate(Grid, Row), Col, NS).

gen(Grid, NewGrid) :-
	length(Grid, Dim),
	numlist(1, Dim, Row),
	maplist(formrow(Grid), Row, NewGrid).

same([], []).
same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).

same_mat(M1, M2) :-
	flatten(M1, F1),
	flatten(M2, F2),
	same(F1, F2).

occupied(1).

life(Grid) :-
    flatten(Grid, Flat),
    include(occupied, Flat, Occ), length(Occ, Len), writeln(Len),
    gen(Grid, NewGrid),
    \+ same_mat(Grid, NewGrid),
    life(NewGrid).

main :-
	read_file('input11', Grid), life(Grid).

println([]).
println([X|Xs]) :-
	writeln(X),
	println(Xs).
