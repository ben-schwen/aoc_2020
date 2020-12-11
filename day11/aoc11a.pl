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
    nth1(Col, RL, Index),
    (Index =:= 1 -> State=1 ; State=0),
    !.
state(_, _, _, 0).

neigh_active(Grid, Row, Col, Sum) :-
	XM1 is Col - 1,
    XP1 is Col + 1,
    YM1 is Row - 1,
    YP1 is Row + 1,
    state(Grid, YM1, Col, N),
    state(Grid, YM1, XP1, NE),
    state(Grid, Row, XP1, E),
    state(Grid, YP1, XP1, SE),
    state(Grid, YP1, Col, S),
    state(Grid, YP1, XM1, SW),
    state(Grid, Row, XM1, W),
    state(Grid, YM1, XM1, NW),
    Sum is N + NE + E + SE + S + SW + W + NW.

% seat is currently empty
next_state(Grid, Row, Col, 0, NS) :-
    neigh_active(Grid, Row, Col, Total),
    (Total =:= 0 -> NS = 1 ; NS = 0).
% seat is currently taken
next_state(Grid, Row, Col, 1, NS) :-
    neigh_active(Grid, Row, Col, Total),
    (Total >= 4 -> NS = 0 ; NS = 1).

% floor stays floor
formstate(Grid, Row, Col, 9) :-
	nth1(Row, Grid, RL),
    nth1(Col, RL, 9),
    !.
formstate(Grid, Row, Col, NS) :-
    state(Grid, Row, Col, CS),
    next_state(Grid, Row, Col, CS, NS).

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
    %println(Grid),
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
