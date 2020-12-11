:- use_module(library(pio)).
:- use_module(library(clpfd)).

lines([])     --> call(eos_), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos_) ), !.
line([C|Cs]) --> [C], line(Cs).

eos_([], []).

read_file(File, Input) :-
	once(phrase_from_file(lines(Input), File)).

row_col_id(Row, Col, Id) :-
	Id #= (Row * 8) + Col,
	Row in 0..127,
	Col in 0..7.

seat_next(L0, U0, C, L, U) :-
	(C = 'F'; C = 'L'),
	L #= L0,
	U #= L0 + ((U0 - L0) // 2).
seat_next(L0, U0, C, L, U) :-
	(C = 'B'; C = 'R'),
	L #= U0 - ((U0 - L0) // 2),
	U #= U0.

seat(L, U, [], L, U).
seat(L0, U0, [C|Cs], L, U) :-
	seat_next(L0, U0, C, L1, U1),
	seat(L1, U1, Cs, L, U).

find_seat(L, U, Cs, X) :-
	seat(L, U, Cs, X, X),
	label([L, U]).

split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).
split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], N, [], []) :- N > 0.
split_at_([X|Xs], N, [X|Take], Rest) :-
    N > 0,
    succ(N0, N),
    split_at_(Xs, N0, Take, Rest).

description_id(Cs,Id) :-
	split_at(7, Cs, C_Rows, C_Cols),
	seat(0, 127, C_Rows, Row, Row),
	seat(0, 7,   C_Cols, Col, Col),
	row_col_id(Row, Col, Id).

main :-
	read_file('input5', Input), 
	maplist(maplist(char_code), Cs, Input),
	maplist(description_id, Cs, Ids),
	max_list(Ids, Max),
	writeln(Max),
	min_list(Ids, Min),
	numlist(Min, Max, L),
	subtract(L, Ids, Seat),
	writeln(Seat).
