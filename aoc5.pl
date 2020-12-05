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

list_maximum([L|Ls], M) :-
	foldl(maximum_, Ls, L, M).
maximum_(L, M0, M) :- M #= max(L,M0).

list_minimum([L|Ls], M) :-
	foldl(minimum_, Ls, L, M).
minimum_(L, M0, M) :- M #= min(L,M0).

seat(Lower, Upper, Code, L1, U1) :-
	(Code = 'F'; Code = 'L'),
	L1 #= Lower,
	U1 #= Lower + ((Upper - Lower) // 2).
seat(Lower, Upper, Code, L1, U1) :-
	(Code = 'B'; Code = 'R'),
	L1 #= Upper - ((Upper - Lower) // 2),
	U1 #= Upper.

/* modularised version for arbitrary big planes
find_row([], 0, 127).
find_row([C|Cs], L1, U1) :-
	seat(L, U, C, L1, U1),
	find_row(Cs, L, U).

find_col([], 0, 7).
find_col([C|Cs], L1, U1) :-
	seat(L, U, C, L1, U1),
	find_col(Cs, L, U).

% https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list
take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

description_id(Cs,Id) :-
	take(7, Cs, Row_C),
	reverse(Row_C, Rows),
	reverse(Cs, Rev_Cs),
	take(3, Rev_Cs, Cols),
	find_row(Rows, Row, Row),
	find_col(Cols, Col, Col),
	row_col_id(Row, Col, Id).*/

description_id([C1,C2,C3,C4,C5,C6,C7,C8,C9,C10],Id) :-
	seat(0, 127, C1, L1, U1),
	seat(L1, U1, C2, L2, U2),
	seat(L2, U2, C3, L3, U3),
	seat(L3, U3, C4, L4, U4),
	seat(L4, U4, C5, L5, U5),
	seat(L5, U5, C6, L6, U6),
	seat(L6, U6, C7, Row, Row),
	seat(0, 7, C8, L8, U8),
	seat(L8, U8, C9, L9, U9),
	seat(L9, U9, C10, Col, Col),
	row_col_id(Row, Col, Id).

main :-
	read_file('input5', Input), 
	maplist(maplist(char_code), Cs, Input),
	maplist(description_id, Cs, Ids),
	list_maximum(Ids, Max),
	writeln(Max),
	list_minimum(Ids, Min),
	numlist(Min, Max, L),
	subtract(L, Ids, Seat),
	writeln(Seat).
