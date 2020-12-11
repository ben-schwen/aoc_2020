
:- use_module(library(clpfd)).
:- use_module(library(readutil)).
:- use_module(library(aggregate)).

read_lines(Stream, []) :-
	at_end_of_stream(Stream).
read_lines(Stream, [Line|Rest]) :-
	\+ at_end_of_stream(Stream),
	read_line_to_codes(Stream, Line),
	read_lines(Stream, Rest).

count(W, S, N) :-
	atom_chars(S, L),
    aggregate(count, member(W, L), N).

input_indexes(I, X1, X2, X3) :-
	split_string(I, " ", [], Xs),
	nth0(0, Xs, X1),
	nth0(1, Xs, X2),
	nth0(2, Xs, X3).

index2_atom(S, A) :-
	atom_chars(S, L),
	nth0(0, L, A). 

index1_bounds(X1, Lower, Upper) :-
	split_string(X1, "-", [], Xs),
	nth0(0, Xs, L),
	number_string(Lower, L),
	nth0(1, Xs, U),
	number_string(Upper, U).

bounds(L, U, X) :-
	X #>= L,
	X #=< U.

part1(I) :-
	input_indexes(I, X1, X2, X3),
	index2_atom(X2, A),
	index1_bounds(X1, L, U),
	count(A, X3, N),
	bounds(L, U, N).

part2(I) :-
	input_indexes(I, X1, X2, X3),
	index2_atom(X2, A),
	index1_bounds(X1, L, U),
	atom_chars(X3, Ws),
	nth1(L, Ws, C1),
	nth1(U, Ws, C2),
	((A = C1) ; (A = C2)),
	dif(C1, C2).

main :- 
	read_file_to_string('input2', Raw, []), 
	split_string(Raw, '\n', [], Strings), 
	include(part1, Strings, Valid1), 
	length(Valid1, Valid_length1),
	write(Valid_length1), nl,
	include(part2, Strings, Valid2),
	length(Valid2, Valid_length2),
	write(Valid_length2), nl.

