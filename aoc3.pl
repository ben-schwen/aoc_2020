:- use_module(library(clpfd)).

matrix_max([M|Ms], MAX_I, MAX_J) :-
	length([M|Ms], MAX_I),
	length(M, MAX_J).

index(M, [I,J]) :-
	nth1(I, M, Row),
	nth1(J, Row, X),
	X = '#'.

create_index(Right, Down, MAX_I, MAX_J, I, J) :-
	X #> 0,
	I #= 1 + Down * X,
	J #= (((1 + Right * X ) - 1) mod MAX_J) + 1,
	I in 1..MAX_I,
	J in 1..MAX_J.

all_index(Right, Down, MAX_I, MAX_J, Indices) :-
	findall([I,J], (create_index(Right, Down, MAX_I, MAX_J, I, J), label([I, J])), Indices).

count_index(M, Indices, N) :-
	include(index(M), Indices, Valid),
	length(Valid, N).

main :-
	read_file_to_string('input3', Raw, []), 
	split_string(Raw, '\n', [], Lines), 
	maplist(atom_chars, Lines, M),
	matrix_max(M, MAX_I, MAX_J),
	all_index(3, 1, MAX_I, MAX_J, I2),
	count_index(M, I2, N2),
	write(N2), nl,
	all_index(1, 1, MAX_I, MAX_J, I1),
	all_index(5, 1, MAX_I, MAX_J, I3),
	all_index(7, 1, MAX_I, MAX_J, I4),
	all_index(1, 2, MAX_I, MAX_J, I5),
	count_index(M, I1, N1),
	count_index(M, I3, N3),
	count_index(M, I4, N4),
	count_index(M, I5, N5),
	Nprod #= N1 * N2 * N3 * N4 * N5,
	write(Nprod), nl.
