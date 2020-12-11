:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

lines([]) --> (eos | "\n\n"), !.
lines([X|Xs]) --> integer(X), "\n", lines(Xs).

read_file(File, Input) :-
	once(phrase_from_file(lines(Input), File)).

sublist([], []).
sublist(S, [_|L]) :-
    sublist(S, L).
sublist(S, L) :-
    append(S, _, L).

sum(X, Xs) :-
	member(A, Xs),
	member(B, Xs),
	dif(A,B),
	X #= A + B,
	!.

next(Xs, N, Z) :-
	sublist(Ys, Xs),
	length(Ys, N),
	append(Zs, [Z], Ys),
	\+ sum(Z, Zs).

next2(Xs, X, Min_Max_Sum) :-
	sublist(S, Xs),
	length(S, N),
	N #>= 2,
	sum(S, #=, X),
	min_list(S, Min),
	max_list(S, Max),
	Min_Max_Sum #= Min + Max.

main :-
	read_file('input9', Input),
	next(Input, 26, Z),
	writeln(Z),
	next2(Input, Z, MM),
	writeln(MM).