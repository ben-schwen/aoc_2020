:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(S), {term_string(L, S)}, lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

read_file(File, Input) :-
    once(phrase_from_file(lines(Input), File)).

task(N) :-
	read_file('input18', Input),
	op(N, yfx, *), 
	maplist(#=, Input, Xs), 
	sumlist(Xs, Sum),
	writeln(Sum).

main :- 
	task(501),
	task(500).