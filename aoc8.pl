:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).

lines([]) --> (eos | "\n\n"), !.
lines([X|Xs]) --> line(X), "\n", lines(Xs).

line(operator(Op, Arg1)) --> nonblanks(Arg0), {atom_chars(Op, Arg0)}, white, integer(Arg1).

read_file(File, Input) :-
	once(phrase_from_file(lines(Input), File)).

zip([], [], []).
zip([X|Xs], [Y|Ys], [X-Y|Zs]) :- zip(Xs,Ys,Zs).

list_avl(List, AVL) :-
	length(List, N),
	numlist(1, N, Keys),
	zip(Keys, List, Pair_list),
	list_to_assoc(Pair_list, AVL).

do_op(Op, Prev_Index, Prev_Acc, Index, Acc) :-
	Op = operator(nop, _),
	Index #= Prev_Index + 1,
	Acc #= Prev_Acc.
do_op(Op, Prev_Index, Prev_Acc, Index, Acc) :-
	Op = operator(jmp, A),
	Index #= Prev_Index + A,
	Acc #= Prev_Acc.
do_op(Op, Prev_Index, Prev_Acc, Index, Acc) :-
	Op = operator(acc, A),
	Index #= Prev_Index + 1,
	Acc #= Prev_Acc + A.

parse(_, 1, 0, []).
parse(Ops, Index, Acc, [I|V]) :-
	parse(Ops, I, A, V),
	get_assoc(I, Ops, Op),
	do_op(Op, I, A, Index, Acc).

parse_all(Ops, Index, Acc, V) :-
	parse(Ops, Index, Acc, V),
	member(Index, V),
	!.

change_op(Xs, Ys) :-
	X = operator(nop, A),
	Y = operator(jmp, A),
	member(X, Xs),
	select(X, Xs, Y, Ys),
	writeln([X,Y]).

change_op(Xs, Ys) :-
	X = operator(jmp, A),
	Y = operator(nop, A),
	member(X, Xs),
	select(X, Xs, Y, Ys),
	writeln([X,Y]).

find_problem :-
	read_file('input8', Xs),
	change_op(Xs, Ys),
	list_avl(Ys, AVL),
	call_with_time_limit(5, \+ parse_all(AVL, _, _, _)).

main :- 
	read_file('input8', A),
	list_avl(A, AVL),
	parse_all(AVL, _, Acc, _),
	writeln(Acc),
	read_file('input8b', B),
	list_avl(B, BVL),
	parse(BVL, 639, Acc2, _),
	!,
	writeln(Acc2).

