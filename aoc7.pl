:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

%%%===================================================================
%%% DCG for parsing
%%%===================================================================

lines([])     --> (eos | "\n\n"), !.
lines([X|Xs]) --> line(X), "\n", lines(Xs).

line(bag(B, B2)) --> 
	nonblanks(Adj), white, nonblanks(Col),
	{append(Adj,[95|Col], Chars), atom_chars(B, Chars)}, 
	" bags contain ", contains(B2).

contains([[],0]) --> "no other bags.".
contains([[X,I]]) --> integer(I), white, nonblanks(Adj), white, nonblanks(Col), 
	{append(Adj,[95|Col], Chars), atom_chars(X, Chars)}, 
	white, bag, ".".
contains([[X,I]|Xs]) --> integer(I), white, nonblanks(Adj), white, nonblanks(Col), 
	{append(Adj,[95|Col], Chars), atom_chars(X, Chars)}, 
	white, bag, ",", white, contains(Xs).

bag --> "bag".
bag --> "bags".

%%%===================================================================
%%% Predicates
%%%===================================================================

read_file(File, Input) :-
	once(phrase_from_file(lines(Input), File)).

member_bag_(X, B, Bags) :-
	member(Bag, Bags),
	Bag = bag(B, C),
	maplist(nth0(0), C, CBags),
	member(X, CBags).

member_bag(X, B, Bags) :-
	member_bag_(X, B, Bags).
member_bag(X, Z, Bags) :-
	member_bag_(X, Y, Bags),
	member_bag(Y, Z, Bags).

mult(X, Y, Z) :- Z #= X * Y.
sum_prod(A, B, SumProd) :-
    maplist(mult, A, B, Prods),
    sumlist(Prods, SumProd).

inside(Bags, X, 0) :-
	member(Bag, Bags), 
	Bag = bag(X, [[],0]).
inside(Bags, X, Y) :-
	member(Bag, Bags),
	Bag = bag(X, Inside),
	maplist(nth0(0), Inside, Inside_Bags),
	maplist(nth0(1), Inside, Inside_Counts),
	maplist(inside(Bags), Inside_Bags, IB_Counts),
	sum_prod(Inside_Counts, IB_Counts, SumProd),
	sumlist(Inside_Counts, SumInside),
	Y #= SumProd + SumInside.

main :- 
	read_file('input7', Bags), 
	setof(B, member_bag(shiny_gold, B, Bags), Shiny), 
	length(Shiny, N),
	writeln(N),
	inside(Bags, shiny_gold, NI),
	writeln(NI).

println([]).
println([X|Xs]) :-
	writeln(X),
	println(Xs).