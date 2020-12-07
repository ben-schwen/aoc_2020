:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- set_prolog_flag(double_quotes, codes).

%%%===================================================================
%%% DCG for parsing
%%%===================================================================

lines([])     --> (eos | "\n\n"), !.
lines([X|Xs]) --> line(X), "\n", lines(Xs).

line(bag(B, C, B2)) --> 
	nonblanks(Adj), white, nonblanks(Col),
	{append(Adj,[32|Col], B)}, 
	" bags contain ", contains([C, B2]).

contains([[I|Is],[X|Xs]]) --> digit(I), white, nonblanks(Adj), white, nonblanks(Col), 
	{append(Adj,[32|Col], X)}, white, bag, ",", white, contains([Is, Xs]).
contains([[0],[]]) --> "no other bags.".
contains([[I],[X]]) --> digit(I), white, nonblanks(Adj), white, nonblanks(Col), 
	{append(Adj,[32|Col], X)}, white, bag, ".".

bag --> "bag".
bag --> "bags".

%%%===================================================================
%%% Predicates
%%%===================================================================

read_file(File, Input) :-
	once(phrase_from_file(lines(Input), File)).

member_bag_(X, B, Bags) :-
	member(Bag, Bags),
	Bag = bag(B, _, C),
	member(X, C).

member_bag(X, B, Bags) :-
	member_bag_(X, B, Bags).
member_bag(X, Z, Bags) :-
	member_bag_(X, Y, Bags),
	member_bag(Y, Z, Bags).

main :- 
	read_file('input7', Bags), 
	findall(B, member_bag("shiny gold", B, Bags), Shiny), 
	maplist(string_chars, S, Shiny), 
	list_to_set(S, Set), 
	length(Set, N),
	writeln(N).


bag_string(bag(B, _, C), S) :-
	flatten(C, F),
	append(B, [58|F], BC),
	string_chars(S, BC).

test0 :- 
	S = "2 vibrant aqua bags.",
	phrase(contains(B), S), 
	maplist(string_chars, Bs, B),
	writeln(Bs).

test1 :- 
	S = "no other bags.",
	phrase(contains(B), S), 
	maplist(string_chars, Bs, B),
	writeln(Bs).

test2 :-
	S = "5 dark green bags, 5 light gray bags, 3 faded indigo bags, 2 vibrant aqua bags.", 
	phrase(contains(B), S), 
	maplist(string_chars, Bs, B),
	writeln(Bs).

test3 :-
	S = "light beige bags contain 5 dark green bags, 5 light gray bags, 3 faded indigo bags, 2 vibrant aqua bags.",
	phrase(line(B), S),
	bag_string(B, Bs),
	writeln(Bs).

println([]).
println([X|Xs]) :-
	writeln(X),
	println(Xs).