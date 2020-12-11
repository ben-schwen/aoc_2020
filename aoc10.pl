:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

lines([]) --> (eos | "\n\n"), !.
lines([X|Xs]) --> integer(X), "\n", lines(Xs).

read_file(File, Input) :-
    once(phrase_from_file(lines(Input), File)).

one(1).
three(3).

minus(X,Y,Z) :- Z is X-Y.

sublist(List, Start, End, Sub) :-
    length(List, Len), Start >= 0, End =< Len,
    SubLen is End - Start,
    append(T1, Sub, T2),
    length(T1, Start), length(Sub, SubLen),
    append(T2, _, List).

task1(N) :-
	read_file('input10', L),
    sort(L, L1), 
    length(L1, Len),
    sub_list([0|L1], 0, Len, L2),
    maplist(minus, L1, L2, Diff),
    include(one, Diff, Ones), 
    length(Ones, N1),
    include(three, Diff, Threes), 
    length(Threes, N3),
    N is (N1) * (N3 + 1).

ways(0, _, 1).
ways(Key, HT, 0) :- \+ _ = HT.get(Key).
ways(Key, HT, Val) :- Val = HT.get(Key).

ways_rec(Key, HT, HT2) :-
    Key1 is Key - 1, ways(Key1, HT, Val1),
    Key2 is Key - 2, ways(Key2, HT, Val2),
    Key3 is Key - 3, ways(Key3, HT, Val3),
    Val is Val1 + Val2 + Val3,
    HT2 = HT.put([Key = Val]).

task2(N) :-
	read_file('input10', L),
    sort(L, Sorted), 
    max_list(Sorted, Max),
    foldl(ways_rec, Sorted, ht{}, HT),
    N = HT.get(Max).