:- use_module(library(pio)).

lines([])     --> call(eos_), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n\n" | call(eos_) ), !.
line([C|Cs]) --> [C], line(Cs).

eos_([], []).

read_file(File, Input) :-
	once(phrase_from_file(lines(Input), File)).

% https://github.com/mndrix/list_util/blob/master/prolog/list_util.pl
split([], _, [[]]) :- !.  % optimization
split([Div|T], Div, [[]|Rest]) :-
    split(T, Div, Rest),  % implies: dif(Rest, [])
    !.
split([H|T], Div, [[H|First]|Rest]) :-
    split(T, Div, [First|Rest]).

split_(Sep, L1, L2) :-
	split(L1, Sep, L2).

cup(Xss, Union) :-
	foldl(union, Xss, [], Union).

cap(Xss, Intersection) :-
	foldl(intersection, Xss, _, Intersection).

llist_UniqueN(Xss, N) :-
	maplist(list_to_set, Xss, Set),
	maplist(length, Set, Len),
	sum_list(Len, N).

main :-
	read_file('input6', Raw), 
	maplist(split_(10), Raw, Input), 
	maplist(cup, Input, Union),
	llist_UniqueN(Union, UN),
	writeln(UN),
	maplist(cap, Input, Intersection),
	llist_UniqueN(Intersection, IN),
	writeln(IN).