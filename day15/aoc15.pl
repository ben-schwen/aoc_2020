split([], []).
split([X|Xs], [[X]|Xss]) :-
	split(Xs, Xss).

db(_, [], []).
db(T, [K|Ks], [V|Vs]) :-
	trie_insert(T, K, V),
	db(T, Ks, Vs).

next(N, (T, Last), (T, 0)) :-
	trie_lookup(T, Last, Val),
	Val = [_],
	trie_lookup(T, 0, Zeroes),
	last(Zeroes, Zero),
	trie_update(T, 0, [Zero, N]).

next(N, (T, Last), (T, 0)) :-
	trie_lookup(T, Last, Val),
	Val = [_],
	\+ trie_lookup(T, 0, _),
	trie_update(T, 0, [N]).

next(N, (T, Last), (T, V)) :-
	trie_lookup(T, Last, Val),
	Val = [V1, V2],
	V is V2-V1,
	trie_lookup(T, V, Index),
	last(Index, I),
	trie_update(T, V, [I, N]).

next(N, (T, Last), (T, V)) :-
	trie_lookup(T, Last, Val),
	Val = [V1, V2],
	V is V2-V1,
	\+ trie_lookup(T, V, _),
	trie_update(T, V, [N]).

task(L1, End) :-	
	length(L1, Len),
	numlist(1, Len, L2x),
	split(L2x, L2), 
	Start is Len+1,
	numlist(Start, End, L), 
	last(L1, Last),
	trie_new(T0), 
	db(T0, L1, L2), 
	foldl(next, L, (T0, Last), (_, V)),
	writeln(V).

:- set_prolog_flag(stack_limit, 32_000_000_000).

main :-
	%task([18,11,9,0,5,1], 2020),
	task([18,11,9,0,5,1], 30000000).