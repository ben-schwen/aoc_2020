:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).
:- use_module(library(reif)).

mask([]) --> [].
mask([_|Xs]) --> "X", mask(Xs).
mask([X|Xs]) --> digit(C), {number_codes(X, [C])}, mask(Xs).
mem(I-V) --> "mem[", integer(I), "] = ", integer(V).
mems([X|Xs]) --> mem(X), ([],{Xs=[]} | "\n", mems(Xs)).
line(Mask-Mem) --> "mask = ", mask(Mask), "\n", mems(Mem).
lines([]) --> (eos | "\n" ), !.
lines([X|Xs]) --> line(X), (eos | "\n", lines(Xs)).

read_file(File, Input) :-
    once(phrase_from_file(lines(Input), File)).

% https://stackoverflow.com/questions/27788739/binary-to-decimal-prolog
binlist_dec(Bs0, N) :-
        reverse(Bs0, Bs),
        binlist_dec(Bs, 0, 0, N).
binlist_dec([], _, N, N).
binlist_dec([B|Bs], I0, N0, N) :-
        B in 0..1,
        N1 #= N0 + (2^I0)*B,
        I1 #= I0 + 1,
        binlist_dec(Bs, I1, N1, N).

number_code(N, C) :- number_codes(N, [C]).

dec_bin(Dec, Bin) :-
	N = 36,
	binlist_dec(Ns, Dec),
	length(Bin, N),
	maplist(=(0), Zeroes),
	append(Zeroes, Ns, Bin), 
	!.

mask_one(_, Mask, Mask) :-
	ground(Mask).
mask_one(Val, Mask, Val) :-
	var(Mask).

mask_all(Val, Mask, All) :-
	maplist(mask_one, Val, Mask, All).	

list_index_el(Ls, I, El) :- nth1(I, Ls, El).

mask_val_dec(Mask, Val, Dec) :-
	dec_bin(Val, Bin),
	mask_all(Bin, Mask, BDec),
	binlist_dec(BDec, Dec).

list_uniqueIndex(List, UI) :-
	must_be(list, List),
	lists:number_list(List, 1, Rev),
	reverse(Rev, Numbered),
    % fisrt sort by value in order to remove duplicates
    sort(1, @=<, Numbered, ONum),
    lists:remove_dup_keys(ONum, NumSet),
    pairs_values(NumSet, UI).

task1_(Mask-Mem, Is, Ds) :-	
	pairs_values(Mem, Vs),
	pairs_keys(Mem, Is),
	maplist(mask_val_dec(Mask), Vs, Ds).

main :-
	read_file('input14', Input),
	maplist(task1_, Input, Iss, Dss),
	flatten(Iss, Is),
	flatten(Dss, Ds),
	list_uniqueIndex(Is, Unique),
	maplist(list_index_el(Ds), Unique, Ss),
	sumlist(Ss, Sum),
	writeln(Sum).
