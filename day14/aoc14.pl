:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).
:- use_module(library(yall)).

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

dec_bin(Dec, Bin) :-
	N = 36,
	binlist_dec(Ns, Dec),
	length(Bin, N),
	maplist(=(0), Zeroes),
	append(Zeroes, Ns, Bin), 
	!.

mask1(_, Mask, Mask) :- ground(Mask).
mask1(Val, Mask, Val) :- var(Mask).

mask_all(Val, Mask, All) :- maplist(mask1, Val, Mask, All).	

mask_val_dec(Mask, Val, Dec) :-
	dec_bin(Val, Bin),
	mask_all(Bin, Mask, BDec),
	binlist_dec(BDec, Dec).

task1_(Mask-Mem, DictIn, DictOut) :-	
	pairs_values(Mem, Vs),
	pairs_keys(Mem, Is),
	maplist(mask_val_dec(Mask), Vs, Ds),
	pairs_keys_values(Ps, Is, Ds),
	put_dict(Ps, DictIn, DictOut).

task1 :-
	read_file('input14', Input), 
	foldl(task1_, Input, ht{}, HT), 
	dict_keys(HT, Keys), 
	maplist({HT}/[Key,Val]>>get_dict(Key,HT,Val), Keys, Vals),
	sumlist(Vals, Sum),
	writeln(Sum).

mask2(_, Mask, Mask) :- var(Mask), !.
mask2(Val, 0, Val).
mask2(_, 1, 1).

float_dec(Float, Dec) :-
	Float ins 0..1,
	include(var, Float, Vars),
	label(Vars),
	binlist_dec(Float, Dec).

float_all(Float, Ds) :- findall(Dec, float_dec(Float, Dec), Ds).

mask_val_float(Mask, Val, Fs) :-
	dec_bin(Val, Bin),
	maplist(mask2, Bin, Mask, Float),
	float_all(Float, Fs).

repeat(X, N, Xs) :- length(Xs, N), maplist(=(X), Xs).

list_uniqueIndex(List, UI) :-
	must_be(list, List),
	lists:number_list(List, 1, Rev),
	reverse(Rev, Numbered),
    sort(1, @=<, Numbered, ONum),
    lists:remove_dup_keys(ONum, NumSet),
    pairs_values(NumSet, UI).

list_select(Xs, Select, Xs_Select) :-
	maplist({Xs}/[I,E] >> nth1(I, Xs, E), Select, Xs_Select).

keys_val_pairs(Keys, Val, Pairs) :-
	maplist(length, Keys, Lens),
	maplist(repeat, Val, Lens, Vs),
	flatten(Vs, VsFlat),
	flatten(Keys, KsFlat),
	list_uniqueIndex(KsFlat, Unique),
	list_select(KsFlat, Unique, Ks_Uniq),
	list_select(VsFlat, Unique, Vs_Uniq),
	pairs_keys_values(Pairs, Ks_Uniq, Vs_Uniq).

task2_(Mask-Mem, DictIn, DictOut) :-
	pairs_values(Mem, Vs),
	pairs_keys(Mem, Is),
	maplist(mask_val_float(Mask), Is, As),
	keys_val_pairs(As, Vs, Ps),
	put_dict(Ps, DictIn, DictOut).

task2 :-
	read_file('input14', Input), 
	foldl(task2_, Input, ht{}, HT), 
	dict_keys(HT, Keys), 
	maplist({HT}/[Key,Val]>>get_dict(Key,HT,Val), Keys, Vals),
	sumlist(Vals, Sum),
	writeln(Sum).

main :-
	task1,
	task2.