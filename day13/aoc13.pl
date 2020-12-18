:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

lines(X, Bs) --> integer(X), "\n", bus(Bs).

bus([]) --> (eos | "\n" ), !.
bus([X|Xs]) --> (integer(X) | ([X], {letter(X)})), ("," | []), bus(Xs).

letter(L) :-
	char_type(L, alpha).

time_bus_wait(T, B, W) :-
	W #= B - (T mod B).

read_file(File, Time, Bs) :-
    once(phrase_from_file(lines(Time, Bs), File)).

x(120).

prod(X, Y, Z) :-
	Z #= X*Y.

div(X, Y, Z) :-
	Z #= X div Y.

euclid(D,M,N,S,T) :- egcdaux(1,0,0,1,M,N,S,T,D).

egcdaux(_,_,A,B,C,D,A,B,D) :- 0 #= C mod D.
egcdaux(A1,B1,A,B,C,D,S,T,Y) :-
                Q #= C div D,
                R #= C mod D,
                NA #= A1 - Q * A,
                NB #= B1 - Q * B,
                egcdaux(A,B,NA,NB,D,R,S,T,Y).

chinese_remainder(Bs, Ws, X, Prod) :-
	foldl(prod, Bs, 1, Prod), 
	maplist(div(Prod), Bs, Xs), 
	maplist(euclid(1), Bs, Xs, _, Ss), 
	maplist(prod, Xs, Ss, Ms), 
	maplist(prod, Ms, Ws, Ps), 
	sumlist(Ps, Sum), 
	X #= -Sum mod Prod.

nth0_(Xs, X, Index) :-
	nth0(Index, Xs, X).

model(Bss, Bs, X, Prod) :-
	maplist(nth0_(Bss), Bs, Ws),
	chinese_remainder(Bs, Ws, X, Prod).

main :-
	read_file('input13', Time, Bss),
	exclude(x, Bss, Bs), 
	maplist(time_bus_wait(Time), Bs, Wait_Times), 
	min_list(Wait_Times, Min_Time), 
	nth0(Index, Wait_Times, Min_Time),
	nth0(Index, Bs, Bus),
	N #= Bus * Min_Time,
	writeln(N),
	model(Bss, Bs, X, Prod),
	writeln(X).
