:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

lines([]) --> (eos | "\n\n"), !.
lines([(C,N)|Xs]) --> alpha_to_lower(A), {char_code(C,A)}, integer(N), "\n", lines(Xs).

read_file(File, Input) :-
    once(phrase_from_file(lines(Input), File)).

angle_dir(0, n).
angle_dir(90, e).
angle_dir(180, s).
angle_dir(270, w).

move((n, N), (X, Y, A), (X, Y1,   A)) :- Y1 #= Y + N.
move((s, N), (X, Y, A), (X, Y1,   A)) :- Y1 #= Y - N.
move((e, N), (X, Y, A), (X1, Y,   A)) :- X1 #= X + N.
move((w, N), (X, Y, A), (X1, Y,   A)) :- X1 #= X - N.
move((l, N), (X, Y, A), (X,  Y,  A1)) :- A1 #= (A - N) mod 360.
move((r, N), (X, Y, A), (X,  Y,  A1)) :- A1 #= (A + N) mod 360.
move((f, N), (X, Y, A), (X1, Y1,  A)) :- Op1 = (D, N), angle_dir(A, D), move(Op1, (X, Y, A), (X1, Y1, A)).

rotate_right((X,Y), (X1, Y1)) :- X1 #= Y, Y1 #= -X.
rotate_left(P, P1) :- rotate_right(P1, P).

move2((n,N), (S, (X, Y)), (S, (X, Y1))) :- Y1 #= Y + N.
move2((s,N), (S, (X, Y)), (S, (X, Y1))) :- Y1 #= Y - N.
move2((e,N), (S, (X, Y)), (S, (X1, Y))) :- X1 #= X + N.
move2((w,N), (S, (X, Y)), (S, (X1, Y))) :- X1 #= X - N.
move2((r, 90),  (S, W), (S, W1)) :- rotate_right(W, W1).
move2((r, 180), (S, W), (S, W1)) :- rotate_right(W, WX), rotate_right(WX, W1).
move2((r, 270), (S, W), (S, W1)) :- rotate_left(W, W1).
move2((l, 90),  (S, W), (S, W1)) :- rotate_left(W, W1).
move2((l, 180), (S, W), (S, W1)) :- rotate_right(W, WX), rotate_right(WX, W1).
move2((l, 270), (S, W), (S, W1)) :- rotate_right(W, W1).
move2((f, N), ((SX, SY), (WX, WY)), ((SX1, SY1), (WX, WY))) :-
	SX1 #= SX + (WX * N), SY1 #= SY + (WY * N). 

dist((X,Y), Dist) :-
	Dist #= abs(X) + abs(Y).

main :-
	read_file('input12', Input),
	foldl(move, Input, (0,0,90), (X, Y, _)),
	dist((X,Y), Dist),
	writeln(Dist),
	foldl(move2, Input, ((0,0), (10,1)), (Ship, _, _)),
	dist(Ship, Dist2),
	writeln(Dist2).