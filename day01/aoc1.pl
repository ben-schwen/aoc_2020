:- use_module(library(clpfd)).
:- use_module(library(readutil)).

find2(Z,Xs) :-
    member(X, Xs),
    member(Y, Xs),
    X + Y #= 2020,
    X #< Y,
    Z #= X*Y.

find3(Y,Xs) :-
    member(X1, Xs),
    member(X2, Xs),
    member(X3, Xs),
    X1 #< X2,
    X2 #< X3,
    X1 + X2 + X3 #= 2020,
    Y #= X1 * X2 * X3.
