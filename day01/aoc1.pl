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


/*read_file_to_string('input2', S, []), split_string(S, '\n', [], S2), 
maplist(number_string, Xs, S2),
find3(Z, Xs).*/
Rscript -e "rmarkdown::render('ex8/ex8_amrc.Rmd', 'all')"