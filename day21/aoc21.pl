:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

words([]) --> [].
words([X|Xs]) --> string_without(" ", X), blank, words(Xs).

allergenes([]) --> [].
allergenes([X|Xs]) --> string_without(",)",X), (", " | ")"), allergenes(Xs).

line(Xs-Ys) --> words(Xs), "(contains ", allergenes(Ys).

lines([]) --> (eos), !.
lines([]) --> ("\n\n"), !.
lines(X|Xs) --> line(X), "\n", lines(Xs).