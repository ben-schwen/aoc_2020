:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

input([X|Xs]) --> string_without("\n",X), "\n", input(Xs).
input([X]) --> string_without("\n", X). 

concat_([]) --> [].
concat_([X|Xs]) --> X, concat_(Xs).

concat(Xs, S) :-
	once(phrase(concat_(Xs), C)),
	string_chars(S,C).

numbers([]) --> [].
numbers([X|Xs]) --> integer(X), blanks, numbers(Xs).

rule(and(N1-Xs)) --> integer(N1), ": ", numbers(Xs).
rule(or(N1-(X1,X2))) --> integer(N1), ": ",  numbers(X1), "| ", numbers(X2).
rule(p(N1-X)) --> integer(N1), ": ", [34], [X], ([34] | [34,10]).

rules([]) --> (eos | "\n"), !.
rules([X|Xs]) --> rule(X), rules(Xs).

messages([]) --> (eos | "\n"), !.
messages([X|Xs]) --> string_without("\n",X), messages(Xs).

parse_number_([X|Xs]) --> "r",{number_string(X,S)}, S, ", ", parse_number_(Xs).
parse_number_([X]) --> "r", {number_string(X,S)}, S.
parse_number(Xs, S) :-
	once(phrase(parse_number_(Xs), C)),
	string_chars(S, C).

parse(p(N1-X), S) :-
	number_string(N1, S1),
	string_codes(S2, [X]),
	concat(["r", S1, " --> ", "\"", S2, "\"."], S).
parse(and(N1-Xs), S) :-
	number_string(N1, S1),
	parse_number(Xs, S2),
	concat(["r", S1, " --> ", S2, "."], S).
parse(or(N1-(X1,X2)), S) :-
	number_string(N1, S1),
	parse_number(X1, S2),
	parse_number(X2, S3),
	concat(["r", S1, " --> (", S2, ")|(", S3, ")."], S).	

lines([])     --> eos, !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | eos ), !.
line([C|Cs]) --> [C], line(Cs).

parse_all(FileIn, FileOut) :-
	phrase_from_file(rules(Rules), FileIn), 
	maplist(parse, Rules, Parsed), 
	open(FileOut, write, Out), 
	maplist(format(Out, '~w\n'), Parsed), 
	close(Out).

task1 :-
	Out = 'parse.pl',
	parse_all('input19a', Out),
	consult(Out),
	phrase_from_file(lines(Input), 'input19b'),
	include(phrase(r0), Input, Valid),
	length(Valid, N),
	writeln(N).