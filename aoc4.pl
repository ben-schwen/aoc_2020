:- use_module(library(clpfd)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

split(In, Sep, [Left|Rest]) :-
    append(Left, [Sep|Right], In), !, split(Right, Sep, Rest).
split(In, _Sep, [In]).

concat(X, Y) :-
	atomic_list_concat(X, " ", Y).

read_file(File, Input) :-
	read_file_to_string(File, Raw, []), 
	split_string(Raw, "\n", [], Lines), 
	split(Lines, "", Split), 
	maplist(concat, Split, Input).

parse(byr) --> "byr:", nonblanks(_).
parse(iyr) --> "iyr:", nonblanks(_).
parse(eyr) --> "eyr:", nonblanks(_).
parse(hgt) --> "hgt:", nonblanks(_).
parse(hcl) --> "hcl:", nonblanks(_).
parse(ecl) --> "ecl:", nonblanks(_).  
parse(pid) --> "pid:", nonblanks(_).
parse(cid) --> "cid:", nonblanks(_).

parse_file([F|Fs]) -->
	parse(F),
	whites,
	parse_file(Fs).
parse_file([]) --> [].

all_found(X) :-
	atom_codes(X, Codes),
	phrase(parse_file(Fields), Codes),
	(
		sort(Fields, [byr, ecl, eyr, hcl, hgt, iyr, pid]);
		sort(Fields, [byr, cid, ecl, eyr, hcl, hgt, iyr, pid])
	).

field(byr(X)) --> "byr:", integer(X), whites.
field(iyr(X)) --> "iyr:", integer(X), whites.
field(eyr(X)) --> "eyr:", integer(X), whites.
field(hgt_cm(X)) --> "hgt:", integer(X), "cm".
field(hgt_in(X)) --> "hgt:", integer(X), "in".
field(hcl(X)) --> "hcl:", "#", nonblanks(X).
field(ecl(X)) --> "ecl:", nonblanks(X).  
field(pid(X)) --> "pid:", digits(X).
field(cid(X)) --> "cid:", nonblanks(X).

fields([F|Fs]) -->
    field(F),
    whites,
    fields(Fs).
fields([]) --> [].

byr(X) :- X in 1920..2002.
iyr(X) :- X in 2010..2020.
eyr(X) :- X in 2020..2030.
hgt_cm(X) :- X in 150..193.
hgt_in(X) :- X in 59..76.
pid(X) :- length(X,9).
cid(_).
hcl(Chars) :-
	length(Chars, 6),
	Chars ins (48..57 \/ 97..102).
ecl(Chars) :- 
	string_chars(String, Chars),
	atom_string(X, String), 
	(X = amb; X = blu; X = brn; X = gry; X = grn; X = hzl; X = oth).

eval_field(X) :-
	(X -> true).

all_valid(X) :-
	atom_codes(X, Codes),
	phrase(fields(Fields), Codes),
	include(eval_field, Fields, Valid),
	length(Fields, N),
	length(Valid, N).

main :-
	read_file('input4', Input),  
	include(all_found, Input, Valid1), 
	length(Valid1, N1),
	writeln(N1),
	include(all_valid, Input, Valid_Input),
	include(all_found, Valid_Input, Valid2),
	length(Valid2, N2),
	writeln(N2).
