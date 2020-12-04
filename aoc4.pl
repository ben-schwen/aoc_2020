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

extract_id(String, Id) :-
	split_string(String, ":", [], Split),
	nth0(0, Split, Id).

all_ids(String, Ids) :-
	split_string(String, " ",  [], Split),
	maplist(extract_id, Split, Ids).

valid1(Ids) :-
	member("byr", Ids),
	member("iyr", Ids),
	member("eyr", Ids),
	member("hgt", Ids),
	member("hcl", Ids),
	member("ecl", Ids),
	member("pid", Ids).

split_str2(Sep, Pad, String, Split) :-
	split_string(String, Sep, Pad, Split).

byr(X) :- X #>= 1920, X #=< 2002.

iyr(X) :- X #>= 2010, X #=< 2020.
eyr(X) :- X #>= 2020, X #=< 2030.
hgt_cm(X) :- X #>= 150, X #=< 193.
hgt_in(X) :- X #>= 59, X #=< 76.
pid(X) :- length(X,9).
cid(_).
hcl(Chars) :-
	length(Chars, 6),
	Chars ins (48..57 \/ 97..102).

ecl(Chars) :- 
	string_chars(String, Chars),
	atom_string(X, String), 
	(X = amb; X = blu; X = brn; X = gry; X = grn; X = hzl; X = oth).

command(byr(X)) --> "byr:", integer(X), whites.
command(iyr(X)) --> "iyr:", integer(X), whites.
command(eyr(X)) --> "eyr:", integer(X), whites.
command(hgt_cm(X)) --> "hgt:", integer(X), "cm".
command(hgt_in(X)) --> "hgt:", integer(X), "in".
command(hcl(X)) --> "hcl:", "#", nonblanks(X).
command(ecl(X)) --> "ecl:", nonblanks(X).  
command(pid(X)) --> "pid:", digits(X).
command(cid(X)) --> "cid:", nonblanks(X).

commands([Command|Commands]) -->
    command(Command),
    whites,
    commands(Commands).
commands([]) --> [].

eval_command(X) :-
	(X -> true).

all_valid(X) :-
	atom_codes(X, Codes),
	phrase(commands(Commands), Codes),
	include(eval_command, Commands, Valid),
	length(Commands, N),
	length(Valid, N).

main :-
	read_file('input4', Input), 
	maplist(all_ids, Input, Ids1), 
	include(valid1, Ids1, Valid1), 
	length(Valid1, N1),
	writeln(N1),
	include(all_valid, Input, Valid_Input),
	maplist(all_ids, Valid_Input, Ids2),
	include(valid1, Ids2, Valid2),
	length(Valid2, N2),
	writeln(N2).
