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

parse(Op, Args) --> string_without(":", Op), ":", nonblanks(Args).

parse_file([[Op,Args]|Fs]) -->
	parse(Op, Args),
	whites,
	parse_file(Fs).
parse_file([]) --> [].

parse_first([Chars|_], Op) :-
	string_chars(String, Chars),
	atom_string(Op, String).

all_found(X) :-
	atom_codes(X, Codes),
	phrase(parse_file(Fields), Codes),
	maplist(parse_first, Fields, Ops),
	(
		sort(Ops, [byr, ecl, eyr, hcl, hgt, iyr, pid]);
		sort(Ops, [byr, cid, ecl, eyr, hcl, hgt, iyr, pid])
	).

parse_op([Op_C, Args|_]) :-
	string_chars(Op, Op_C),
	operator(Op, Args).

operator("byr", Arg) :- number_string(N, Arg), N in 1920..2002.
operator("iyr", Arg) :- number_string(N, Arg), N in 2010..2020.
operator("eyr", Arg) :- number_string(N, Arg), N in 2020..2030.
operator("pid", Arg) :- length(Arg, 9), Arg ins (48..57).
operator("hcl", [35|Arg]) :- length(Arg, 6), Arg ins (48..57 \/ 97..102).
operator("cid", _).
operator("ecl", Arg) :-
	string_chars(String, Arg),
	atom_string(X, String), 
	(X = amb; X = blu; X = brn; X = gry; X = grn; X = hzl; X = oth).
operator("hgt", Args) :-
	append(Arg, [X1, X2], Args),
	number_string(N, Arg),
	(
		([X1,X2] = [99,109] -> N in 150..193);
		([X1,X2] = [105,110] -> N in 59..76)
	).

all_valid(X) :-
	atom_codes(X, Codes),
	phrase(parse_file(Fields), Codes),
	include(parse_op, Fields, Valid),
	Fields = Valid.

main :-
	read_file('input4', Input),  
	include(all_found, Input, Valid1), 
	length(Valid1, N1),
	writeln(N1),
	include(all_valid, Input, Valid_Input),
	include(all_found, Valid_Input, Valid2),
	length(Valid2, N2),
	writeln(N2).
