:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).
:- use_module(library(yall)).


lines([]) --> ("\n" | eos), !.
lines([X|Xs]) --> string_without("\n", X), "\n", lines(Xs).

id(N) --> "Tile", blanks, integer(N), ":".

tile(img(N,Xs)) --> id(N), "\n", lines(Xs).

tiles([]) --> ("\n" | eos), !.
tiles([X|Xs]) --> tile(X), tiles(Xs).

read_file(File, Tiles) :-
	phrase_from_file(tiles(Tiles), File).

dif_img(img(Id1,_), img(Id2,_)) :- dif(Id1,Id2).

print(Xs) :- is_list(Xs),  maplist(string_chars, S, Xs), maplist(writeln, S).
print(img(_,Xs)) :- print(Xs).


flip(Xs, Ys) :- maplist(reverse, Xs, Ys).

rotate(Xs, Ys) :- transpose(Xs, Zs), maplist(reverse, Zs, Ys).
rotate(Xs, Ys) :- reverse(Xs, Zs), maplist(reverse, Zs, Ys).
rotate(Xs, Ys) :- transpose(Xs, Zs), reverse(Zs, Ys).

move(img(N,Xs), img(N,Xs)).
move(img(N,Xs), img(N,Ys)) :- rotate(Xs, Ys).
move(img(N,Xs), img(N,Ys)) :- rotate(Xs, Zs), flip(Zs, Ys).

first([X|_],X).

top(img(_,Xs), Top) :- first(Xs, Top).
bot(img(_,Xs), Bot) :- last(Xs, Bot).
left(img(_,Xs), Left) :- maplist(first, Xs, Left).
right(img(_,Xs), Right) :- maplist(last, Xs, Right).

lr(Xs, Ys) :- left(Xs, X), right(Ys, X).
tp(Xs, Ys) :- top(Xs, X), bot(Ys, X).

list_select(Xs, Select, Xs_Select) :-
	maplist({Xs}/[I,E] >> nth1(I, Xs, E), Select, Xs_Select).

img_id(img(Id,_), Id).

prod(X,Y,Z) :- Z #= X*Y.

position(Images, Table, N, Rotated) :-
	length(Images, Dim),
	Len = round(sqrt(Dim)),
    member(Image, Images),
    move(Image, Rotated),
    LeftN is N - 1,
    (
        (0 is LeftN mod Len) ->
            true
        ;   (
            nth1(LeftN, Table, Left),
            dif_img(Left, Image),
            lr(Rotated, Left)
        )
    ),
    UpN is N - Len,
    (
        (UpN < 1) ->
            true
        ;   (
            nth1(UpN, Table, Top),
            dif_img(Top, Image),
            tp(Rotated, Top)
        )
    ).

rbind(Xs, Ys, Zs) :- append(Xs, Ys, Zs).
cbind(Xs, Ys, Zs) :- maplist(append,Xs, Ys, Zs).

list_tail([_|Xs], Xs).
list_butlast(Xs, Ys) :-
	reverse(Xs, R1),
	R1 = [_|R2],
	reverse(R2, Ys).

tile_borderless(T1, T2) :-
	is_list(T1),
	list_tail(T1, Tail),
	list_butlast(Tail, Front),
	maplist(list_tail, Front, Right),
	maplist(list_butlast, Right, T2).
	
tile_borderless(img(N,T1), img(N,T2)) :-
	tile_borderless(T1, T2).

task1 :-
	read_file('input20', Images),
	length(Images, Dim), 
	length(Table, Dim), 
	numlist(1, Dim, Ns),
	maplist(position(Images, Table), Ns, Table), 
	maplist(img_id, Table, Ids),
	N is round(sqrt(Dim)),
	LB is N*(N-1)+1,
	list_select(Ids, [1,N,LB,Dim], Select),  
	foldl(prod, Select, 1, Prod), 
	writeln(Prod).