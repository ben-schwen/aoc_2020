:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).

cube(X,Y,0,Mat,1) :- index(Mat,X,Y,1).
cube(_,_,_,_,0).

neighbor(X,Y,Z,X1,Y1,Z1) :-
	abs(X-X1) + abs(Y-Y1) + abs(Z-Z1) #> 0,
	abs(X-X1) #=< 1,
	abs(Y-Y1) #=< 1,
	abs(Z-Z1) #=< 1.

cube_count(X,Y,Z,Mat,N) :-
	findall((A,B,C), 
		(neighbor(X,Y,Z,A,B,C), label([A,B,C]), cube(A,B,C,Mat,1)), 
	Xs),
	length(Xs,N).

cube_next(1,X,Y,Z,Mat,1) :-
	cube(X,Y,Z,Mat,1),
	cube_count(X,Y,Z,Mat,2), !.
cube_next(1,X,Y,Z,Mat,1) :-
	cube(X,Y,Z,Mat,_),
	cube_count(X,Y,Z,Mat,3), !.
cube_next(1,_,_,_,_,0).

lines([]) --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ("\n" | white | call(eos)), !.
line([C|Cs]) --> parse(C), line(Cs).

parse(1) --> "#".
parse(0) --> ".".

read_file(File, Input) :-
    once(phrase_from_file(lines(Input), File)).

index(Matrix, Row, Col, Value):-
  nth1(Row, Matrix, MatrixRow),
  nth1(Col, MatrixRow, Value).
