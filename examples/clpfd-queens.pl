
%%	n_queens(?N, ?Cols) is nondet.
%
%	@param The n-th element of Cols tells the row in which the
%	queen in the n-th column is placed.
%	@author Markus Triska

:- use_module(library(clpfd)).

n_queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).


/** <examples>

?- n_queens(8, Qs), labeling([ff], Qs).
?- n_queens(24, Qs), labeling([ff], Qs).
?- n_queens(100, Qs), labeling([ff], Qs).

*/
