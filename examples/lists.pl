% Some simple test Prolog programs
% working with lists
% Also demonstrates timing
% --------------------------------

suffix(Xs, Ys) :-
    append(_, Ys, Xs).

prefix(Xs, Ys) :-
    append(Ys, _, Xs).

sublist(Xs, Ys) :-
    suffix(Xs, Zs),
    prefix(Zs, Ys).

nrev([], []).
nrev([H|T0], L) :-
	nrev(T0, T),
	append(T, [H], L).


/** <examples>

?- sublist([a, b, c, d, e], [c, d]).
?- sublist([a, b, c, d, e], Ys).
?- sublist(Xs, Ys).

?- numlist(1, 1000, _L), time(nrev(_L, _)).

*/

