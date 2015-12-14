% Doing database manipulation
% ---------------------------

:- dynamic p/1.

assert_and_retract :-
    forall(between(1, 10, X), assert(p(X))),
    forall(retract(p(X)), writeln(X)).

assert_many(Count) :-
    forall(between(1, Count, X), assert(p(X))),
    retractall(p(_)).

/** <examples>

% Basic usage
?- assert_and_retract.

% Show timing
?- assert_many(1 000 000).

% Pengines have a (default) 100Mb limit to their program size
?- assert_many(10 000 000).
*/
