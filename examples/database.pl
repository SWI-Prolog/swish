% Doing database manipulation
% ---------------------------

:- dynamic p/1.

assert_and_retract :-
    forall(between(1, 10, X), assert(p(X))),
    forall(retract(p(X)), writeln(X)).


/** <examples>

?- assert_and_retract.

*/
