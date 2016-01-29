:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

sampled_male(X):0.5:-
  db(male(X)).

:- end_lpad.

male(john).
male(david).

female(anna).
female(elen).
female(cathy).

/** <examples>

?- prob(sampled_male(X),Prob).
% expected result 0.5 for X=john, X=david

*/
