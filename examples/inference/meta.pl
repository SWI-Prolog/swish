/*
Probabilities computation in the body of probabilistic clauses.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

a:0.2:-
  prob(b,P),
  P>0.2.

b:0.5:-
  c.

c.

:- end_lpad.

/** <examples>

?- prob(a,Prob).
% expected result 0.2
?- prob_bar(a,Prob).
% expected result 0.2

*/
