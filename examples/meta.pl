
:- use_module(library(pita)).

:- cplint.

a:0.2:-
  prob(b,P),
  P>0.2.

b:0.5:-
  c.

c.

:- end_cplint.

/** <examples>

?- prob(a,Prob).
?- prob_bar(a,Prob).

*/
