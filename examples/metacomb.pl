/*
Meta-calls/nested probability computations.
Reasoning about the probabilities of queries within a probabilistic program.
Can be used to implement simple forms of combining rules.
In this example, max_true(G1,G2) succeeds with the success probability of 
the more likely argument.
From
De Raedt, Luc, and Angelika Kimmig. "Probabilistic (logic) programming concepts." Machine Learning (2015): 1-43.
*/



:- use_module(library(pita)).

:- cplint.



max(A,B,A):-
  A>=B.

max(A,B,B):-
  B>=A.

p(P):P.

max_true(G1, G2) :-
  prob(G1, P1), 
  prob(G2, P2), 
  max(P1, P2, P), p(P).

a:0.5. 
b:0.7. 
c:0.2.

d :-
  a, \+ b.

e :- 
  b, c.
:- end_cplint.

/** <examples>

?-  prob(max_true(d,e),P).
% expected result 0.15000000000000002

*/
