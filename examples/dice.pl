/*
A six-sided die is repeatedly thrown until the outcome is six.
on(T,F) means that on the Tth throw the face F came out.
From
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(pita)).
:- cplint.

% on(T,F) means that the dice landed on face F at time T

on(0,1):1/6;on(0,2):1/6;on(0,3):1/6;
on(0,4):1/6;on(0,5):1/6;on(0,6):1/6.
% at time 0 the dice lands on one of its faces with equal probability

on(X,1):1/6;on(X,2):1/6;on(X,3):1/6;
on(X,4):1/6;on(X,5):1/6;on(X,6):1/6:-
  X1 is X-1,X1>=0,on(X1,_),
  \+ on(X1,6).
% at time T the dice lands on one of its faces with equal probability if
% at the previous time point it was thrown and it did not landed on face 6

:- end_cplint.

/** <examples>

?- prob(on(0,1),Prob). % what is the probability that the dice lands on face 1 at time 0?
?- prob(on(1,1),Prob). % what is the probability that the dice lands on face 1 at time 1?
?- prob(on(2,1),Prob). % what is the probability that the dice lands on face 1 at time 2?

*/
 
