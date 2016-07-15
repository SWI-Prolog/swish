/*
A six-sided die is repeatedly thrown until the outcome is six.
on(T,F) means that on the Tth throw the face F came out.
From
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

% on(T,F) means that the die landed on face F at time T

on(0,1):1/6;on(0,2):1/6;on(0,3):1/6;
on(0,4):1/6;on(0,5):1/6;on(0,6):1/6.
% at time 0 the dice lands on one of its faces with equal probability

on(X,1):1/6;on(X,2):1/6;on(X,3):1/6;
on(X,4):1/6;on(X,5):1/6;on(X,6):1/6:-
  X1 is X-1,X1>=0,on(X1,_),
  \+ on(X1,6).
% at time T the die lands on one of its faces with equal probability if
% at the previous time point it was thrown and it did not land on face 6

evidence:-
  on(0,1),
  on(1,1).

:- end_lpad.

/** <examples>

?- prob(on(0,1),Prob). % what is the probability that the die lands on face 1 at time 0?
% expected result 0.16666666666666666
?- prob(on(1,1),Prob). % what is the probability that the die lands on face 1 at time 1?
% expected result 0.13888888888888887
?- prob(on(2,1),Prob). % what is the probability that the die lands on face 1 at time 2?
% expected result 0.11574074074074071
?- prob(on(2,1),on(0,1),Prob). % what is the probability that the die lands on face 1 at time 2 given that it landed on face 1 at time 0?
% expected result 0.13888888888888887
?- prob(on(2,1),evidence,Prob).
% what is the probability that the die lands on face 1 at time 2 given that it landed on face 1 at times 0 and 1?
% expected result 0.16666666666666666
?- prob_bar(on(0,1),Prob). % what is the probability that the die lands on face 1 at time 0?
% expected result 0.16666666666666666
?- prob_bar(on(1,1),Prob). % what is the probability that the die lands on face 1 at time 1?
% expected result 0.13888888888888887
?- prob_bar(on(2,1),Prob). % what is the probability that the die lands on face 1 at time 2?
% expected result 0.11574074074074071

?- prob_bar(on(2,1),on(0,1),Prob). % what is the probability that the die lands on face 1 at time 2 given that it landed on face 1 at time 0?
% expected result 0.13888888888888887
?- prob_bar(on(2,1),on(1,1),Prob). % what is the probability that the die lands on face 1 at time 2 given that it landed on face 1 at time 1?
% expected result 0.16666666666666666


*/
 
