/*
A three-sided die is repeatedly thrown until the outcome is three.
on(T,F) means that on the Tth throw the face F came out.
From
Riguzzi and Terrance Swift. The PITA system: Tabling and answer subsumption for 
reasoning under uncertainty. Theory and Practice of Logic Programming, 
27th International Conference on Logic Programming (ICLP'11) Special Issue, 
Lexington, Kentucky 6-10 July 2011, 11(4-5):433-449, 2011
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

% on(T,F) means that the dice landed on face F at time T
on(0,1):1/3;on(0,2):1/3;on(0,3):1/3.
% at time 0 the dice lands on one of its faces with equal probability

on(X,1):1/3;on(X,2):1/3;on(X,3):1/3:-
  X1 is X-1,X1>=0,
  on(X1,_),
  \+ on(X1,3).
% at time T the dice lands on one of its faces with equal probability if
% at the previous time point it was thrown and it did not landed on face 3

:- end_lpad.

/** <examples>

?- prob(on(0,1),Prob). % what is the probability that the dice lands on face 1 at time 0?
% expected result 0.3333333333333333
?- prob(on(1,1),Prob). % what is the probability that the dice lands on face 1 at time 1?
% expected result 0.2222222222222222
?- prob(on(2,1),Prob). % what is the probability that the dice lands on face 1 at time 2?
% expected result 0.14814814814814814
?- prob(on(3,1),Prob). % what is the probability that the dice lands on face 1 at time 3?
% expected result 0.09876543209876543
?- prob_bar(on(0,1),Prob). % what is the probability that the dice lands on face 1 at time 0?
% expected result 0.3333333333333333
?- prob_bar(on(1,1),Prob). % what is the probability that the dice lands on face 1 at time 1?
% expected result 0.2222222222222222
?- prob_bar(on(2,1),Prob). % what is the probability that the dice lands on face 1 at time 2?
% expected result 0.14814814814814814
?- prob_bar(on(3,1),Prob). % what is the probability that the dice lands on face 1 at time 3?
% expected result 0.09876543209876543

?- prob(on(2,1),on(0,1),Prob). % what is the probability that the dice lands on face 1 at time 2 given that it landed on face 1 at time 0?
% expected result 0.222222222222222
?- prob(on(2,1),on(1,1),Prob). % what is the probability that the dice lands on face 1 at time 2 given that it landed on face 1 at time 1?
% expected result 0.333333333333333
*/
 
