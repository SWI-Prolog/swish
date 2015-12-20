/*
Example showing the effect of noisy or. From
J. Vennekens, Marc Denecker, and Maurice Bruynooghe. CP-logic: A language of 
causal probabilistic events and its relation to logic programming. 
Theory Pract. Log. Program., 9(3):245-308, 2009.
*/
:- use_module(library(pita)).
:- cplint.

% Russian roulette with two guns, left_gun and right_gun

death : 1/6 :- pull_trigger(left_gun).
% if you pull the trigger of the left gun, you are dead with probability 1/6

death : 1/6 :- pull_trigger(right_gun).
% if you pull the trigger of the right gun, you are dead with probability 1/6

pull_trigger(left_gun).
% you surely pull the trigger of the left gun
pull_trigger(right_gun).
% you surely pull the trigger of the right gun

:- end_cplint.

/** <examples>

?- prob(death,Prob). % what is the probability that you are dead?
*/
 
