/*
Example showing the use of negation.  From
J. Vennekens, Marc Denecker, and Maurice Bruynooghe. CP-logic: A language 
of causal probabilistic events and its relation to logic programming. 
Theory Pract. Log. Program., 9(3):245-308, 2009.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- cplint.

push.
% we surely push the switch

light : 0.4 :- push.
% if the switch is pushed, the light goes on with probability 0.4

replace :- \+ light.
% if there is no light we must replace the bulb

:- end_cplint.

/** <examples>

?- prob(replace,Prob). % what is the probability that we replace the bulb?
% expected result 0.6
?- prob(light,Prob).   % what is the probability that the light is on?
% expected result 0.4
?- prob_bar(replace,Prob). % what is the probability that we replace the bulb?
% expected result 0.6
?- prob_bar(light,Prob).   % what is the probability that the light is on?
% expected result 0.4


*/
 
