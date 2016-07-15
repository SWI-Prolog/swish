/*
This program models the effect of flu and hay fever on the sneezing symptom.
From
F. Riguzzi and T. Swift. The PITA system: Tabling and answer subsumption for reasoning under uncertainty. Theory and Practice of Logic Programming, 27th International Conference on Logic Programming (ICLP'11) Special Issue, 11(4-5):433-449, 2011.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

strong_sneezing(X) : 0.3 ; moderate_sneezing(X) : 0.5 :- flu(X).
% if X has the flu, there is a probability of 0.3 that he has strong sneezing 
% and a probability of 0.5 that she has moderate sneezing

strong_sneezing(X) : 0.2 ; moderate_sneezing(X) : 0.6 :- hay_fever(X).
% if X has hay fever, there is a probability of 0.2 that he has strong sneezing 
% and a probability of 0.6 that she has moderate sneezing

flu(bob).
% bob has certainly the flu

hay_fever(bob).
% bob has certainly hay fever

:- end_lpad.

/** <examples>

?- prob(strong_sneezing(bob),Prob). % what is the probability that bob has strong sneezing?
% expected result 0.43999999999999995
?- prob(moderate_sneezing(bob),Prob). % what is the probability that bob has 
% moderate sneezing?
% expected result 0.7999999999999998
?- prob_bar(strong_sneezing(bob),Prob). % what is the probability that bob has strong sneezing?
% expected result 0.43999999999999995
?- prob_bar(moderate_sneezing(bob),Prob). % what is the probability that bob has 
% moderate sneezing?
% expected result 0.7999999999999998


*/
