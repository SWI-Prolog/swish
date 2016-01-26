/*
Flexible probabilities: variable probabilistic annotations.
The example models drawing a person at random from a population and 
computing the probability that it is a male or a female.
From
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- cplint.

male:M/P; female:F/P:-
  findall(Male,male(Male),LM),
  findall(Female,female(Female),LF),
  length(LM,M),
  length(LF,F),
  P is F+M.

:- end_cplint.

male(john).
male(david).

female(anna).
female(elen).
female(cathy).

/** <examples>

?- prob(male,Prob).  % what is the probability of sampling a male from the 
% population?
% expected result 0.4
?- prob(female,Prob).% what is the probability of sampling a female from the 
% population?
% expected result 0.6
?- prob_bar(male,Prob).  % what is the probability of sampling a male from the 
% population?
% expected result 0.4
?- prob_bar(female,Prob). % what is the probability of sampling a female from the 
% population?
% expected result 0.6
*/

 
