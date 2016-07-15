/*
A simple Bayesian network from Figure 2 in
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195.209. Springer, 2004.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

burg(t):0.1; burg(f):0.9.
% there is a burglary with probability 0.1
earthq(t):0.2; earthq(f):0.8.
% there is an eartquace with probability 0.2
alarm(t):-burg(t),earthq(t).
% if there is a burglary and an earthquake then the alarm surely goes off
alarm(t):0.8 ; alarm(f):0.2:-burg(t),earthq(f).
% it there is a burglary and no earthquake then the alarm goes off with probability 0.8
alarm(t):0.8 ; alarm(f):0.2:-burg(f),earthq(t).
% it there is no burglary and an earthquake then the alarm goes off with probability 0.8
alarm(t):0.1 ; alarm(f):0.9:-burg(f),earthq(f).
% it there is no burglary and no earthquake then the alarm goes off with probability 0.1

:- end_lpad.

/** <examples>

?- prob(alarm(t),Prob).  % what is the probability that the alarm goes off?
% expected result 0.30000000000000004
?- prob(alarm(f),Prob).  % what is the probability that the alarm doesn't go off?
% expected result  0.7000000000000002
?- prob_bar(alarm(t),Prob).  % what is the probability that the alarm goes off?
% expected result 0.30000000000000004
?- prob_bar(alarm(f),Prob).  % what is the probability that the alarm doesn't go off?
% expected result  0.7000000000000002

*/
