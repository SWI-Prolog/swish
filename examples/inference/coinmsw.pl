/*
Throwing a coin with uncertainty on its fairness, from
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
values(throw(_),[heads,tails]).
:- set_sw(throw(fair),[0.5,0.5]).
:- set_sw(throw(biased),[0.6,0.4]).
values(fairness,[fair,biased]).
:- set_sw(fairness,[0.9,0.1]).

res(Coin,R):- toss(Coin),fairness(Coin,Fairness),msw(throw(Fairness),R).
% if we toss a Coin that is not biased then it lands heads with probability 1/2
% and tails with probability 1/2
% if we toss a Coin that is biased then it lands heads with probability 0.6
% % and tails with probability 0.4
fairness(_Coin,Fairness):-msw(fairness,Fairness).
% a Coin is fair with probability 0.9 and biased with probability 0.1
toss(coin).
% coin is certainly tossed

:- end_lpad.

/** <examples>

?- prob(res(coin,heads),Prob).% what is the probability that coin lands heads?
% expected result 0.51
?- prob(res(coin,tails),Prob).  % what is the probability that coin lands tails?
% expected result 0.49
?- prob_bar(res(coin,heads),Prob).  % what is the probability that coin lands heads?
% expected result 0.51
?- prob_bar(res(coin,tails),Prob).  % what is the probability that coin lands tails?
% expected result 0.49


*/
 
