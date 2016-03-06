/*
Throwing a coin with uncertainty on its fairness, from
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

heads(Coin): 1/2; tails(Coin) : 1/2:-toss(Coin),\+biased(Coin).
% if we toss a Coin that is not biased then it lands heads with probability 1/2
% and tails with probability 1/2
heads(Coin): 0.6 ; tails(Coin) : 0.4:-toss(Coin),biased(Coin).
% if we toss a Coin that is biased then it lands heads with probability 0.6
% % and tails with probability 0.4
fair(Coin):0.9 ; biased(Coin):0.1.
% a Coin is fair with probability 0.9 and biased with probability 0.1
toss(coin).
% coin is certainly tossed

:- end_lpad.

/** <examples>

?- mc_prob(heads(coin),Prob).  % what is the probability that coin lands heads?
% expected result 0.51
?- mc_prob(tails(coin),Prob).  % what is the probability that coin lands tails?
% expected result 0.49
?- mc_prob_bar(heads(coin),Prob).  % what is the probability that coin lands heads?
% expected result 0.51
?- mc_prob_bar(tails(coin),Prob).  % what is the probability that coin lands tails?
% expected result 0.49
?- mc_sample(heads(coin),1000,T,F,Prob).  
% take 1000 sample of heads(coin) and return the number of successes (T),
% the number of failures (F) and the probability

?- mc_sample(tails(coin),1000,T,F,Prob).  
% take 1000 sample of tails(coin) and return the number of successes (T),
% the number of failures (F) and the probability

?- mc_sample(heads(coin),1000,Prob).  
% take 1000 sample of heads(coin) and return the probability

?- mc_sample(tails(coin),1000,Prob).  
% take 1000 sample of tails(coin) and return the probability

?- mc_sample_bar(heads(coin),1000,Chart).  
% take 1000 sample of heads(coin) and chart the number of successes and 
% faliures

?- mc_sample_bar(tails(coin),1000,Chart).  
% take 1000 sample of tails(coin) and chart the number of successes and 
% faliures

?- mc_rejection_sample(heads(coin),biased(coin),1000,S,F,P).
% take 1000 sample of heads(coin) given that biasdd(coin) is true
% Use rejection sampling
% F = 387,
% P = 0.613,
% S = 613
*/
 
