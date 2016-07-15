/*
One-dimensional  Kalman filter. Hidden Markov model with a real
value as state and a real value as output. The next state is given by
the current state plus Gaussian noise (mean 0 and variance 2 in this example)
and the output is given by the current state plus Gaussian noise (mean
0 and variance 1 in this example). 
This example can be considered as modeling a random walk of a single continuous 
state variable with noisy observations. 
Given that at time 0 the value 2.5 was
observed, what is the distribution of the state at time 1 (filtering problem)?
The distribution of the state is plotted in the case of having (posterior) or 
not having the observation (prior).
Liklihood weighing is used to condition the distribution on evidence on
a continuous random variable (evidence with probability 0).
CLP(R) constraints allow both sampling and weighing samples with the same
program.
From
Islam, Muhammad Asiful, C. R. Ramakrishnan, and I. V. Ramakrishnan. 
"Inference in probabilistic logic programs with continuous random variables." 
Theory and Practice of Logic Programming 12.4-5 (2012): 505-523.
http://arxiv.org/pdf/1112.2681v3.pdf
Russell, S. and Norvig, P. 2010. Arficial Intelligence: A Modern Approach. 
Third Edition, Prentice Hall, Figure 15.10 page 587

*/
:- use_module(library(mcintyre)).
:- use_module(library(clpr)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

dp_n_values(N,N,_Alpha,[]):-!.

dp_n_values(N0,N,Alpha,[[V]-1|Vs]):-
  N0<N,
  dp_value(N0,Alpha,V),
  N1 is N0+1,
  dp_n_values(N1,N,Alpha,Vs).
  
dp_value(NV,Alpha,V):-
  dp_stick_index(NV,Alpha,I),
  dp_pick_value(I,V).

dp_pick_value(_,V):gaussian(V,0,2).

dp_stick_index(NV,Alpha,I):-
  dp_stick_index(1,NV,Alpha,I).

dp_stick_index(N,NV,Alpha,V):-
  stick_proportion(N,Alpha,P),
  choose_prop(N,NV,Alpha,P,V).
  
choose_prop(N,NV,_Alpha,P,N):-
  pick_portion(N,NV,P).

choose_prop(N,NV,Alpha,P,V):-
  neg_pick_portion(N,NV,P),
  N1 is N+1,
  dp_stick_index(N1,NV,Alpha,V).
 


stick_proportion(_,Alpha,P):beta(P,1,Alpha).

pick_portion(_,_,P):P;neg_pick_portion(_,_,P):1-P.

:- end_lpad.
obs0([
-1,-1,-1,-1]).


obs([
-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]).

hist(Samples,NBins,Chart):-
  mc_sample_arg(dp_stick_index(1,10.0,V),Samples,V,L),
  histogram(L,NBins,Chart).

hist_repeated_indexes(Samples,NBins,Chart):-
  repeat_sample(0,Samples,L),
  histogram(L,NBins,Chart).

repeat_sample(S,S,[]):-!.

repeat_sample(S0,S,[[N]-1|LS]):-
  mc_sample_arg_first(dp_stick_index(1,1,10.0,V),10,V,L),
  length(L,N),
  S1 is S0+1,
  repeat_sample(S1,S,LS).

hist_val(Samples,NBins,Chart):-
  mc_sample_arg_first(dp_n_values(0,Samples,10.0,V),1,V,L),
  L=[Vs-_],
  histogram(Vs,NBins,Chart).

prior(Samples,NBins,Chart):-
  mc_sample_arg_first(dp_n_values(0,100,10.0,T),Samples,T,L),
  maplist(to_list,L,L1),
  append(L1,Vs),
%  L=[Vs-_],
  histogram(Vs,NBins,Chart).
%  
post(Samples,NBins,Chart):-
  obs(O),
  maplist(to_val,O,O1),
  length(O1,N),
  mc_lw_sample_arg(dp_n_values(0,N,10.0,T),dp_n_values(0,N,10.0,O1),Samples,T,L),
  maplist(to_list,L,L1),
  append(L1,Vs),
%  L=[Vs-_],
  histogram(Vs,NBins,Chart).
%   densities(L0,L,NBins,Chart).

to_list(L-W,L1):-
  maplist(app_w(W),L,L1).

app_w(W,V-_,V-W).
to_val(V,[V]-1).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5.
% Observation as in Russel and Norvig 2010, Fig 15.10

/** <examples>
?- dens_lw(1000,40,G).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5 by taking 1000 samples and dividing the domain
% in 40 bins
?- post(100,40,G).
?- hist(100,40,G).
?- hist_val(100,40,G).
?- hist_repeated(100,40,G).
% plot the density of the state at time 1 in case of no observation
% by taking 1000 samples and dividing the domain
% in 40 bins


*/
 
