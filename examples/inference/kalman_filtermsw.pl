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
PRISM syntax.
*/
:- use_module(library(mcintyre)).
:- use_module(library(clpr)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

kf(N,O, T) :-
  msw(init,S),
  kf_part(0, N, S,O,T).

kf_part(I, N, S,[V|RO], T) :-
  I < N, 
  NextI is I+1,
  trans(S,I,NextS),
  emit(NextS,I,V),
  kf_part(NextI, N, NextS,RO, T).

kf_part(N, N, S, [],S).

trans(S,_I,NextS) :-
  {NextS =:= E + S},
  msw(trans_err,E).

emit(NextS,_I,V) :-
  {V =:= NextS +X},
  msw(obs_err,X).

values(init,real).
values(trans_err,real).
values(obs_err,real).

:- set_sw(init, norm(0,1)).
:- set_sw(trans_err,norm(0,2)).
:- set_sw(obs_err,norm(0,1)).

% prior as in Russel and Norvig 2010, Fig 15.10
% transition noise as in Russel and Norvig 2010, Fig 15.10
% observation noise as in Russel and Norvig 2010, Fig 15.10

:- end_lpad.



hist(Samples,NBins,Chart):-
  mc_sample_arg(kf(1,_O1,Y),Samples,Y,L0),
  histogram(L0,NBins,Chart).
% plot the density of the state at time 1 in case of no observation (prior)
% and in case of observing 2.5.
% Observation as in Russel and Norvig 2010, Fig 15.10

/** <examples>
?- hist(1000,40,G).
% plot the density of the state at time 1 in case of no observation
% by taking 1000 samples and dividing the domain
% in 40 bins

*/
 
