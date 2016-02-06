/*
Model checking of a Markov chain: we want to know what is the likelihood 
that on an execution of the chain from a start state s, a final state t 
will be reached?
From
Gorlin, Andrey, C. R. Ramakrishnan, and Scott A. Smolka. "Model checking with probabilistic tabled logic programming." Theory and Practice of Logic Programming 12.4-5 (2012): 681-700.
*/


:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.
% reach(S, I, T) starting at state S at instance I,
%   state T is reachable.
reach(S, I, T) :-
  trans(S, I, U),
  reach(U, next(I), T).

reach(S, _, S).

% trans(S,I,T) transition from S at instance I goes to T

trans(s0,S,s0):0.5; trans(s0,S,s1):0.3; trans(s0,S,s2):0.2.

trans(s1,S,s1):0.4; trans(s1,S,s3):0.1; trans(s1,S,s4):0.5.

trans(s4,_,s3).
:- end_lpad.



/** <examples>

?- mc_prob(reach(s0,0,s0),P).
% expected result ~ 1.

?- mc_prob(reach(s0,0,s1),P).
% expected result ~ 0.5984054054054054.

?- mc_prob(reach(s0,0,s2),P).
% expected result ~ 0.4025135135135135.

?- mc_prob(reach(s0,0,s3),P).
% expected result ~ 0.5998378378378378.

?- mc_prob(reach(s0,0,s4),P).
% expected result ~ 0.49948717948717947.

?- mc_prob(reach(s1,0,s0),P).
% expected result ~ 0.

?- mc_sample(reach(s0,0,s1),1000,T,F,P).
% expected result ~ 0.5984054054054054.

?- mc_sample_bar(reach(s0,0,s1),1000,Chart).

?- mc_sample_arg(reach(s0,0,S),50,S,Values). 
% take 50 samples of L in findall(S,reach(s0,0,S),L)

?- mc_sample_arg_bar(reach(s0,0,S),50,S,Chart). 
% take 50 samples of L in findall(S,reach(s0,0,S),L)

?- mc_sample_arg_first(reach(s0,0,S),50,S,Values). 
% take 50 samples of the first value returned for S in reach(s0,0,S)

?- mc_sample_arg_first_bar(reach(s0,0,S),50,S,Chart).
% take 50 samples of the first value returned for S in reach(s0,0,S)

*/

