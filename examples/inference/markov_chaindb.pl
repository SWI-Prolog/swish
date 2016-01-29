/*
Model checking of a Markov chain: we want to know what is the likelihood 
that on an execution of the chain from a start state s, a final state t 
will be reached?
From
Gorlin, Andrey, C. R. Ramakrishnan, and Scott A. Smolka. "Model checking with probabilistic tabled logic programming." Theory and Practice of Logic Programming 12.4-5 (2012): 681-700.
*/

:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- set_pita(depth_bound,true).
:- set_pita(depth,5).


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

?- prob(reach(s0,0,s0),P).
% expecte result ~ 1.

?- prob(reach(s0,0,s1),P).
% expecte result ~ 0.5984054054054054.

?- prob(reach(s0,0,s2),P).
% expecte result ~ 0.4025135135135135.

?- prob(reach(s0,0,s3),P).
% expecte result ~ 0.5998378378378378.

?- prob(reach(s0,0,s4),P).
% expecte result ~ 0.49948717948717947.

?- prob(reach(s1,0,s0),P).
% expecte result ~ 0.

*/

