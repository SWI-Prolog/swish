/*
Probabilistic contect-free grammar implemented as a Stochastic Logic Program.
The grammar
0.2:S->aS
0.2:S->bS
0.3:S->a
0.3:S->b
can be represented with the SLP
0.2::s([a|R]):-
  s(R).
0.2::s([b|R]):-
  s(R).
0.3::s([a]).
0.3::s([b]).
SLPs define a distribution over the argument of goals: the above program 
defines a distribution over the values of S in for which s(S) succeeds.
This SLP can be written as an LPAD/ProbLog program by 
recalling that in SLPs the probabilities of all rules with the same head predicate sum to one and define a mutually exclusive choice on how to continue a proof.
Furthermore, repeated choices are independent, i.e., no stochastic memoization
is done. Therefore, a counter argument is added to the predicate s/1 to
keep track of the derivation step.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

s_as(N):0.2;s_bs(N):0.2;s_a(N):0.3;s_b(N):0.3.
s([a|R],N0):-
  s_as(N0),
  N1 is N0+1,
  s(R,N1).
s([b|R],N0):-
  s_bs(N0),
  N1 is N0+1,
  s(R,N1).
s([a],N0):-
  s_a(N0).
s([b],N0):-
  s_b(N0).

s(L):-s(L,0).
:- end_lpad.

/** <examples>
?- mc_sample_arg(s(S),100,S,L).
% sample 100 sentences from the language
?- mc_sample_arg_bar(s(S),100,S,L).
% sample 100 sentences from the language and draw a bar chart




*/

