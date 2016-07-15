/*
Probabilistic contect-free grammar.
0.2:S->aS
0.2:S->bS
0.3:S->a
0.3:S->b
From
 Taisuke Sato and Keiichi Kubota. Viterbi training in PRISM. 
Theory and Practice of Logic Programming,  doi:10.1017/S1471068413000677. 
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.
% pcfg(LT): LT is string of terminals accepted by the grammar
% pcfg(L,LT,LT0) L is a tring of terminals and not terminals that derives
% the list of terminals in LT-LT0

pcfg(L):- pcfg(['S'],[],_Der,L,[]).
% L is accepted if it can be derived from the start symbol S and an empty
% string of previous terminals

pcfg([A|R],Der0,Der,L0,L2):-
  rule(A,Der0,RHS),
  pcfg(RHS,[rule(A,RHS)|Der0],Der1,L0,L1),
  pcfg(R,Der1,Der,L1,L2).
% if there is a rule for A (i.e. it is a non-terminal), expand A using the rule
% and continue with the rest of the list

pcfg([A|R],Der0,Der,[A|L1],L2):-
  \+ rule(A,_,_),
  pcfg(R,Der0,Der,L1,L2).
% if A is a terminal, move it to the output string

pcfg([],Der,Der,L,L).
% there are no more symbols to expand

rule('S',Der,['S','S']):0.4; rule('S',Der,[a]):0.3; 
  rule('S',Der,[b]):0.3.

% encodes the three rules of the grammar

:- end_lpad.

/** <examples>
?- mc_prob(pcfg([a]),Prob).
% expected result ~ 0.2986666666666667.

?- mc_prob(pcfg([b]),Prob).
% expected result ~ 0.2976666666666667.

?- mc_prob(pcfg([a,a]),Prob).
% expected result ~ 0.035666666666666666.

?- mc_prob(pcfg([b,b]),Prob).
% expected result ~ 0.036833333333333336.

?- mc_prob(pcfg([a,b]),Prob).
% expected result ~ 0.035833333333333335.

?- mc_prob(pcfg([a,b,a]),Prob).
% expected result ~ 0.009.

?- mc_sample(pcfg([a,a]),1000,T,F,Prob). % take 1000 samples of pcfg([a,a])

?- mc_sample_bar(pcfg([a,a]),1000,Chart). % take 1000 samples of pcfg([a,a])

?- mc_sample_arg(pcfg(S),20,S,Values). % take 20 samples of S in 
% findall(S,pcfg(S),L)

?- mc_sample_arg_bar(pcfg(L),20,L,Chart). % take 20 samples of S in 
% findall(S,pcfg(S),L)

*/

