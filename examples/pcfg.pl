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
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- cplint.

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

rule('S',Der,[a,'S']):0.2; rule('S',Der,[b,'S']):0.2; rule('S',Der,[a]):0.3; rule('S',Der,[b]):0.3.
% encodes the three rules of the grammar

:- end_cplint.

/** <examples>

?- prob(pcfg([a,b,a,a]),Prob). % what is the probability that the string abaa belongs to the language?
% expected result 0.0024
?- prob_bar(pcfg([a,b,a,a]),Prob). % what is the probability that the string abaa belongs to the language?
% expected result 0.0024


*/

