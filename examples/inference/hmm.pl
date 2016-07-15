/* 
Hidden Markov model for modeling DNA sequences. 
The model has three states, q1, q2 and end, and four output symbols, 
a, c, g, and t, corresponding to the four nucleotides (letter).
From
Christiansen, H. and Gallagher, J. P. 2009. Non-discriminating arguments and 
their uses. In International Conference on Logic Programming. LNCS, 
vol. 5649. Springer, 55-69.

*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- begin_lpad.

% hmm(O): O is the output sequence 
% hmm1(S,O): O is the output sequence and S is the sequence of states
% hmm(Q,S0,S,O):  from state Q and previous state S0, generates output O and
% sequence of states S

hmm(O):-hmm1(_,O).
% O is an output sequence if there is a state seuqnece S such that hmm1(S,O) 
% holds

hmm1(S,O):-hmm(q1,[],S,O).
% O is an output sequence and S a state sequence if the chain stats at state
% q1 and ends generating state sequence S and output sequence O

hmm(end,S,S,[]).
% an HMM in state end terminates the sequence without emitting any symbol

hmm(Q,S0,S,[L|O]):-
	Q\= end,
	next_state(Q,Q1,S0),
	letter(Q,L,S0),
	hmm(Q1,[Q|S0],S,O).
% an HMM in state Q different from end goes in state Q1, emits the letter L 
% and continues the chain

next_state(q1,q1,S):1/3;next_state(q1,q2,S):1/3;next_state(q1,end,S):1/3.
% from state q1 the HMM can go to q1, q2 or end with equal probability

next_state(q2,q1,S):1/3;next_state(q2,q2,S):1/3;next_state(q2,end,S):1/3.
% from state q2 the HMM can go to q1, q2 or end with equal probability


letter(q1,a,S):0.25;letter(q1,c,S):0.25;letter(q1,g,S):0.25;letter(q1,t,S):0.25.
% from state q1 the HMM emits one of the letters with equal probability

letter(q2,a,S):0.25;letter(q2,c,S):0.25;letter(q2,g,S):0.25;letter(q2,t,S):0.25.
% from state q1 the HMM emits one of the letters with equal probability

:- end_lpad.


state_diagram(digraph(G)):-
    findall(edge(A -> B,[label=P]),
      (clause(next_state(A,B,_,_,_),
        (get_var_n(_,_,_,Probs,_),equality(_,_,N,_))),
        nth0(N,Probs,P)),
      G).
/** <examples>

?- prob(hmm([a,c]),Prob). % what is the probability that the model emits the sequence [a,c])
% expected result 0.01388888888888889
?- prob_bar(hmm([a,c]),Prob). % what is the probability that the model emits the sequence [a,c])
% expected result 0.01388888888888889

?- state_diagram(G).
% show the state diagram

*/
 
