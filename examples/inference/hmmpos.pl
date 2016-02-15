/* 
Hidden Markov model for part-of-speech tagging.
The states represent parts-of-speech, and the symbols emitted by the states are words. The assumption is that a word depends probabilistically on just its own part-of-speech (i.e. its tag) which in turn depends on the part-of-speech of the preceding word (or on the start state in case there is no preceding word).
From
http://www.ling.gu.se/~lager/Spaghetti/spaghetti.html
Original program by Torbjorn Lager, adapted to MCINTYRE by Fabrizio Riguzzi
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.

:- begin_lpad.

% hmm(O): O is the output sequence 
% hmm1(S,O): O is the output sequence and S is the sequence of states
% hmm(Q,S0,S,O):  from state Q and previous state S0, generates output O and
% sequence of states S

hmm(O):-hmm(_,O).
% O is an output sequence if there is a state seuqnece S such that hmm1(S,O) 
% holds

hmm(S,O):-trans(start,Q0,[]),hmm(Q0,[],S0,O),reverse(S0,S).
% O is an output sequence and S a state sequence if the chain stats at state
% q1 and ends generating state sequence S and output sequence O

hmm(Q,S0,S,[L|O]):-
	trans(Q,Q1,S0),
	out(L,Q,S0),
	hmm(Q1,[Q|S0],S,O).
% an HMM in state Q goes in state Q1, emits the word L 
% and continues the chain

hmm(_,S,S,[]).
% an HMM in sny state terminates the sequence without emitting any symbol


trans(start,det,_):0.30;
trans(start,aux,_):0.20;
trans(start,v,_):0.10;
trans(start,n,_):0.10;
trans(start,pron,_):0.30.

trans(det,det,_):0.20;
trans(det,aux,_):0.01;
trans(det,v,_):0.01;
trans(det,n,_):0.77;
trans(det,pron,_):0.01.

trans(aux,det,_):0.18;
trans(aux,aux,_):0.10;
trans(aux,v,_):0.50;
trans(aux,n,_):0.01;
trans(aux,pron,_):0.21.

trans(v,det,_):0.36;
trans(v,aux,_):0.01;
trans(v,v,_):0.01;
trans(v,n,_):0.26;
trans(v,pron,_):0.36.

trans(n,det,_):0.01;
trans(n,aux,_):0.25;
trans(n,v,_):0.39;
trans(n,n,_):0.34;
trans(n,pron,_):0.01.

trans(pron,det,_):0.01;
trans(pron,aux,_):0.45;
trans(pron,v,_):0.52;
trans(pron,n,_):0.01;
trans(pron,pron,_):0.01.

/*
out(a,det,_):0.300.
out(can,aux,_):0.010.
out(can,v,_):0.005.
out(can,n,_):0.001.
out(he,pron,_):0.070.
*/
out(a,det,_).
out(can,aux,_).
out(can,v,_).
out(can,n,_).
out(he,pron,_).


:- end_lpad.

state_diagram(digraph(G)):-
    setof(A,(B,S,Body)^
          clause(trans(A,B,S),Body),Nodes),
    maplist(nodelab,Nodes,NodesLab),
    findall(edge(A -> B,[label=P]),
      (clause(trans(A,B,_),
        (sample_head(_,_,Probs,_),_=N)),
        nth0(N,Probs,_:P)),
      Edges),
    append(NodesLab,Edges,G).

nodelab(N,node(N,[label=Lab])):-
    findall(W,clause(out(W,N,_),_),L),
    atomic_list_concat([N,'\nOut:\n'|L],Lab).


/** <examples>

?- mc_sample_arg(hmm(S,[he,can,can,a,can]),20,S,O).
% sample the state sequence corresonding to the phrase "he can can a can"
% the most frequent state sequence is an approximate POS tagging for the 
% sentence. It corresponds to the Viterbi path of the HMM.
% expected result: the most frequent tagging should be [pron, aux, v, det, n]
?- mc_sample_arg_bar(hmm(S,[he,can,can,a,can]),20,S,O).

?- state_diagram(G).
% show the state diagram
*/
 
