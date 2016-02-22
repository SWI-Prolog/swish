/* 
1st and 2nd-order Hidden Markov model for part-of-speech tagging.
This program differs from http://cplint.lamping.unife.it/example/inference/hmmpos.pl because 
1. a 1st-order HMM and a 2nd-order HMM are included
2. the probabilistic predicates trans/3, trans2/4, out/3 and out2/4 are defined
intensionally 
3. the probability values are defined on the basis of frequency data from a 
(toy in this example) dataset
The states represent parts-of-speech, and the symbols emitted by the states are words.
In the 1st-order HMM, a word depends probabilistically on its own part-of-speech (i.e. its tag) which in turn depends on the part-of-speech of the preceding word (or on the start state in case there is no preceding word).
In the 2nd-order HMM, a word depends probabilistically on its own part-of-speech and the preceding tag which in turn depends on the part-of-speech of the two preceding words (or on the start state in case there are no preceding words).
From
http://stp.lingfil.uu.se/~nivre/docs/thesis3.pdf
Original program by Joakim Nivre and Torbjorn Lager, adapted to MCINTYRE by Fabrizio Riguzzi
*/
/** <examples>

?-  mc_sample_arg(hmm(S,['I',can,can,a,can]),1000,S,O).
% sample the state sequence corresonding to the phrase "I can can a can"
% the most frequent state sequence is an approximate POS tagging for the
% sentence. It corresponds to the Viterbi path of the HMM.
% expected result: the most frequent tagging should be [pn,vb,vb,dt,nn]
?- mc_sample_arg(hmm2(S,['I',can,can,a,can]),1000,S,O).
% as above but for the second order model
%
?- mc_sample_arg_bar(hmm(S,['I',can,can,a,can]),1000,S,O).
?- mc_sample_arg_bar(hmm2(S,['I',can,can,a,can]),1000,S,O).

*/

:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- mc.

:- begin_lpad.

% hmm(O): O is the output sequence 
% hmm(S,O): O is the output sequence and S is the sequence of states
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
	out(Q,L,S0),
	hmm(Q1,[Q|S0],S,O).
% an HMM in state Q goes in state Q1, emits the word L 
% and continues the chain

hmm(_,S,S,[]).
% an HMM in sny state terminates the sequence without emitting any symbol

% hmm2(O): O is the output sequence 
% hmm2(S,O): O is the output sequence and S is the sequence of states
% hmm2(Q,S0,S1,S,O):  from state Q and previous state S1, generates output O and
% sequence of states S

hmm2(O):-hmm2(_,O).
% O is an output sequence if there is a state seuqnece S such that hmm1(S,O) 
% holds

hmm2(S,O):-trans2(start,start,Q0,[]),hmm2(start,Q0,[],S0,O),reverse(S0,S).
% O is an output sequence and S a state sequence if the chain stats at state
% q1 and ends generating state sequence S and output sequence O

hmm2(Q0,Q,S0,S,[L|O]):-
	trans2(Q0,Q,Q1,S0),
	out2(Q0,Q,L,S0),
	hmm2(Q,Q1,[Q|S0],S,O).
% an HMM in state Q goes in state Q1, emits the word L 
% and continues the chain

hmm2(_,_,S,S,[]).
% an HMM in sny state terminates the sequence without emitting any symbol

trans(S0,S1,H):-
  findall((S,P),pc_c(S,S0,P),L),
  append(L0,[(LastS,_P)],L),
  foldl(pick_next_state(S0,H),L0,(1,_),(_,S1)),
  (var(S1)->  
    S1=LastS
  ;
    true
  ).

pick_next_state(_S0,_H,_L,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_next_state(S0,H,(S,P),(P0,V0),(P1,V1)):-
  var(V0),
  PF is P/P0,
  (prob_fact_state(S0,S,H,PF)->
    P1=PF,
    V1=S
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

prob_fact_state(_,_,_,P):P.

out(S0,W,H):-
  findall((W,P),pw_c(W,S0,P),L),
  append(L0,[(LastW,_P)],L),
  foldl(pick_word(S0,H),L0,(1,_),(_,W)),
  (var(W)->  
    W=LastW
  ;
    true
  ).

pick_word(_S0,_H,_L,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_word(S0,H,(W,P),(P0,V0),(P1,V1)):-
  var(V0),
  PF is P/P0,
  (prob_fact_word(S0,W,H,PF)->
    P1=PF,
    V1=W
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

prob_fact_word(_,_,_,P):P.

trans2(S0,S1,S2,H):-
  findall((S,P),pc_cc(S,S0,S1,P),L),
  append(L0,[(LastS,_P)],L),
  foldl(pick_next_state2(S0,S1,H),L0,(1,_),(_,S2)),
  (var(S2)->  
    S2=LastS
  ;
    true
  ).

pick_next_state2(_S0,_S1,_H,_L,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_next_state2(S0,S1,H,(S,P),(P0,V0),(P1,V1)):-
  var(V0),
  PF is P/P0,
  (prob_fact_state2(S0,S1,S,H,PF)->
    P1=PF,
    V1=S
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

prob_fact_state2(_,_,_,_,P):P.

out2(S0,S1,W,H):-
  findall((W,P),pw_cc(W,S0,S1,P),L),
  append(L0,[(LastW,_P)],L),
  foldl(pick_word2(S0,S1,H),L0,(1,_),(_,W)),
  (var(W)->  
    W=LastW
  ;
    true
  ).

pick_word2(_S0,_S1,_H,_L,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_word2(S0,S1,H,(W,P),(P0,V0),(P1,V1)):-
  var(V0),
  PF is P/P0,
  (prob_fact_word2(S0,S1,W,H,PF)->
    P1=PF,
    V1=W
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

prob_fact_word2(_,_,_,_,P):P.



:- end_lpad.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% con.mle.pl
%
% MLE CONTEXTUAL MODEL
%
% Maximum likelihood estimation of contextual probabilities
% P(c), P(c2|c1), P(c3|c1,c2).
%
% The definitions presuppose that the following
% frequencies are stored in the internal database:
%
% fc(C,F) - Frequency of class C
% fcc(C1,C2,F) - Joint frequency of classes C1 and C2
% fccc(C1,C2,C3) - Joint frequency of classes C1, C2 and C3
%
% NB: fc/2, fcc/3 and fccc/4 must also be defined for the dummy
% class start used to initialize the tagger. Thus, the
% following must be defined for all classes C1 and C2:
%
% fc(start,F).
% fcc(start,start,F).
% fcc(start,C1,F).
% fccc(start,start,C1,F).
% fccc(start,C1,C2,F).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% P(c)
pc(C,P) :-
  fc(C,F),
  tokens(N),
  P is F/N.

% P(c2|c1)
pc_c(C,start,P):-!,
  fc(C,F1),
  tokens(F2),
  P is F1/F2.

pc_c(C2,C1,P) :-
  fcc(C1,C2,F1),
  fc(C1,F2),
  P is F1/F2.

pc_c(C2,C1,0) :-
  \+ fcc(C1,C2,_).

pc_cc(C,start,start,P):-!,
  pc_c(C,start,P).

pc_cc(C,start,C1,P):-!,
  pc_c(C,C1,P).


% P(c3|c1,c2)
pc_cc(C3,C1,C2,P) :-
  fccc(C1,C2,C3,F1),
  fcc(C1,C2,F2),
  P is F1/F2.

pc_cc(C3,C1,C2,0) :-
  \+ fccc(C1,C2,C3,_).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% lex.mle.pl
%
% MLE LEXICAL MODEL
%
% Maximum likelihood estimation of lexical probabilities P(w|c).
% The definition presupposes that the following
% frequencies are stored in the internal database:
%
% fwc(W,C,F) - Joint frequency of word W and class C
% fc(C,F) - Frequency of class C
%
% It also presupposes that the set of open classes (i.e. classes
% allowed for unknown words) is defined by means of clauses of
% the form:
%
% open(C)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*pw_c(W,C,P) :-
  fwc(W,C,F1),
  fc(C,F2),
  P is F1/F2.
pw_c(W,C,0) :-
  \+ fwc(W,_,_),
  open(C).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% lex.uni.pl
%
% UNIFORM SMOOTHING OF LEXICAL MODEL
%
% For known words, the lexical probability is a standard
% maximum likelihood estimate:
%
% P(w|c) = f(w,c) / f(c)
%
% except that f(c) is adjusted by adding f(c)/n for open
% word classes.
%
% For unknown words, the lexical probability is 1/n
% for all open classes, where n is the number of tokens
% in the training corpus.
%
% The definition presupposes that the following
% frequencies are stored in the internal database:
%
% fwc(W,C,F) - Joint frequency of word W and class C
% fc(C,F) - Frequency of class C
%
% It also presupposes that the number of open classes
% (i.e. those allowed for unknown words) are defined by
% clauses of the following form:
%
% open(C)
%
% Finally, it presupposes that the total number of tokens
% in the training corpus is defined:
%
% tokens(N)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pw_c(W,C,P) :-
  fwc(W,C,F1),
  fc(C,F2),
  \+ open(C),
  P is F1/F2.

pw_c(W,C,P) :-
  fwc(W,C,F1),
  fc(C,F2),
  open(C),
  tokens(N),
  P is F1/(F2+(F2/N)).

pw_c(W,C,P) :-
  \+ fwc(W,_,_),
  open(C),
  tokens(N),
  P is 1/N.

pw_cc(W,C1,C2,P) :-
  fwcc(W,C1,C2,F1),
  hfreq(F),
  F1 > F,
  fcc(C1,C2,F2),
  P is F1/F2.

pw_cc(W,C1,C2,P) :-
  pw_c(W,C2,P),
%  fc(C1,_),
  \+ (fwcc(W,C1,C2,F1), hfreq(F), F1 > F).

open(ab).
open(in).
open(jj).
open(nn).
open(pc).
open(p).
open(rg).
open(uo).
open(vb).

hfreq(0).

/* Corpus
I/pn can/vb light/vb a/dt fire/nn and/cn you/pn
can/vb open/vb a/dt can/nn of/pp beans/nn ./dl
Now/ab the/dt can/nn is/vb open/jj ,/dl and/cn
we/pn can/vb eat/vb in/pp the/dt light/nn of/pp
the/dt fire/nn ./dl

Frequency Databases
*/
fwc('.', dl, 2).
fwc(',', dl, 1).
fwc(a, dt, 2).
fwc(and, cn, 2).
fwc(beans, nn, 1).
fwc(can, nn, 2).
fwc(can, vb, 3).
fwc(eat, vb, 1).
fwc(fire, nn, 2).
fwc('I', pn, 1).
fwc(in, pp, 1).
fwc(is, vb, 1).
fwc(light, nn, 1).
fwc(light, vb, 1).
fwc('Now', ab, 1).
fwc(of, pp, 2).
fwc(open, vb, 1).
fwc(open, jj, 1).
fwc(the, dt, 3).
fwc(we, pn, 1).
fwc(you, pn, 1).

fwcc('.', dl, nn,  2).
fwcc(',', jj, dl,1).
fwcc(a, vb, dt, 2).
fwcc(and, nn,cn,1).
fwcc(and, dl,cn, 1).
fwcc(beans, pp,nn, 1).
fwcc(can, dt,nn,2).
fwcc(can, pn,vb, 3).
fwcc(eat, vb,vb,1).
fwcc(fire, dt,nn, 2).
%fwcc('I', start,pn, 1).
fwcc(in, vb,pp, 1).
fwcc(is, nn,vb, 1).
fwcc(light, dt,nn, 1).
fwcc(light, vb, vb,1).
fwcc('Now', dl,ab, 1).
fwcc(of, nn,pp, 2).
fwcc(open, vb, vb,1).
fwcc(open, vb,jj,1).
fwcc(the, ab,dt, 1).
fwcc(the,pp, dt, 2).
fwcc(we, cn,pn,1).
fwcc(you, cn,pn, 1).


%fc(start,F):-
%  tokens(F).

fc(ab, 1).
fc(cn, 2).
fc(dl, 3).
fc(dt, 5).
fc(jj, 1).
fc(nn, 6).
fc(pn, 3).
fc(pp, 3).
fc(vb, 7).


%fcc(start,C1,F):-
%  fc(C1,F).


fcc(ab, dt, 1).
fcc(cn, pn, 2).
fcc(dl, ab, 1).
fcc(dl, cn, 1).
fcc(dt, nn, 5).
fcc(jj, dl, 1).
fcc(nn, cn, 1).
fcc(nn, dl, 2).
fcc(nn, pp, 2).
fcc(nn, vb, 1).
fcc(pn, vb, 3).
fcc(pp, dt, 2).
fcc(pp, nn, 1).
fcc(vb, dt, 2).
fcc(vb, jj, 1).
fcc(vb, pp, 1).
fcc(vb, vb, 3).


fccc(ab, dt, nn, 1).
fccc(cn, pn, vb, 2).
fccc(dl, ab, dt, 1).
fccc(dl, cn, pn, 1).
fccc(dt, nn, cn, 1).
fccc(dt, nn, dl, 1).
fccc(dt, nn, pp, 2).
fccc(dt, nn, vb, 1).
fccc(jj, dl, cn, 1).
fccc(nn, cn, pn, 1).
fccc(nn, dl, ab, 1).
fccc(nn, pp, dt, 1).
fccc(nn, pp, nn, 1).
fccc(nn, vb, jj, 1).
fccc(pn, vb, vb, 3).
fccc(pp, nn, dl, 1).
fccc(pp, dt, nn, 2).
fccc(vb, dt, nn, 2).
fccc(vb, jj, dl, 1).
fccc(vb, pp, d1, 1).
fccc(vb, vb, dt, 2).
fccc(vb, vb, pp, 1).

classes :-
  setof(C,F^fc(C,F),Cs),
  length(Cs,N),
  assert(classes(N)).

tokens :-
  bagof(F,W^C^fwc(W,C,F),Fs),
  sum_list(Fs,N),
  assert(tokens(N)).

types :-
  setof(W,C^F^fwc(W,C,F),Ws),
  length(Ws,N),
  assert(types(N)).

:- tokens.
:- types.
:- classes.

