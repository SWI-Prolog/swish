/*
A PLCG is a probabilistic version of left-corner grammar which uses the same set
of CFG rules as a PCFG but models a shift-reduce parser with probabilities assigned to shift-reduce operations.
From
Sato, Taisuke, Yoshitaka Kameya, and Kenichi Kurihara. "Variational Bayes via propositionalized probability computation in PRISM." Annals of Mathematics and Artificial Intelligence 54.1-3 (2008): 135-158.
*/


:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.
% grammar
% S->SS
% S->a
% S->b
plc(Ws) :- g_call(['S'] ,Ws,[], [],_Der).

g_call([],L,L,Der,Der). 

g_call([G|R], [G|L],L2,Der0,Der) :- % shift
  terminal(G),
  g_call(R,L,L2,Der0,Der).

g_call([G|R], [Wd|L] ,L2,Der0,Der) :-
  \+ terminal(G),
  first(G,Der0,Wd),
  lc_call(G,Wd,L,L1,[first(G,Wd)|Der0],Der1),
  g_call(R,L1,L2,Der1,Der).

lc_call(G,B,L,L1,Der0,Der) :- % attach
  lc(G,B,Der0,rule(G, [B|RHS2])),
  attach_or_project(G,Der0,attach),
  g_call(RHS2,L,L1,[lc(G,B,rule(G, [B|RHS2])),attach|Der0],Der).

lc_call(G,B,L,L2,Der0,Der) :- % project 
  lc(G,B,Der0,rule(A, [B|RHS2])),
  attach_or_project(G,Der0,project),
  g_call(RHS2,L,L1,[lc(G,B,rule(A, [B|RHS2])),project|Der0],Der1),
  lc_call(G,A,L1,L2,Der1,Der).

lc_call(G,B,L,L2,Der0,Der) :- 
  \+ lc(G,B,Der0,rule(G,[B|_])),
  lc(G,B,Der0,rule(A, [B|RHS2])),
  g_call(RHS2,L,L1,[lc(G,B,rule(A, [B|RHS2]))|Der0],Der1),
  lc_call(G,A,L1,L2,Der1,Der).

attach_or_project(A,Der,Op) :-
  lc(A,A,Der,_),
  attach(A,Der,Op).

attach_or_project(A,Der,attach) :-
  \+ lc(A,A,Der,_).


lc('S','S',_Der,rule('S',['S','S'])). 

lc('S',a,_Der,rule('S',[a])).

lc('S',b,_Der,rule('S',[b])). 

first('S',Der,a):0.5; first('S',Der,b):0.5.


attach('S',Der,attach):0.5; attach('S',Der,project):0.5.

terminal(a).
terminal(b).



:- end_lpad.



/** <examples>

?- mc_prob(plc([a,b]),P). % what is the probability of sentence [a,b]?
% expecte result ~  0.031

?- mc_sample(plc([a,b]),1000,T,F,P). % take 1000 samples of plc([a,b])

?- mc_sample_bar(plc([a,b]),1000,Chart). % take 1000 samples of plc([a,b])

?- mc_sample_arg(plc(S),20,S,Values). % take 20 samples of L in 
% findall(S,pls(S),L)

?- mc_sample_arg_bar(plc(L),20,L,Chart). % take 20 samples of L in 
% findall(S,pls(S),L)
**/

