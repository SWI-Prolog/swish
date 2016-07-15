/*
A prefix parser for a probabilistic left-corner grammar.
From
T. Sato, P. Meyer, Tabling for infinite probability computation, in:
Intnational Confence on Logic Programming, Vol. 17 of LIPIcs, 2012,
pp.  348-358.
T. Sato, P. Meyer, Infinite probability computation by cyclic explanation
graphs, Theory and Practice of Logic Programming 14 (2014) 909-937.
doi:10.1017/S1471068413000562.
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
pre_plc(Ws) :- g_call(['S'] ,Ws,[], [],_Der).

g_call([],L,L,Der,Der). 

g_call([G|R], [G|L],L2,Der0,Der) :- % shift
  terminal(G),
  g_call(R,L,L2,Der0,Der).

g_call([G|_R], [Wd|L] ,[],Der0,Der) :-
  \+ terminal(G),
  first(G,Der0,Wd),
  lc_call(G,Wd,L,[],[first(G,Wd)|Der0],Der). % pseudo success

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

?- mc_prob(pre_plc([a,b]),P). what is the probability of sentence [a,b]?
% expecte result ~ 0.0326


*/

