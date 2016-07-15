/*
Model checking of the Synchronous Leader Election Protocol expressed in 
Probabilistic Computation Tree Logic (PCTL).

Given a synchronous ring of N processes the protocol is used 
to elect a leader (a uniquely designated processor) by sending messages around 
the ring.

The protocol proceeds in rounds and is parametrised by a constant K. Each round 
begins by all processors (independently) choosing a random number (uniformly) 
from {1,...,K} as an id. The processors then pass their ids around the ring. 
If there is a unique id, then the processor with the maximum unique id is 
elected the leader, and otherwise the processors begin a new round.

With this program you can 
- check that the probability of eventually electing a leader is 1
- compute the probability of electing a leader within a certain 
  number of rounds
- compute the expected number of rounds to elect a leader
- graph the probability of electing a leader within L rounds as a function of L
- graph the expected number of rounds to elect a leader as a function of the 
  number of processes or of K
From
Gorlin, Andrey, C. R. Ramakrishnan, and Scott A. Smolka. "Model checking with probabilistic tabled logic programming." Theory and Practice of Logic Programming 12.4-5 (2012): 681-700.
This program was kindly provided by Andrey Gorlin and adapted to MCINTYRE
by Fabrizio Riguzzi.
See also http://www.prismmodelchecker.org/casestudies/synchronous_leader.php
*/

/** <examples>
% see http://www.prismmodelchecker.org/casestudies/synchronous_leader.php
% What is the probability that eventually a leader is elected?
?- mc_sample(eventually(elect),100,P).
% expected result 1

% What is the probability of electing a leader within 3 rounds?
?- mc_sample(bounded_eventually(elect,3),1000,P).
% expected result 0.97

% What is the expected number of rounds to elect a leader?
?- mc_expectation(eventually(elect,T),1000,T,E).
% expected result 1.2


?- graph_prob(G). 
% graph the probability of electing a leader within L rounds as
% a function of L

?- graph_exp_rounds_n(G).
% graph the expected number of rounds to elect a leader as a
% funtion of the number of processes when K=3

?- graph_exp_rounds_k(G).
% graph the expected number of rounds to elect a leader as a
% funtion of K when N=3

?- network_topology(G).
% draw a graph representing the network topology
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.
:- dynamic kr/1,num/1.
:- mc.

:- begin_lpad.

% State Formulae 
models(_S, tt,_Hist,_Limit,_Time).
models(S, prop(P),_Hist,_Limit,_Time) :-
	proposition(P, S).
models(S, and(F1, F2),Hist,Limit,Time) :-
	models(S, F1,Hist,Limit,Time), models(S, F2,Hist,Limit,Time).
models(S, or(F1, _F2),Hist,Limit,Time) :-
	models(S, F1,Hist,Limit,Time).
models(S, or(F1, F2),Hist,Limit,Time) :-
	\+ models(S, F1,Hist,Limit,Time),
	models(S, F2,Hist,Limit,Time).
models(S, not(F), Hist,Limit,Time) :-
	\+ models(S, F,Hist,Limit,Time).
models(S, prob_until(comp(Op, P), F1, F2),Hist,Limit,Time) :-
	mc_sample(pmodels(S, until(F1, F2),Hist,Limit,Time),20, Q),
	comp(Q, Op, P).
models(S, prob_next(comp(Op, P), F),Hist,Limit,Time) :-
	mc_sample(pmodels(S, next(F),Hist,Limit,Time),20, Q),
	comp(Q, Op, P).

comp(Q,>,P):-
  Q>P.

comp(Q,>=,P):-
  Q>=P.

comp(Q,<,P):-
  Q<P.

comp(Q,=<,P):-
  Q=<P.


% Path Formulae
pmodels(S,F):-
  pmodels(S,F,[],nolimit,0,_Time).

pmodels(S,F,Hist,Limit,Time):-
  pmodels(S,F,Hist,Limit,Time,_Time).

pmodels(S, until(_F1, F2),Hist,Limit,Time,Time) :-
	models(S, F2,Hist,Limit,Time),!.
	
pmodels(S, until(F1, F2),Hist0,Limit,Time0,Time) :-
	within_limit(Time0,Limit),
	models(S, F1,Hist0,Limit,Time0),
	ctrans(S, _, T, Hist0, Hist),!,
	Time1 is Time0+1,
	pmodels(T, until(F1,F2),Hist,Limit,Time1,Time).

pmodels(S, next(F), Hist,Limit,Time0,Time) :-
	within_limit(Time0,Limit),
	ctrans(S, _, T, Hist, _),!,
	Time is Time0+1,
	models(T, F,Hist,Limit,Time).

within_limit(_Time,nolimit):-!.

within_limit(Time,Limit):-
  Time<Limit.

bounded_eventually(Prop,Rounds):-
  num(N),
  B is Rounds*(N+1),
  eventually(Prop,B,_T).

eventually(Prop):-
  eventually(Prop,_T).

eventually(Prop,Rounds):-
  eventually(Prop,nolimit,T),
  num(N),
  Rounds is T/(N+1).

eventually(Prop,Limit,T) :-
	init(S),
	pctlspec(Prop, F),
	pmodels(S, F,[],Limit,0,T).


pctlspec(X, until(tt, prop(X))).
proposition(P, S) :- final(P, S).

final(elect, [_|L]) :-
	num(N),
	gen_elected_state(N, L).

gen_elected_state(J, L) :-
	(J==0
	->    L=[]
	;     L = [state(3,_,_,_)|Rest],
	      J1 is J-1,
	      gen_elected_state(J1,Rest)
	).
	

% transitions
% module counter
% [read] c<N-1 -> (c'=c+1);
% reading
trans(counter, counter(C), read, counter(D),_S,H,H) :-
  num(N),
  C < N-1,
  D is C+1.

% [read] c=N-1 -> (c'=c);
% finished reading
trans(counter, counter(C), read, counter(C),_S,H,H) :-
  num(N),
  C =:= N-1.

% [done] u1 | u2 | u3 | u4 -> (c'=c);
% done
trans(counter, counter(C), done, counter(C),S,H,H) :-
  get_processid(P), 
  nonlocal(process(P,_), uniqueid, 1,S).
   

% [retry] !(u1 | u2 | u3 | u4) -> (c'=1);
% pick again reset counter 
trans(counter, counter(_C), retry, counter(1),S,H,H) :-
        findall(P,get_processid(P),PL),
	maplist(nl(S),PL).

% [loop] s1=3 -> (c'=c);
% loop (when finished to avoid deadlocks)
trans(counter, counter(C), loop, counter(C),S,H,H) :-
  nonlocal(process(1,_), state, 3,S).

% module process
% local state
% s1=0 make random choice
% s1=1 reading
% s1=2 deciding
% s1=3 finished

% [pick] s1=0 -> 1/K : (s1'=1) & (p1'=0) & (v1'=0) & (u1'=true) + ...;
% pick value
trans(process(_N,_Next), state(0,_,_,_), pick, state(1,1,R,R),_S,H,[pick(R)|H]) :-
  pick(H,R).

%read 
% [read] s1=1 &  u1 & !p1=v2 & c<N-1 -> (u1'=true) & (v1'=v2);
trans(process(_N,Next), state(1,1,_,P), read, state(1,1,V,P),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C < CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P \== V.

% [read] s1=1 &  u1 &  p1=v2 & c<N-1 -> (u1'=false) & (v1'=v2) & (p1'=0);
trans(process(_N,Next), state(1,1,_,P), read, state(1,0,V,0),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C < CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P == V.

% [read] s1=1 & !u1 &  c<N-1 -> (u1'=false) & (v1'=v2);
trans(process(_N,Next), state(1,0,_,P), read, state(1,0,V,P),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C < CN - 1,
  nonlocal(process(Next,_), value, V,S).
 
% read and move to decide 
% [read] s1=1 &  u1 & !p1=v2 & c=N-1 -> (s1'=2) & (u1'=true) & (v1'=0) & (p1'=0);
trans(process(_N,Next), state(1,1,_,P), read, state(2,1,0,0),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C =:= CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P \== V.

% [read] s1=1 &  u1 &  p1=v2 & c=N-1 -> (u1'=false) & (v1'=0) & (p1'=0);
trans(process(_N,Next), state(1,1,_,P), read, state(2,0,0,0),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C =:= CN - 1,
  nonlocal(process(Next,_), value, V,S),
  P == V.

% [read] s1=1 & !u1 &  c=N-1 -> (s1'=2) & (u1'=false) & (v1'=0);
trans(process(_N,_Next), state(1,0,_,P), read, state(2,0,0,P),S,H,H) :-
  nonlocal(counter, counter, C,S),
  num(CN),
  C =:= CN - 1.

% done
% [done] s1=2 -> (s1'=3) & (u1'=false) & (v1'=0) & (p1'=0);
trans(process(_N,_Next), state(2,_,_,_), done, state(3,0,0,0),_S,H,H).

% retry
% [retry] s1=2 -> (s1'=0) & (u1'=false) & (v1'=0) & (p1'=0);
trans(process(_N,_Next), state(2,_,_,_), retry, state(0,0,0,0),_S,H,H).

% loop (when finished to avoid deadlocks)
% [loop] s1=3 -> (s1'=3);
trans(process(_N,_Next), state(3,U,V,P), loop, state(3,U,V,P),_S,H,H).

pick(H,V):-
  kr(K),
  K1 is K-1,
  PH is 1/K,
  findall(I,between(0,K1,I),L),
  foldl(pick_value(H,PH),L,(1,_),(_,V)).

pick_value(_H,_PH,_I,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_value(H,PH,I,(P0,V0),(P1,V1)):-
  var(V0),
  PF is PH/P0,
  (pick_fact(H,V0,PF)->
    P1=PF,
    V1=I
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

pick_fact(_,_,P):P.

%pick(H,0):0.5; pick(H,1):0.5.

ctrans(S, A, T, Hi, Ho) :-
	config(P),
	find_matching_trans(P,S,S,[],A,T,Hi,Ho).

find_matching_trans([], [], _CS, _PA, A, [], H,H) :- nonvar(A).
find_matching_trans([P|Ps], [S|Ss], CS, PA, A, [T|Ts],Hi,Ho) :-
	pick_trans(P, S, CS, PA, A, T, Hi,H1),
	find_matching_trans(Ps, Ss, CS, PA, A, Ts,H1,Ho).
find_matching_trans([P|Ps], [S|Ss], CS, PA, A, [S|Ts], Hi,Ho) :-
	% skip current process; but then all transitions enabled in the current
	% process will be prohibited for selection in later processes.
	enabled_trans(P,L),
	(nonvar(A) -> \+ member(A,L); true),
	append(L, PA, PA1),
	find_matching_trans(Ps, Ss, CS, PA1, A, Ts, Hi, Ho).

pick_trans(P, S, CS, PA, A, T, H0,H) :-
	etrans(P, S, PA, A, T,CS, H0,H).

etrans(P, S, PA, A, T, CS,H0,H) :-
	trans(P, S, A, T,CS,H0,H),
	A \= epsilon,
	\+ member(A, PA).

enabled_trans(P, L) :-
	setof(A, enabled_trans_in_process(P,A), L).

enabled_trans_in_process(P,A) :-
	clause(trans(P,_,A,_,_,_,_),_),
	A \= epsilon.

nonlocal(Proc, Var, Val,CS) :-
	getpid(Proc, Var, Pid, Idx),
	nth1(Pid, CS, State),
	arg(Idx, State, Val).
%	writeln(nonlocal_read(Proc, State, Var, Val)).

getpid(Proc, Var, Pid, Idx) :-
	config(Config),
	nth1(Pid, Config, Proc),
	nonlocal_access(Proc, Var, Idx).

get_processid(P):-
  num(N),
  between(1,N,P).

config([counter| L]) :-
	findall(P,get_processid(P),PL),
	maplist(neighbor,PL,L).

neighbor(I,process(I,J)) :-
	num(N),
	(I<N
	->  J is I+1
	;   J = 1
	).

%config([counter, process(1,2), process(2,3), process(3,4), process(4,1)]).

init(S) :-
	config(P),
	maplist(init_state,P,S).

init_state(counter, counter(1)).
init_state(process(_,_), state(0,0,0,0)).

nonlocal_access(counter, counter, 1).
nonlocal_access(process(_,_), state, 1).
nonlocal_access(process(_,_), uniqueid, 2).
nonlocal_access(process(_,_), value, 3).

nl(S,P):-
  nonlocal(process(P, _), uniqueid, 0,S).

num(4).
kr(4).


:- end_lpad.

network_topology(digraph(G)):-
  config([_|L]),
  maplist(to_edge,L,G).

to_edge(process(A,B),edge(A -> B,[])).

graph_prob(G):-
  retract(num(N)),
  retract(kr(K)),
  assert(num(4)),
  assert(kr(2)),
  findall(L-P,
    (between(1,6,L),mc_sample(bounded_eventually(elect,L),100,P)),LV),
  G=c3{data:_{x:x, rows:[x-'Probability of leader elected within L rounds (N=4, K=2)'|LV]},%legend:_{show: false},
    axis:_{x:_{min:1,max:6,label:'L',padding:_{bottom:0.0,top:0.0}},
%        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}},
           y:_{label:'Probability',padding:_{bottom:0.0,top:0.0}}}},
%        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}}},
  retract(num(4)),
  retract(kr(2)),
  assert(num(N)),
  assert(kr(K)).

graph_exp_rounds_n(G):-
  retract(num(NI)),
  retract(kr(KI)),
  assert(kr(3)),
  findall(N-E,
    (between(3,8,N),
     assert(num(N)),
     mc_expectation(eventually(elect,T),100,T,E),
     retract(num(N))),
    LV),
  G=c3{data:_{x:x, rows:[x-'Expected rounds to elect a leader (K=3)'|LV]},%legend:_{show: false},
    axis:_{x:_{min:3,max:8,label:'N',padding:_{bottom:0.0,top:0.0}},
           y:_{label:'Expected rounds',padding:_{bottom:0.0,top:0.0}}}},
  retract(kr(3)),
  assert(num(NI)),
  assert(kr(KI)).

graph_exp_rounds_k(G):-
  retract(num(NI)),
  retract(kr(KI)),
  assert(num(3)),
  findall(K-E,
    (between(3,8,K),
     assert(kr(K)),
     mc_expectation(eventually(elect,T),500,T,E),
     retract(kr(K))),
    LV),
  G=c3{data:_{x:x, rows:[x-'Expected rounds to elect a leader (N=3)'|LV]},%legend:_{show: false},
    axis:_{x:_{min:3,max:8,label:'K',padding:_{bottom:0.0,top:0.0}},
           y:_{label:'Expected rounds',padding:_{bottom:0.0,top:0.0}}}},
  retract(num(3)),
  assert(num(NI)),
  assert(kr(KI)).



