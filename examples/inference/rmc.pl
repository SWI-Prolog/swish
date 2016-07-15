/*
Model checker for Recursive Markov Chains expressed in 
generalized probabilistic logic (GPL).
GPL is an expressive logic based on the modal mu-calculus for probabilistic 
systems.
GPL is designed for model checking reactive probabilistic transition systems 
(RPLTS).
From
Gorlin, Andrey, C. R. Ramakrishnan, and Scott A. Smolka. "Model checking with probabilistic tabled logic programming." Theory and Practice of Logic Programming 12.4-5 (2012): 681-700.
This program was kindly provided by Andrey Gorlin and adapted to MCINTYRE 
by Fabrizio Riguzzi.
*/

/** <examples>
?- mc_sample(models(en(0), form(x), []),20,P).
% expected result 1
% The values for en can be 0+.
?- mc_sample(models(en(1), form(x), []),1, P).
% expected result 1
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- dynamic kr/1,num/1.
:- mc.

:- begin_lpad.

% models(S,F,H,W)
%   S: is a state of an RPLTS
%   F: is a GPL formula
%   H: is the history of actions (list)
models(_S, tt, _).
models(S, form(X), H) :-
	fdef(X, F),
	models(S, F, H).
models(S, and(F1,F2), H) :-
	models(S, F1, H),
	models(S, F2, H).
models(S, or(F1,F2), H) :-
	models(S, F1, H);
	models(S, F2, H).
models(S, diam(A, F), H) :-
	trans(S, A, SW),
	msw(SW, H, T),
	models(T, F, [T,SW|H]).
models(S, box(A, F), H) :-
	findall(SW, trans(S,A,SW), L),
	all_models(L, S, F, H).

all_models([], _, _, _H).
all_models([SW|Rest], S, F, H) :-
	msw(SW, H, T),
	models(T,F,[T,SW|H]),
	all_models(Rest, S, F, H).

% Prob. formulas of GPL are omitted
% Negated formulas are omitted


% 1-exit RMC example for the slow-converging x = 1/2 (1 + x^2)
% equation, scaled to repeat it multiple times.

fdef(x, or(diam(e, tt),
           or(diam(p, form(x)),
              and(diam(c, form(x)), diam(r, form(x)))))).

trans(en(N), p, en(N, p)).
trans(ex(N), e, ex(N, e)).
trans(l1x(N), p, l1x(N, p)).
trans(l1e(N), c, l1e(N, c)).
trans(l1e(N), r, l1e(N, r)).
trans(l2x(N), p, l2x(N, p)).
trans(l2e(N), c, l2e(N, c)).
trans(l2e(N), r, l2e(N, r)).
trans(l3x(N), p, l3x(N, p)).
trans(l3e(N), c, l3e(N, c)).
trans(l3e(N), r, l3e(N, r)).

values(en(0, p), [l1e(0), ex(0)]).
values(en(N, p), [l1e(N), l3e(N)]) :- N>0.

values(ex(N, e), [ex(N)]).

values(l1x(N, p), [l2e(N)]).
values(l1e(N, c), [en(N)]).
values(l1e(N, r), [l1x(N)]).

values(l2x(0, p), [ex(0)]).
values(l2x(N, p), [l3e(N)]) :- N>0.

values(l2e(N, c), [en(N)]).
values(l2e(N, r), [l2x(N)]).

values(l3x(N, p), [ex(N)]).

values(l3e(N, c), [en(M)]) :-
        N>0,
        M is N-1.

values(l3e(N, r), [l3x(N)]).

set_sw(en(_N, p), [0.5, 0.5]).
set_sw(ex(_N, e), [1]).
set_sw(l1x(_N, p), [1]).
set_sw(l1e(_N, c), [1]).
set_sw(l1e(_N, r), [1]).
set_sw(l2x(_N, p), [1]).
set_sw(l2e(_N, c), [1]).
set_sw(l2e(_N, r), [1]).
set_sw(l3x(_N, p), [1]).
set_sw(l3e(_N, c), [1]).
set_sw(l3e(_N, r), [1]).



msw(SW,H,V):-
  values(SW,L),
  append(L0,[LastV],L),
  set_sw(SW,PH),
  append(PH0,[_LastP],PH),
  foldl(pick_value(SW,H),PH0,L0,(1,_),(_,V)),
  (var(V)->  
    V=LastV
  ;
    true
  ).

pick_value(_SW,_H,_PH,_I,(P0,V0),(P0,V0)):-
  nonvar(V0).

pick_value(SW,H,PH,I,(P0,V0),(P1,V1)):-
  var(V0),
  PF is PH/P0,
  (prob_fact(SW,H,I,PF)->
    P1=PF,
    V1=I
  ;
    P1 is P0*(1-PF),
    V1=V0
  ).

prob_fact(_,_,_,P):P.


:-end_lpad.

