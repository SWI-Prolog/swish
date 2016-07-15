/*
Model checker for fuzzy formulas in generalized probabilistic logic (GPL).
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
?- mc_sample(models(s1(3), form(x(2)), []),10,P).
% expected result 1
% The values for s1 can be 0+, and for x from 2 to 7.
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


temporal(tabled_models(_,_,_)).

% This is a simple example with entanglement that can scale the
% formula and the model.  Here, t is essentially a step down.
%
% The formula x can take values from 2 to 7.  When it's at 2:
%
% x = (<b>x /\ <c>t) \/ (<b>t /\ <c>x) \/ (<b>t /\ <c>t),
%
% t = [a]x \/ <b>[a]x

% trans(s1(N), a, s1(N, a)).
% trans(s1(N), b, s1(N, b)).
% trans(s1(N), c, s1(N, c)).
% trans(s1(N), d, s1(N, d)).
trans(s1(N), A, s1(N, A)).
trans(s2(N), a, s2(N, a)) :- N>0.

% values(s1(_N, a), [s3]).
% values(s1(N, b), [s1(N), s2(N)]).
% values(s1(N, c), [s1(N), s2(N)]).
values(s1(_N, a), [s3]) :- !.
values(s1(N, _A), [s1(N), s2(N)]).
values(s2(N, a), [s1(M)]) :- M is N-1.

% set_sw(s1(_N, a), [1]).
% set_sw(s1(_N, b), [0.3, 0.7]).
% set_sw(s1(_N, c), [0.5, 0.5]).
set_sw(s1(_N, a), [1]) :- !.
set_sw(s1(_N, b), [0.2, 0.8]) :- !.
set_sw(s1(_N, _A), [0.1, 0.9]).
set_sw(s2(_N, a), [1]).

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

% Formulae:
letter(1, b).
letter(2, c).
letter(3, d).
letter(4, e).
letter(5, f).
letter(6, g).
letter(7, h).

ex_conj(1, N, F, diam(b, form(G))) :- !,
        (N==1
        -> G=F
        ;  G=t(F)
        ).
ex_conj(N, C, F, and(diam(L, form(G)), Fs)) :-
        M is N-1,
        letter(N, L),
        ex_conj(M, C, F, Fs),
        (N==C
        -> G=F
        ;  G=t(F)
        ).

ex_disj(N, 0, F, Fs) :- !,
        ex_conj(N, 0, F, Fs).
ex_disj(N, C, F, or(G, Fs)) :-
        M is C-1,
        ex_conj(N, C, F, G),
        ex_disj(N, M, F, Fs).

fdef(x(N), F) :-
        ex_disj(N, N, x(N), F).

fdef(t(F), or(box(a, form(F)), diam(b, form(n(F))))).

fdef(n(F), box(a, form(F))).

:-end_lpad.

