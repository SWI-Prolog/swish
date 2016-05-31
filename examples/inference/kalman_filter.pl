/*
Kalman filter example from
Islam, Muhammad Asiful, C. R. Ramakrishnan, and I. V. Ramakrishnan. 
"Inference in probabilistic logic programs with continuous random variables." 
Theory and Practice of Logic Programming 12.4-5 (2012): 505-523.
Russell, S. and Norvig, P. 2010. Arficial Intelligence: A Modern Approach. 
Third Edition, Prentice Hall, Figure 15.10 page 587
*/
:- use_module(library(mcintyre)).
:- use_module(library(clpr)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.
kf(N,O, T) :-
  init(S),
  kf_part(0, N, S,O,T).

kfo(N,O) :-
  init(S),
  kf_part(0, N, S,O,_T).


kf(N, T) :-
  init(S),
  kf_part(0, N, S,_O,T).

kf_part(I, N, S,[V|RO], T) :-
  I < N, 
  NextI is I+1,
  trans(S,I,NextS),
  emit(NextS,I,V),
  kf_part(NextI, N, NextS,RO, T).

kf_part(N, N, S, [],S).

trans(S,I,NextS) :-
  {E =:= NextS - S},
  trans_err(I,E).

emit(NextS,I,V) :-
  {X =:= V - NextS},
  obs_err(I,X).

init(S):gaussian(S,0,1).
% prior as in Russel and Norvig 2010, Fig 15.10
trans_err(_,E):gaussian(E,0,2).
% transition noise as in Russel and Norvig 2010, Fig 15.10

obs_err(_,E):gaussian(E,0,1).
:- end_lpad.



hist_lw(Samples,NBins,Chart):-
  mc_sample_arg(kf(1,Y),Samples,Y,L0),
  mc_lw_sample_arg(kf(1,T),kfo(1,[2.5]),Samples,T,L),
  densities(L0,L,NBins,Chart).
% observation as in Russel and Norvig 2010, Fig 15.10

/** <examples>
mc_lw_sample_arg(kf(1,T),kfo(1,[2.5]),10,T,L).
?- hist_lw(1000,40,G).

?- hist_rej_heads(10000,40,G).
?- hist_mh_heads(10000,2,40,G).
?- hist_rej_dis(10000,40,G).
?- hist_mh_dis(10000,2,40,G).

*/
 
