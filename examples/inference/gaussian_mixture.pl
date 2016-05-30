/*
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

heads:0.6;tails:0.4. 
g(X): gaussian(X,0, 1).
h(X):gaussian(X,5, 2).

mix(X) :- heads, g(X).
mix(X) :- tails, h(X).


:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(mix(X),Samples,X,L0),
  hist(L0,NBins,Chart).

hist_rej_heads(Samples,NBins,Chart):-
  mc_rejection_sample_arg(mix(X),heads,Samples,X,L0),
  hist(L0,NBins,Chart).

hist_mh_heads(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(mix(X),heads,Samples,Lag,X,L0),
  hist(L0,NBins,Chart).

hist_rej_dis(Samples,NBins,Chart):-
  mc_rejection_sample_arg(mix(X),(mix(Y),Y>2),Samples,X,L0),
  hist(L0,NBins,Chart).

hist_mh_dis(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(mix(X),(mix(Y),Y>2),Samples,Lag,X,L0),
  hist(L0,NBins,Chart).


hist(L0,NBins,Chart):-
  maplist(val,L0,L),
  max_list(L,Max),
  min_list(L,Min),
  sort(L,L1),
  D is Max-Min,
  BinWidth is D/NBins,
  bin(NBins,L1,Min,BinWidth,LB),
  NTick=10,
  TickWidth is D/NTick,
  int_round(TickWidth,1,TW),
  int_round(Min,1,MinR),
  MinR1 is MinR-TW,
  ticks(MinR1,TW,Max,_Tick),
  Chart = c3{data:_{x:elem, rows:[elem-freq|LB], type:bar},
          axis:_{ x:_{ tick:_{fit:false}}},
          bar:_{
            width:_{ ratio: 1.0 }}, 
            legend:_{show: false}}.
ticks(Min,T,Max,[]):-
  Min+T> Max,!.

ticks(Min,T,Max,[TickN|RT]):-
  Tick is Min+T,
  format(atom(TickA), '~2f', [Tick]),
  atom_number(TickA,TickN),
  ticks(Tick,T,Max,RT).

int_round(TW0,F,TW):-
  IP is float_integer_part(TW0*F),
  (IP=\=0->
    TW is IP/F
  ;
    F1 is F*10,
    int_round(TW0,F1,TW)
  ).

bin(0,_L,_Min,_BW,[]):-!.

bin(N,L,Lower,BW,[V-Freq|T]):-
  V is Lower+BW/2,
  Upper is Lower+BW,
  count_bin(L,Upper,0,Freq,L1),
  N1 is N-1,
  bin(N1,L1,Upper,BW,T).

count_bin([],_U,F,F,[]).

count_bin([H|T0],U,F0,F,T):-
  (H>=U->
    F=F0,
    T=T0
  ;
    F1 is F0+1,
    count_bin(T0,U,F1,F,T)
  ).

val([E]-_,E).
/** <examples>
?- hist_uncond(10000,40,G).

?- hist_rej_heads(10000,40,G).
?- hist_mh_heads(10000,2,40,G).
?- hist_rej_dis(10000,40,G).
?- hist_mh_dis(10000,2,40,G).

*/
 
