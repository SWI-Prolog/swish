/*
Throwing a coin with uncertainty on its fairness, from
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=gaussian-posteriors
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

value(I,X) :- 
  Var is sqrt(5),
  mean(M,Var),
  value(I,M,X).


mean(M,Var): gaussian(M,1.0, Var).

value(_,M,X): gaussian(X,M, 2.0).


:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(val(0,X),Samples,X,L0),
  hist(L0,NBins,Chart).

hist_rej_heads(Samples,NBins,Chart):-
  mc_rejection_sample_arg(widget(X),machine(a),Samples,X,L0),
  hist(L0,NBins,Chart).

hist_mh_heads(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(widget(X),machine(a),Samples,Lag,X,L0),
  hist(L0,NBins,Chart).

hist_rej_dis(Samples,NBins,Chart):-
  mc_rejection_sample_arg(widget(X),(pt(Y),Y>0.2),Samples,X,L0),
  hist(L0,NBins,Chart).

hist_mh_dis(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(widget(X),(pt(Y),Y>0.2),Samples,Lag,X,L0),
  hist(L0,NBins,Chart).

hist_lw(Samples,NBins,Chart):-
  mc_sample_arg(value(0,Y),Samples,Y,L0),
  mc_lw_sample_arg(value(0,X),(value(1,9),value(2,8)),Samples,X,L),
  hist(L0,L,NBins,Chart).

hist(L0,P,NBins,Chart):-
  maplist(val,L0,L),
  keysort(P,PS),
  maplist(key,PS,P1),
  append(L,P1,All),
  max_list(All,Max),
  min_list(All,Min),
  sort(L,L1),
  D is Max-Min,
  BinWidth is D/NBins,
  bin(NBins,L1,Min,BinWidth,LB),
  /*max_list(P1,MaxP),
  min_list(P1,MinP),
  DP is MaxP-MinP,
  BWP is DP/NBins,*/
  binP(NBins,PS,Min,BinWidth,PB),
  maplist(to_dict_pre,LB,DB),
  maplist(to_dict_post,PB,DP),
  dicts_join(x, DB, DP, Data),
  NTick=10,
  TickWidth is D/NTick,
  int_round(TickWidth,1,TW),
  int_round(Min,1,MinR),
  MinR1 is MinR-TW,
  ticks(MinR1,TW,Max,Tick),
%  Chart = c3{data:_{xs:_{pre: xpre,post: xpost}, 
  %Chart = c3{data:_{xs:_{pre: xpre}, 
  Chart = c3{data:_{x: x, 
  rows: Data},
   axis:_{x:_{min:Min,max:Max,
       tick:_{values:Tick}}
  }}.

ticks(Min,T,Max,[]):-
  Min+T> Max,!.

ticks(Min,T,Max,[Tick|RT]):-
  Tick is Min+T,
  ticks(Tick,T,Max,RT).

int_round(TW0,F,TW):-
  IP is float_integer_part(TW0*F),
  (IP=\=0->
    TW is IP/F
  ;
    F1 is F*10,
    int_round(TW0,F1,TW)
  ).
binP(0,_L,_Min,_BW,[]):-!.

binP(N,L,Lower,BW,[V-Freq|T]):-
  V is Lower+BW/2,
  Upper is Lower+BW,
  count_binP(L,Upper,0,Freq,L1),
  N1 is N-1,
  binP(N1,L1,Upper,BW,T).

count_binP([],_U,F,F,[]).

count_binP([H-W|T0],U,F0,F,T):-
  (H>=U->
    F=F0,
    T=T0
  ;
    F1 is F0+W,
    count_binP(T0,U,F1,F,T)
  ).

to_dict_pre(X-Y,r{x:X,pre:Y}).
to_dict_post(X-Y,r{x:X,post:Y}).
hist(L0,NBins,Chart):-
  maplist(val,L0,L),
  max_list(L,Max),
  min_list(L,Min),
  sort(L,L1),
  D is Max-Min,
  BinWidth is D/NBins,
  bin(NBins,L1,Min,BinWidth,LB),
  Chart = c3{data:_{x:elem, rows:[elem-freq|LB], type:bar},
          axis:_{ x:_{ tick:_{
    format: 'function (x) { return x.toFixed(2);}' ,
           fit: true,culling:_{max:7} }} },
          bar:_{
            width:_{ ratio: 1.0 }}, 
            legend:_{show: false}}.

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

key(K-_,K).

split(X-Y,X,Y).
/** <examples>
?- hist_lw(1000,40,G).
?- hist_uncond(10000,40,G).

?- hist_rej_heads(10000,40,G).
?- hist_mh_heads(10000,2,40,G).
?- hist_rej_dis(10000,40,G).
?- hist_mh_dis(10000,2,40,G).

*/
 
