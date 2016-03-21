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
  std_dev(I,Sigma),
  mean(M),
  measurement(I,M,Sigma,X).


std_dev(_,S): uniform(S,0,25).

mean(M): gaussian(M,0, 50).

measurement(_,M,Sigma,X): gaussian(X,M,Sigma*Sigma).


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

hist_lw(Samples,NBins,Chart,E):-
  mc_sample_arg(value(0,Y),Samples,Y,L0),
  mc_lw_sample_arg(mean(X),(value(1,-27.020),value(2,3.570),
  value(3,8.191),value(4,9.898),value(5,9.603),value(6,9.945),
  value(7,10.056)),Samples,X,L),
  exp(L,Samples,E),
  hist(L0,L,NBins,Chart).

hist_lw_noise(Samples,Chart,E):-
%  mc_sample_arg((std_dev(1,Y1),std_dev(2,Y2),std_dev(3,Y3),std_dev(4,Y4),
%  std_dev(5,Y5),std_dev(6,Y6)),Samples,(Y1,Y2,Y3,Y4,Y5,Y6),L0),
  mc_lw_sample_arg((std_dev(1,Y1),std_dev(2,Y2),std_dev(3,Y3),std_dev(4,Y4),
    std_dev(5,Y5),std_dev(6,Y6),std_dev(7,Y7)),(value(1,-27.020),value(2,3.570),
  value(3,8.191),value(4,9.898),value(5,9.603),value(6,9.945),
  value(7,10.056)),Samples,(Y1,Y2,Y3,Y4,Y5,Y6,Y7),L),
  exp_noise(L,Samples,E),
  E = (E1,E2,E3,E4,E5,E6,E7),
  Chart = c3{data:_{x:x, rows:[x-e,1-E1,2-E2,3-E3,4-E4,5-E5,6-E6,7-E7],
                    type: bar}}.


exp_noise(L,S,(E1,E2,E3,E4,E5,E6,E7)):-
  foldl(agg_noise,L,(0,0,0,0,0,0,0),(S1,S2,S3,S4,S5,S6,S7)),
  E1 is S1/S,
  E2 is S2/S,
  E3 is S3/S,
  E4 is S4/S,
  E5 is S5/S,
  E6 is S6/S,
  E7 is S7/S.

agg_noise((V1,V2,V3,V4,V5,V6,V7)-W,(S1,S2,S3,S4,S5,S6,S7),
  (S1+V1*W,S2+V2*W,S3+V3*W,S4+V4*W,S5+V5*W,S6+V6*W,S7+V7*W)).

exp(L,S,E):-
  foldl(agg,L,0,Sum),
  E is Sum/S.

agg(V-W,S,S+V*W).

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
       tick:_{values:Tick}}}
  }.

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
?- hist_lw(1000,40,G,E).
?- hist_lw_noise(1000,Chart,E).

*/
 
