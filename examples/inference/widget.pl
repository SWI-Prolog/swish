/*
Throwing a coin with uncertainty on its fairness, from
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

widget(X) :- 
  machine(M),
  st(M,Z),
  pt(Y),
  X is Y + Z.

machine(a):0.3;machine(b):0.7.

st(a,X): gaussian(X,2.0, 1.0).

st(b,X): gaussian(X,3.0, 1.0).

pt(X): gaussian(X,0.5, 0.1).


:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(widget(X),Samples,X,L0),
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
/** <examples>
?- hist_uncond(10000,40,G).

?- hist_rej_heads(10000,40,G).
?- hist_mh_heads(10000,2,40,G).
?- hist_rej_dis(10000,40,G).
?- hist_mh_dis(10000,2,40,G).

*/
 
