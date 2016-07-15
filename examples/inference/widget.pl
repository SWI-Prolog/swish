/*
Factory producing widgets.
Consider a factory with two machines a and b. Each machine produces a widget
with a continuous feature. A widget is produced by machine a with probability
0.7 and by machine b with probability b.
If the widget is produced by machine a, the feature is distributed as a
Gaussian with mean 2.0 and variance 1.0.
If the widget is produced by machine b, the feature is distributed as a
Gaussian with mean 3.0 and variance 1.0.
The widget then is processed by a third machine that adds a random quantity to
the feature distributed as a Gaussian with mean 0.5 and variance 1.5.
What is the distribution of the feature?
What is the distribution of the feature given that the widget was procuded
by machine a?
What is the distribution of the feature given that the third machine added a
quantity greater than 0.2?
What is the distribution of the feature given that the third machine added
a quantity of 2.0?
Adapted from
Islam, Muhammad Asiful, C. R. Ramakrishnan, and I. V. Ramakrishnan. 
"Inference in probabilistic logic programs with continuous random variables." 
Theory and Practice of Logic Programming 12.4-5 (2012): 505-523.
http://arxiv.org/pdf/1112.2681v3.pdf
*/
:- use_module(library(mcintyre)).
:- use_module(library(clpr)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

widget(X) :- 
  machine(M),
  st(M,Z),
  pt(Y),
  {X =:= Y + Z}.

machine(a):0.3;machine(b):0.7.

st(a,X): gaussian(X,2.0, 1.0).

st(b,X): gaussian(X,3.0, 1.0).

pt(X): gaussian(X,0.5, 1.5).


:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(widget(X),Samples,X,L0),
  histogram(L0,NBins,Chart).
% What is the distribution of the feature?

hist_rej_macha(Samples,NBins,Chart):-
  mc_rejection_sample_arg(widget(X),machine(a),Samples,X,L0),
  histogram(L0,NBins,Chart).
% What is the distribution of the feature given that the widget was procuded
% by machine a, computed by taking Samples samples with rejection sampling and
% drawing a histogram with NBins bins?

hist_mh_macha(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(widget(X),machine(a),Samples,Lag,X,L0),
  histogram(L0,NBins,Chart).
% What is the distribution of the feature given that the widget was procuded
% by machine a, computed by taking Samples samples with Metropolis-Hastings
% (lag=Lag) and drawing a histogram with NBins bins?

hist_rej_dis(Samples,NBins,Chart):-
  mc_rejection_sample_arg(widget(X),(pt(Y),Y>0.2),Samples,X,L0),
  histogram(L0,NBins,Chart).
% What is the distribution of the feature given that the third machine added a
% quantity greater than 0.2, computed by taking Samples samples with rejection 
% sampling and
% drawing a histogram with NBins bins?

hist_mh_dis(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(widget(X),(pt(Y),Y>0.2),Samples,Lag,X,L0),
  histogram(L0,NBins,Chart).
% What is the distribution of the feature given that the third machine added a
% quantity greater than 0.2, computed by taking Samples samples with 
% Metropolis-Hastings and
% drawing a histogram with NBins bins?

hist_lw(Samples,NBins,Chart):-
  mc_sample_arg(widget(Y),Samples,Y,L0),
  mc_lw_sample_arg(widget(X),pt(2.0),Samples,X,L),
  densities(L0,L,NBins,Chart).
% What is the distribution of the feature given that the third machine added
% a quantity of 2.0, computed by taking Samples samples with likelihood weighting
% and drawing a density with NBins bins?


/** <examples>
?- hist_lw(1000,40,G).
% What is the distribution of the feature given that the third machine added
% a quantity of 2.0, computed by taking 1000 samples with likelihood weighting
% and drawing a density with 40 bins?

?- hist_uncond(10000,40,G).
% What is the distribution of the feature?

?- hist_rej_macha(10000,40,G).
% What is the distribution of the feature given that the widget was procuded
% by machine a, computed by taking 10000 samples with rejection sampling and
% drawing a histogram with 40 bins?

?- hist_mh_macha(10000,2,40,G).
% What is the distribution of the feature given that the widget was procuded
% by machine a, computed by taking 10000 samples with Metropolis-Hastings
% (lag=Lag) and drawing a histogram with 40 bins?

?- hist_rej_dis(10000,40,G).
% What is the distribution of the feature given that the third machine added a
% quantity greater than 0.2, computed by taking 10000 samples with rejection 
% sampling and
% drawing a histogram with 40 bins?

?- hist_mh_dis(10000,2,40,G).
% What is the distribution of the feature given that the third machine added a
% quantity greater than 0.2, computed by taking 10000 samples with 
% Metropolis-Hastings and
% drawing a histogram with 40 bins?

*/
 
