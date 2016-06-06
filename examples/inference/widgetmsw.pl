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
widget(X) :- msw(m, M),
msw(st(M), Z),
msw(pt, Y),
{X = Y + Z}.
% Ranges of RVs
values(m, [a,b]).
values(st(_), real).
values(pt, real).
% PDFs and PMFs:
:- set_sw(m, [0.3, 0.7]),
set_sw(st(a), norm(2.0, 1.0)),
set_sw(st(b), norm(3.0, 1.0)),
set_sw(pt, norm(0.5, 0.1)).


:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(widget(X),Samples,X,L0),
  histogram(L0,NBins,Chart).
% What is the distribution of the feature?


/** <examples>
?- hist_uncond(10000,40,G).
% What is the distribution of the feature?

*/
 
