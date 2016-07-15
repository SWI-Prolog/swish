/*
Distributional Clauses example.
From Example 4 of 
Davide Nitti, Tinne De Laet, and Luc De Raedt. Probabilistic logic programming for hybrid relational domains. Machine Learning 103(3), 407-449, 2016.
http://link.springer.com/article/10.1007/s10994-016-5558-8/fulltext.html
"We have an urn, where the number of balls n is a random variable and each ball 
X has a color, material, and size with a known distribution. 
The i-th ball drawn with replacement from the
urn is named drawn(i)."
See also
https://github.com/davidenitti/DC/blob/master/examples/tutorial.pl
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

n(N): uniform(N,[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).
color(X,C):uniform(C,[grey, blue, black]):-material(X, metal).
color(X,C):uniform(C,[black, brown]) :- material(X,wood).
material(X,M):finite(M,[wood:0.3,metal:0.7]) :- n(N), between(1, N, X).
drawn(_,B) : uniform(B,L):-n(N), findall(X, between(1, N, X), L).
size(X,S):beta(S,2, 3) :- material(X,metal).
size(X,S):beta(S,4, 2):-material(X,wood).



:- end_lpad.

/** <examples>
?- mc_sample(drawn(1,1),1000,T,F,P).
%T = 285,
%F = 715,
%P = 0.285.

?- mc_sample(drawn(1,1),1000,T,F,P).
%T = 290,
%F = 710,
%P = 0.29.

?- mc_sample(drawn(1,1),1000,T,F,P).
%T = 283,
%F = 717,
%P = 0.283.

?- mc_sample((drawn(1,1),material(1,wood)),1000,T,F,P).
%T = 86,
%F = 914,
%P = 0.086.

?- mc_sample((drawn(1,1),material(1,wood),color(1,black)),1000,T,F,P).
%T = 44,
%F = 956,
%P = 0.044.

*/
 
