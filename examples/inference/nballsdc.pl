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

% define another discrete random variable (categorical) with uniform distribution
nballs ~ uniform([1,2,3,4,5,6,7,8,9,10]).

ball(X) := nballs ~= N, between(1,N,X). % read as: for each X and N, if nballs=N and X is between 1 and N then ball(X) is true. nballs=N will succeed for N equals to the value of the number of balls. For example, if nballs=3 then ball(1),ball(2),ball(3) are true.

% define a random variable for each ball
material(X) ~ finite([0.3:wood,0.7:metal]) := ball(X). % read as: for each X if ball(X) is true then the random variable material(X) has a given distribution. For example, if ball(1) and ball(2) are true, then material(1) and material(2) are 2 i.i.d. random variables.

% define the color of each ball: the color distribution depends on the material.
color(X) ~ uniform([grey,blue,black]) := material(X) ~= metal. % read as: for each X if the material(X) is metal then color(X) has a given uniform distribution.
color(X) ~ uniform([black,brown]) := material(X) ~= wood. % read as: for each X if the material(X) is wood then color(X) has a given uniform distribution (different from the previous one).

% define draws with replacement. The ball drawn has a uniform distribution over the number of balls. However, the number of balls is a random variable itself.
drawn(_) ~ uniform(Balls) := nballs ~= N, findall(X,between(1,N,X),Balls).

% define the size of each ball with a beta distribution. The size distribution depends on the material
size(X) ~ beta(2,3) := material(X) ~= metal.
size(X) ~ beta(4,2) := material(X) ~= wood.
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
 
