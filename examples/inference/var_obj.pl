/*
Existence uncertainty/unknown objects.
This programs models a domain where the number of objects is uncertain.
In particular, the number of objects follows a geometric distribution 
with parameter 0.3.
We can ask what is the probability that the object number n exists.
From
Poole, David. "The independent choice logic and beyond." Probabilistic 
inductive logic programming. Springer Berlin Heidelberg, 2008. 222-243.
*/


:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- mc.

:- begin_lpad.
numObj(N, N) :-
  \+ more(N).

numObj(N, N2) :-
  more(N),
  N1 is N + 1,
  numObj(N1, N2).

more(_):0.3.

obj(I):-
 numObj(0,N),
 between(1, N, I).

:- end_lpad.



/** <examples>

?- mc_prob(obj(2),P). % what is the probability that object 2 exists?
% expected result ~ 0.08992307692307693
?- mc_prob_bar(obj(2),P). % what is the probability that object 2 exists?
% expected result ~ 0.08992307692307693
?- mc_prob(obj(5),P). % what is the probability that object 5 exists?
% expected result ~ 0.002666
?- mc_prob_bar(obj(5),P). % what is the probability that object 5 exists?
% expected result ~ 0.002666
?- mc_prob(numObj(0,2),P). % what is the probability that there are 2 objects?
% expected result ~ 0.0656
?- mc_prob(numObj(0,5),P). % what is the probability that there are 5 objects?
% expected result ~ 0.0014
?- mc_sample(obj(5),1000,T,F,P). % take 1000 samples of obj(5)
?- mc_sample_bar(obj(5),1000,Chart). % take 1000 samples of obj(5)
?- mc_sample_arg_bar(numObj(0,N),100,N,Chart). % take 100 samples of L in
% findall(N,numObj(0,N),L)
?- mc_sample_arg_bar(obj(I),100,I,Chart). % take 100 samples of L in
% findall(I,obj(I),L)
??- mc_sample_arg(obj(I),100,I,Values). % take 100 samples of L in 
% findall(I,obj(I),L)

*/

