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


:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- set_pita(depth_bound,true).
:- set_pita(depth,5).

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

?- prob(obj(2),P). what is the probability that object 2 exists?
% expecte result 0.08189999999999999
?- prob_bar(obj(2),P). what is the probability that object 2 exists?
% expecte result 0.08189999999999999
?- prob(numObj(0,2),P). % what is the probability that there are 2 objects?
% expecte result 0.063

*/

