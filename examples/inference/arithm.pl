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

eval(X,Y):-
  random_fn(X,0,F),
  Y is F.

op(+):0.5;op(-):0.5.



random_fn(X,L,F):-
  comb(L),
  random_fn(X,l(L),F1),
  random_fn(X,r(L),F2),
  op(Op),
  F=..[Op,F1,F2].

random_fn(X,L,F):-
  \+ comb(L),
  base_random_fn(X,L,F).

comb(_):0.3.

base_random_fn(X,L,X):-
  identity(L).

base_random_fn(_X,L,C):-
  \+ identity(L),
  random_const(L,C).

identity(_):0.5.


random_const(L,0):0.1;random_const(L,1):0.1;random_const(L,2):0.1;
random_const(L,3):0.1;random_const(L,4):0.1;random_const(L,5):0.1;
random_const(L,6):0.1;random_const(L,7):0.1;random_const(L,8):0.1;
random_const(L,9):0.1.




:- end_lpad.

/** <examples>

?-  mc_rejection_sample(eval(2,4),eval(1,3),1000,T,F,P).
% perform rejection sampling of eval(2,4) given that eval(1,3) is true
% expected result
% T = 88,
% F = 912,
% P = 0.088.
?- mc_rejection_sample(eval(2,4),(eval(0,2),eval(1,3)),10000,T,F,P).
% perform rejection sampling of eval(2,4) given that 
% eval(0,2) and eval(1,3) are true
% expected result 
% T = 1000,
% F = 0,
% P = 1.

?- mc_rejection_sample_arg(eval(2,Y),eval(1,3),10000,Y,V).
% perform rejection sampling of argument Y of eval(2,Y) given that
% eval(1,3) is true
% expected result 
% V = [[3]-419, [4]-47, [6]-44, [2]-28, [5]-10, [1]-3, [0]-1].


?- mc_rejection_sample_arg(eval(2,Y),(eval(0,2),eval(1,3)),10000,Y,V).
% perform rejection sampling of argument Y of eval(2,Y) given that
% eval(0,2) and eval(1,3) are true
% expected result V = [[4]-45].

?- mc_mh_sample(eval(2,4),eval(1,3),10000,1,T,F,P).
% perform Metropolis Hastings sampling of eval(2,Y) given that
% eval(1,3) is true
% T = 1151,
% F = 8849,
% P = 0.1151.

?- mc_mh_sample(eval(2,4),(eval(0,2),eval(1,3)),100,1,T,F,P).
% perform Metropolis Hastings sampling of eval(2,Y) given that
% eval(0,2) and eval(1,3) are true
% T = 100,
% F = 0,
% P = 1.


*/
 
