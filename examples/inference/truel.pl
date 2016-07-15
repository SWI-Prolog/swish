/*
Truel, or duel among three opponents. 
There are three truelists, a, b and c, that take turns in shooting with a gun.
The firing order is a, b and c. Each truelist can shoot at another truelist
or at the sky (deliberate miss). The truelist have these probabilities of 
hitting the target (if they are not aiming at the sky): a 1/3, b 2/3 and c 1.
The aim for each truelist is kill all the other truelists.
The question is: what should a do to maximize his probability of living? 
Aim at b, c or the sky?
Note that the best strategy for the other truelists and situations is
easy to find intuitively and corresponds to aim at the best shooter.
See https://en.wikipedia.org/wiki/Truel
Martin Shubik, Game Theory and Related Approaches to Social Behavior, 1964, page 43

*/

/** <examples>
?- best_strategy(a,[a,b,c],S).
% What is the best action for a?
% S= sky
?- mc_sample(survives_action(a,[a,b,c],0,b),1000,P).
% What is the probability that a survives if it aims at b?
% P = 50/189=0.26455026455
?- mc_sample(survives_action(a,[a,b,c],0,c),1000,P).
% What is the probability that a survives if it aims at c?
% P = 59/189=0.31216931216
?- mc_sample(survives_action(a,[a,b,c],0,sky),1000,P).
% What is the probability that a survives if it aims at the sky?
% P =25/63= 0.39682539682
?- mc_sample(survives([a,c],a,0),1000,P).
% What is the probability that a survives against c?
% P =1/3
?- mc_sample(survives([a,b],a,0),1000,P).
% What is the probability that a survives against b?
% P=3/7=0.42857142857
?- mc_sample(survives_round([b],[a,b],a,0),1000,P).
% What is the probability that a survives against b when it's b's turn?
%P=1/7=0.14285714285
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.
:- dynamic kr/1,num/1.
:- mc.


:- begin_lpad.
/**
 * best_strategy(+A:atom,+L:list,-S:atom).
 *
 * The best strategy for truelist A with 
 * L still alive is to aim at S (with sky for the sky).
 *
 */
best_strategy(A,L,S):-
    delete(L,A,L1),
    append(L1,[sky],L2),
    maplist(ev_action(A,L,0),L2,LP),
    sort(LP,LP1),
    reverse(LP1,[_P-S|_]).


/**
 * ev_action(+A:atom,+L:list,+T:term,+S:atom,-C:couple).
 *
 * Tuelist A with L still alive performing action S in turn T survives
 * with probability P in C=P-S.
 *
 */
ev_action(A,L,T,S,P-S):-
  mc_sample(survives_action(A,L,T,S),1000,P).

/**
 * survives_action(+A:atom,+L0:list,+T:term,+S:atom)
 *
 * A survives truel performing action S with L0 still alive in turn T
 *
 */
survives_action(A,L0,T,S):-
  shoot(A,S,L0,T,L1),
  remaining(L1,A,Rest),
  survives_round(Rest,L1,A,T).

/**
 * shoot(+H:atom,+S:atom,+L0:list,+T:term,-L:list).
 *
 * When H shoots at S with Rest0 to shoot in round T and L0 still alive,
 * the truelist to shoot in the round become Rest and the truelist still
 * alive L
 */
shoot(H,S,L0,T,L):-
    (S=sky ->  
      L=L0
    ;
      (hit(T,H) ->
        delete(L0,S,L)
      ;   
        L=L0
      )
    ).


hit(_,a):1/3.

hit(_,b):2/3.

hit(_,c):1.

/** 
 * survives(+List:list,+Individual:atom,Round:term).
 * 
 * Individual survives the truel with List starting at Round
 *
 */
survives([A],A,_):-!.

survives(L,A,T):-
    survives_round(L,L,A,T).
/**
 * survives_round(+Rest:list,+List:list,+Individual:atom,+Round:term)
 *
 * Individual survives the truel Round with Rest still to shoot and 
 * List truelist still alive
 */
survives_round(_,[A],A,_,[A]):-!.

survives_round(_,[_],_,_,_):-!,fail.

survives_round([],L,A,T):-
  survives(L,A,s(T)).

survives_round([H|_Rest],L0,A,T):-
    base_best_strategy(H,L0,S),
    shoot(H,S,L0,T,L1),
    remaining(L1,H,Rest1),
    member(A,L1),
    survives_round(Rest1,L1,A,T).


/** 
 * base_best_strategy(+A:atom,+T:list,-S:atom).
 *  
 *  the best action for A when 
 *  T is the list of surviving truelist, is S 
 *
 *  These are the strategies that are easy to find (most intuitive)
 *
 */
base_best_strategy(b,[b,c],c).
base_best_strategy(c,[b,c],b).
base_best_strategy(a,[a,c],c).
base_best_strategy(c,[a,c],a).
base_best_strategy(a,[a,b],b).
base_best_strategy(b,[a,b],a).
base_best_strategy(b,[a,b,c],c).
base_best_strategy(c,[a,b,c],b).
 
remaining([A|Rest],A,Rest):-!.

remaining([_|Rest0],A,Rest):-
  remaining(Rest0,A,Rest).

:- end_lpad.




