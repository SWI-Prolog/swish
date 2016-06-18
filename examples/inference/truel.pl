/*
Truel, or duel among three opponents. 
There are three truelist, a, b and c, that take turns in shooting with a gun.
The firing order is a, b and c. Each truelist can shoot at another truelist
or at the sky (deliberate miss). The truelist have these probabilities of 
hitting the target (if they are not aiming at the sky): a 1/3, b 2/3 and c 1.
The question is: what should a do to maximize his probability of living? 
Aim at b, c or the sky?
See https://en.wikipedia.org/wiki/Truel
Martin Shubik, Game Theory and Related Approaches to Social Behavior, 1964, page 43

*/

/** <examples>
?- best_strategy(a,[b,c],[a,b,c],S).
% What is the best action for a?
% S= ($).
?- mc_sample(survives_action(a,[b,c],0,[a,b,c],b),1000,P).
% What is the probability that a survives if it aims at b?
% P = 0.256
?- mc_sample(survives_action(a,[b,c],0,[a,b,c],c),1000,P).
% What is the probability that a survives if it aims at c?
% P = 0.292
?- mc_sample(survives_action(a,[b,c],0,[a,b,c],'$'),1000,P).
% What is the probability that a survives if it aims at the sky?
% P = 0.383
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

survives_round([H|Rest],L0,A,T):-
    best_strategy_base(H,Rest,L0,S),
    shoot(H,S,Rest,L0,T,Rest1,L1),
    member(A,L1),
    survives_round(Rest1,L1,A,T).


/**
 * best_strategy(+A:atom,+Rest:list,+L:list,-S:atom).
 *
 * The best strategy for truelist A with Rest remaining to shoot and
 * L still alive is to aim at S (with '$' for the sky).
 *
 */
best_strategy(A,Rest,L,S):-
    delete(L,A,L1),
    append(L1,['$'],L2),
    maplist(ev_action(A,Rest,L),L2,LP),
    sort(LP,LP1),
    reverse(LP1,[_P-S|_]).


/**
 * ev_action(+A:atom,+Rest:list,+L:list,+S:atom,-C:couple).
 *
 * Tuelist A with Rest to shoot, L still alive performing action S survives
 * with probability P in C=P-S.
 *
 */
ev_action(A,Rest,L,S,P-S):-
    mc_sample(survives_action(A,Rest,0,L,S),1000,P).

/**
 * survives_action(+A:atom,+Rest0:list,+T:term,+L0:list,+S:atom)
 *
 * A survives truel performing action S at round T with Rest0 to shoot in
 * the round and L0 still alive
 *
 */
survives_action(A,Rest0,T,L0,S):-
    shoot(A,S,Rest0,L0,T,Rest,L1),
    survives_round(Rest,L1,A,T).
/**
 * shoot(+H:atom,+S:atom,+Rest0:list,+L0:list,+T:term,-Rest:list,-L:list).
 *
 * When H shoots at S with Rest0 to shoot in round T and L0 still alive,
 * the truelist to shoot in the round become Rest and the truelist still
 * alive L
 */
shoot(H,S,Rest0,L0,T,Rest,L):-
    (S='$' ->  
      L=L0,
      Rest=Rest0
    ;
      (hit(T,H) ->
        delete(L0,S,L),
        delete(Rest0,S,Rest)
      ;   
        L=L0,
        Rest=Rest0
      )
    ).


hit(_,a):1/3.

hit(_,b):2/3.

hit(_,c):1.

/** 
 * best_strategy_base(+A:atom,+Rest:list,+T:list,-S:atom).
 *  
 *  the best action for A when Rest follow him in the round and
 *  T is the list of surviving truelist, is S (with '$' for the sky)
 *
 *  These are the strategies that are easy to find (most intuitive)
 *
 */
best_strategy_base(b,[c],[b,c],c).
best_strategy_base(c,[],[b,c],b).
best_strategy_base(a,[c],[a,c],c).
best_strategy_base(c,[],[a,c],a).
best_strategy_base(a,[b],[a,b],b).
best_strategy_base(b,[],[a,b],a).
best_strategy_base(b,[c],[a,b,c],c).
best_strategy_base(c,[],[a,b,c],b).
 

:- end_lpad.




