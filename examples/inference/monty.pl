/*
Program describing the Monty Hall puzzle which gets its name from the TV game 
show hosted by Monty Hall. A player is given the opportunity to
select one of three closed doors, behind one of which there is a prize. 
Behind the other two doors are empty rooms.
Once the player has made a selection, Monty is obligated to open one of the 
remaining closed doors which does not contain the prize, showing that the room 
behind it is empty. He then asks the player if he would like to switch
his selection to the other unopened door, or stay with his original choice. 
Here is the problem: Does it matter if he switches?
From
Chitta Baral, Michael Gelfond, and Nelson Rushton. "Probabilistic reasoning with answer sets." Theory and Practice of Logic Programming 9.01 (2009): 57-144.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

% prize(A): the prize is behind door A, with A in {1,2,3}
% selected(A): the player selects door A
% open_door(A): Monty opens door A
% win_keep: the player wins in case he keeps his selection
% win_switch: the player wins in case he switches door


prize(1):1/3; prize(2):1/3; prize(3):1/3.
% the prize is behind each door with probability 1/3

selected(1).
% the player selected door 1

open_door(A):0.5; open_door(B):0.5:- 
  member(A,[1,2,3]),
  member(B,[1,2,3]),
  A<B,
  \+ prize(A),
  \+ prize(B),
  \+ selected(A),
  \+ selected(B).
% Monty opens door A with probability 0.5 and door B with probability 0.5 if
% A and B are different doors, the prize is not behind any of them and the
% player did not select any of them

open_door(A):-
  member(A,[1,2,3]),
  \+ prize(A),
  \+ selected(A),
  member(B,[1,2,3]),
  prize(B),
  \+ selected(B).
% Monty opens door A with probability 1 if the prize is not behind it, it is
% not selected by the player and the prize is behind another door A that is
% not selected

win_keep:- 
  selected(A), 
  prize(A).
% the player keeps his choice and wins if he has selected a door with the prize

win_switch:- 
  member(A,[1,2,3]),
  \+ selected(A), 
  prize(A), 
  \+ open_door(A).
% the player switches and wins if the prize is behind the door that he has 
% not selected and that Monty did not open

:- end_lpad.

/** <examples>

?- prob(win_keep,Prob). % what is the probability that the player wins if he keeps his choice?
% expcted result 0.3333333333333333
?- prob(win_switch,Prob). % what is the probability that the player wins if he switches door?
% expcted result 0.6666666666666667
% the probability if the player switches grows from 1/3 to 1/2
?- prob_bar(win_keep,Prob). % what is the probability that the player wins if he keeps his choice?
% expcted result 0.3333333333333333
?- prob_bar(win_switch,Prob). % what is the probability that the player wins if he switches door?
% the probability if the player switches grows from 1/3 to 1/2
% expcted result 0.6666666666666667
*/
 
