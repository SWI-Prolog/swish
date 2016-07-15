/*
This program is inspired by the morphological characteristics of the Stromboli
Italian island.
The Stromboli island is located at the intersection of two geological faults, 
one in the southwest-northeast direction, the other in the east-west direction,
and contains one of the three volcanoes that are active in Italy. 
This program models the possibility that an eruption or an earthquake occurs at Stromboli.

From
Elena Bellodi and Fabrizio Riguzzi. Structure learning of probabilistic logic 
programs by searching the clause space. Theory and Practice of Logic 
Programming, FirstView Articles, 2014
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

eruption : 0.6 ; earthquake : 0.3 :- 
  sudden_energy_release,
  fault_rupture(_).
% If there is a sudden energy release under the island and there is a fault 
% rupture, then there can be an eruption of the volcano on the island with 
% probability 0.6 or an earthquake in the area with probability 0.3

sudden_energy_release : 0.7.
% The energy release occurs with probability 0.7 

fault_rupture(southwest_northeast).
fault_rupture(east_west).
% we are sure that ruptures occur in both faults

:- end_lpad.

/** <examples>

?- prob(eruption,Prob). % what is the probability of an eruption?
% expected result 0.588
?- prob(earthquake,Prob). % what is the probability of an earthquake?
% expected result 0.357
?- prob_bar(eruption,Prob). % what is the probability of an eruption?
% expected result 0.588
?- prob_bar(earthquake,Prob). % what is the probability of an earthquake?
% expected result 0.357


*/
