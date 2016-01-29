/*
Flexible probabilities: variable probabilistic annotations.
The example models drawing a red ball from an urn with R red and G green balls,
where each ball is drawn with uniform probability from the urn.
From
De Raedt, Luc, and Angelika Kimmig. "Probabilistic (logic) programming concepts." Machine Learning (2015): 1-43.
*/

:- use_module(library(pita)).


:- pita.

:- begin_lpad.

red(Prob):Prob .

draw_red(R, G):-
  Prob is R/(R + G),
  red(Prob).

:- end_lpad.
/**
?- prob(draw_red(3,1),P).
% expected result 0.75
*/
