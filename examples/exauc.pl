:-use_module(library(auc)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.


main(AUCROC, ROC, AUCPR, PR):-
  compute_areas_diagrams(
    [0.7 - a, 0.7 - a, 0.7 - \+(a), 0.6 - a, 0.6 - \+(a), 0.5 - a, 0.4 - \+(a)],
    AUCROC, ROC, AUCPR, PR).


/** <examples>

?- main(AUCROC, ROC, AUCPR, PR). 

*/
