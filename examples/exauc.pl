:-use_module(library(auc)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.


main(AUCROC, ROC, AUCPR, PR):-
  compute_areas_diagrams(
    % list of couples prob-example where example is an atom for positive 
    % examples and \+(atom) for negative examples
    [0.7 - a, 0.7 - a, 0.7 - \+(a), 0.6 - a, 
     0.6 - \+(a), 0.5 - a, 0.4 - \+(a)],
    AUCROC, ROC, AUCPR, PR).


/** <examples>

?- main(AUCROC, ROC, AUCPR, PR). 

*/
