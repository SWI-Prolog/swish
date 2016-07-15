/*
Program describing the three-prisoners puzzle. 
Of three prisoners a, b, and c, two are to be executed, but a does not know
which. Thus, a thinks that the probability that i will be executed is 2/3 for
i in {a, b, c}. He says to the jailer, "Since either b or c is certainly going 
to be executed, you will give me no information about my own chances if you give
the name of one man, either b or c, who is going to be executed." But then, no
matter what the jailer says, naive conditioning leads a to believe that his 
chance of execution went down from 2/3 to 1/2.
From
Peter D. Grunwald and Joseph Y. Halpern. "Updating Probabilities." Journal of Artificial Intelligence Research 19 (2003): 243-278.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

% safe(A): prisoner A is not going to be executed, with A in {a,b,c}
% tell_executed(A): the jailer tells a that A is going to be executed
% safe_after_tell: a is safe after the jailer has spoken

safe(a):1/3; safe(b):1/3; safe(c):1/3.
% a, b and c are safe with probability 1/3

tell_executed(b):1/2; tell_executed(c):1/2:-
  safe(a).
% the jailer says that b will be executed with probability 1/2 or c will be
% executed with probability 1/2 if a is safe


tell_executed(A):-
  select(B, [b,c], [A]),
  safe(B).
% the jailer says that A will be executed with certainty if there is a prisoner
% B in {b,c} different from A that is safe

tell:-
  tell_executed(_).
% the jailers speaks if there is a prisoner for which he says that he will be
% executed

safe_after_tell:-
  safe(a),
  tell.
% a is safe after the jailer has spoken if he is safe and the jailer has told
% that someone is going to be executed

:- end_lpad.
  
/** <examples>

?- prob(safe_after_tell,Prob). % what is the probability that a is not executed after the jailer has spoken
% expected result 1/3, means that the jailer communication did not change the 
% probability of a being safe
?- prob(tell,Prob). % what is the probability that the jailer speaks? 
% expcted result 1.0
?- prob(safe(a),Prob). % what is the probability that a is not executed
% expcted result 0.3333333333333333
?- prob(tell_executed(b),Prob). % what is the probability that the jailer says b is going to be executed?
% expcted result 0.5
?- prob(tell_executed(c),Prob). % what is the probability that the jailer says b is going to be executed?
% expcted result 0.5
?- prob_bar(safe_after_tell,Prob). % what is the probability that a is not executed after the jailer has spoken
% expected result 1/3, means that the jailer communication did not change the 
% probability of a being safe
?- prob_bar(tell,Prob). % what is the probability that the jailer speaks? 
% expcted result 1.0
?- prob_bar(safe(a),Prob). % what is the probability that a is not executed
% expcted result 0.3333333333333333
?- prob_bar(tell_executed(b),Prob). % what is the probability that the jailer says b is going to be executed?
% expcted result 0.5
?- prob_bar(tell_executed(c),Prob). % what is the probability that the jailer says b is going to be executed?
% expcted result 0.5

*/


