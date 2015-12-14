% You are on an island where every inhabitant is either a knight or a
% knave. Knights always tell the truth, and knaves always lie. The
% following examples show how various cases can be solved with CLP(B),
% Constraint Logic Programming over Boolean variables.

% These examples appear in Raymond Smullyan's "What Is the Name of
% this Book" and Maurice Kraitchik's "Mathematical Recreations".


:- use_module(library(clpb)).


% We use Boolean variables A, B and C to represent the inhabitants.
% Each variable is true iff the respective inhabitant is a knight.
% Notice that no search is required for any of these examples.


% Example 1: You meet two inhabitants, A and B.
%            A says: "Either I am a knave or B is a knight."

example_knights(1, [A,B]) :-
        sat(A=:=(~A + B)).


% Example 2: A says: "I am a knave, but B isn't."

example_knights(2, [A,B]) :-
        sat(A=:=(~A * B)).


% Example 3: A says: "At least one of us is a knave."

example_knights(3, [A,B]) :-
        sat(A=:=card([1,2],[~A,~B])).


% Example 4: You meet 3 inhabitants. A says: "All of us are knaves."
%            B says: "Exactly one of us is a knight."

example_knights(4, Ks) :-
        Ks = [A,B,C],
        sat(A=:=(~A * ~B * ~C)),
        sat(B=:=card([1],Ks)).


% Example 5: A says: "B is a knave."
%            B says: "A and C are of the same kind."
%            What is C?

example_knights(5, [A,B,C]) :-
        sat(A=:= ~B),
        sat(B=:=(A=:=C)).


/** <examples>

?- example_knights(1, [A,B]).
?- example_knights(2, [A,B]).
?- example_knights(Example, Knights).

*/
