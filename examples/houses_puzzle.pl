%%  houses(-Solution) is det.
%   @param  Solution is a list with the solution to the puzzle.
%   @author Folklore attributes this puzzle to Einstein
%   @see http://en.wikipedia.org/wiki/Zebra_Puzzle


/* Houses logical puzzle: who owns the zebra and who drinks water?

	 1) Five colored houses in a row, each with an owner, a pet, cigarettes, and a drink.
	 2) The English lives in the red house. 
	 3) The Spanish has a dog. 
	 4) They drink coffee in the green house. 
	 5) The Ukrainian drinks tea. 
	 6) The green house is next to the white house. 
	 7) The Winston smoker has a serpent. 
	 8) In the yellow house they smoke Kool. 
	 9) In the middle house they drink milk.
	10) The Norwegian lives in the first house from the left. 
	11) The Chesterfield smoker lives near the man with the fox. 
	12) In the house near the house with the horse they smoke Kool. 
	13) The Lucky Strike smoker drinks juice. 
	14) The Japanese smokes Kent. 
	15) The Norwegian lives near the blue house. 

Who owns the zebra and who drinks water?
*/


houses(Solution) :-
	template(Solution),                                                 %  1
	member(h(english, _, _, _, red), Solution),                         %  2
	member(h(spanish, dog, _, _, _), Solution),                         %  3
	member(h(_, _, _, coffee, green), Solution),                        %  4
	member(h(ukrainian, _, _, tea, _), Solution),                       %  5 
	next(h(_, _, _, _, green), h(_, _, _, _, white), Solution),         %  6
	member(h(_, snake, winston, _, _), Solution),                       %  7
	member(h(_, _, kool, _, yellow), Solution),                         %  8
	Solution = [_, _, h(_, _, _, milk, _), _, _],                       %  9
	Solution = [h(norwegian, _, _, _, _)| _],                           % 10
	next(h(_, fox, _, _, _), h(_, _, chesterfield, _, _), Solution),    % 11
	next(h(_, _, kool, _, _), h(_, horse, _, _, _), Solution),          % 12
	member(h(_, _, lucky, juice, _), Solution),                         % 13
	member(h(japonese, _, kent, _, _), Solution),                       % 14
	next(h(norwegian, _, _, _, _), h(_, _, _, _, blue), Solution),      % 15
	member(h(_, _, _, water, _), Solution),  	% one of them drinks water
	member(h(_, zebra, _, _, _), Solution).  	% one of them owns a zebra

print([]).
print([House| Houses]) :-
	write(House), nl,
	print(Houses).

% h(Nationality, Pet, Cigarette, Drink, Color)
template([h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _), h(_, _, _, _, _)]).

next(A, B, [A, B, _, _, _]).
next(B, C, [_, B, C, _, _]).
next(C, D, [_, _, C, D, _]).
next(D, E, [_, _, _, D, E]).


/** <examples>

?- houses(Solution).

*/
