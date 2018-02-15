%%  houses(-Solution)
%   @param  Solution is a list of houses that satisfy all constraints.
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

% Render the houses term as a nice table.
:- use_rendering(table,
		 [header(h('Owner', 'Pet', 'Cigarette', 'Drink', 'Color'))]).

zebra_owner(Owner) :-
	houses(Hs),
	member(h(Owner,zebra,_,_,_), Hs).

water_drinker(Drinker) :-
	houses(Hs),
	member(h(Drinker,_,_,water,_), Hs).


houses(Hs) :-
	% each house in the list Hs of houses is represented as:
	%      h(Nationality, Pet, Cigarette, Drink, Color)
	length(Hs, 5),                                            %  1
	member(h(english,_,_,_,red), Hs),                         %  2
	member(h(spanish,dog,_,_,_), Hs),                         %  3
	member(h(_,_,_,coffee,green), Hs),                        %  4
	member(h(ukrainian,_,_,tea,_), Hs),                       %  5
	next(h(_,_,_,_,green), h(_,_,_,_,white), Hs),             %  6
	member(h(_,snake,winston,_,_), Hs),                       %  7
	member(h(_,_,kool,_,yellow), Hs),                         %  8
	Hs = [_,_,h(_,_,_,milk,_),_,_],                           %  9
	Hs = [h(norwegian,_,_,_,_)|_],                            % 10
	next(h(_,fox,_,_,_), h(_,_,chesterfield,_,_), Hs),        % 11
	next(h(_,_,kool,_,_), h(_,horse,_,_,_), Hs),              % 12
	member(h(_,_,lucky,juice,_), Hs),                         % 13
	member(h(japanese,_,kent,_,_), Hs),                       % 14
	next(h(norwegian,_,_,_,_), h(_,_,_,_,blue), Hs),          % 15
	member(h(_,_,_,water,_), Hs),		% one of them drinks water
	member(h(_,zebra,_,_,_), Hs).		% one of them owns a zebra

next(A, B, Ls) :- append(_, [A,B|_], Ls).
next(A, B, Ls) :- append(_, [B,A|_], Ls).

/** <examples>

?- zebra_owner(Owner).

?- water_drinker(Drinker).

?- houses(Houses).

*/
