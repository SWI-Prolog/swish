/*
Program describing the Mendelian rules of inheritance of the color of pea 
plants. It considers a family of four grandparents, two parents and a child.
The problem is, given the alleles of the grandparents, predict the
probability of the color (or of its alleles) of a pea plant.
From
H. Blockeel. Probabilistic logical models for mendel's experiments: An exercise.
In Inductive Logic Programming (ILP 2004), Work in Progress Track, 2004.
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

mother(m,s).
mother(mm,m).
mother(mf,f).
father(f,s).
father(fm,m).
father(ff,f).
% family with 7 members: grandparents, partents and s

% cg(I,C,A) means that individual I has color allele A on chromosome C
% the color alleles are p and w and the chromosomes are 1 and 2
% color(I,Col) means that individual I has color Col
% Col can be purple or white



cg(mm,1,p).
cg(mm,2,w).
cg(fm,1,p).
cg(fm,2,p).
cg(mf,1,w).
cg(mf,2,w).
cg(ff,1,w).
cg(ff,2,p).
% we know with certainty the alleles of the grandparants of s


cg(X,1,A):0.5 ; cg(X,1,B):0.5 :- mother(Y,X),cg(Y,1,A), cg(Y,2,B).
% the color allele of an individual on chromosome 1 is inherited from its 
% mother. The two alleles of the mother have equal probability of being
% transmitted
 
cg(X,2,A):0.5 ; cg(X,2,B):0.5 :- father(Y,X),cg(Y,1,A), cg(Y,2,B).
% the color allele of an individual on chromosome 2 is inherited from its 
% father. The two alleles of the mother have equal probability of being
% transmitted


color(X,purple) :- cg(X,_,p).
% if an individual has a p allele its color is purple, i.e., purple is
% dominant

color(X,white) :- cg(X,1,w), cg(X,2,w).
% if an individual has two w alleles its color is white, i.e., white is
% recessive

:- end_lpad.

/** <examples>

?- prob(color(s,purple),Prob). % what is the probability that the color of s' flowers is purple?
% expected result 0.8125
?- prob(color(s,white),Prob). % what is the probability that the color of s' flowers is white?
% expected result 0.1875
?- prob(cg(s,1,p),Prob). % what is the probability that the color allele on chromosme 1 of s is p?
% expected result 0.75
?- prob(cg(s,1,w),Prob). % what is the probability that the color allele on chromosme 1 of s is w?
% expected result 0.25
?- prob(cg(s,2,p),Prob). % what is the probability that the color allele on chromosme 2 of s is p?
% expected result 0.25
?- prob(cg(s,2,w),Prob). % what is the probability that the color allele on chromosme 2 of s is w?
% expected result 0.75
?- prob_bar(color(s,purple),Prob). % what is the probability that the color of s' flowers is purple?
% expected result 0.8125
?- prob_bar(color(s,white),Prob). % what is the probability that the color of s' flowers is white?
% expected result 0.1875
?- prob_bar(cg(s,1,p),Prob). % what is the probability that the color allele on chromosme 1 of s is p?
% expected result 0.75
?- prob_bar(cg(s,1,w),Prob). % what is the probability that the color allele on chromosme 1 of s is w?
% expected result 0.25
?- prob_bar(cg(s,2,p),Prob). % what is the probability that the color allele on chromosme 2 of s is p?
% expected result 0.25
?- prob_bar(cg(s,2,w),Prob). % what is the probability that the color allele on chromosme 2 of s is w?
% expected result 0.75
*/
 
