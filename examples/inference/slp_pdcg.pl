/*
Stochastic logic program defining a distribution over simple sentences with number agreement. The sentences are defined using a definite clause grammars.
Recall that in SLPs, the probabilities of all rules with the same head predicate sum to one and define a mutually exclusive choice on how to continue a proof.
Furthermore, repeated choices are independent, i.e., no stochastic memoization
Form https://dtai.cs.kuleuven.be/problog/tutorial/various/06_slp.html#stochastic-logic-programs
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.
% a stochastic logic program defining a distribution over simple sentences with number agreement
% recall that in SLPs, the probabilities of all rules with the same head predicate sum to one and define a mutually exclusive choice on how to continue a proof
% furthermore, repeated choices are independent, i.e., no stochastic memoization
%
% 1.0 : s(List) :-
%    s(List,[]).
%
% 1.0 : s(List,Rest) :-
%    np(List,Mid,Number),
%    vp(Mid,Rest,Number).
%
% 0.4 : np(List,Rest,sing) :-
%    det(List,Mid,sing),
%    n(Mid,Rest,sing).
% 0.4 : np(List,Rest,pl) :-
%    n(List,Rest,pl).
% 0.2 : np(List,Rest,pl) :-
%    det(List,Mid,pl),
%    n(Mid,Rest,pl).
%
% 1.0 : vp(List,Rest,Num) :-
%    v(List,Mid,Num),
%    np(Mid,Rest,_).
%
% 1/3 : det([the|L],L,pl).
% 1/3 : det([the|L],L,sing).
% 1/3 : det([a|L],L,sing).
%
% 0.25 : n([cat|L],L,sing).
% 0.15 : n([mouse|L],L,sing).
% 0.1 : n([dog|L],L,sing).
% 0.25 : n([cats|L],L,pl).
% 0.15 : n([mice|L],L,pl).
% 0.1 : n([dogs|L],L,pl).
%
% 0.35 : v([chases|L],L,sing).
% 0.15 : v([sees|L],L,sing).
% 0.2 : v([chase|L],L,pl).
% 0.3 : v([see|L],L,pl).

% use counter-based trial IDs for each head predicate with a stochastic choice, and explicit split rather than difference lists

% s(Num) -> np(Num),vp(Num)
s(W,Num,np(N,NN),det(D,DD),n(M,MM),v(V,VV)) :-
     split(W,W1,W2),
     np(W1,Num,np(N,NI),det(D,DI),n(M,MI),v(V,VI)),
     vp(W2,Num,np(NI,NN),det(DI,DD),n(MI,MM),v(VI,VV)).

% np(Num) -> 0.4:: det(sg),n(sg) | 0.4:: n(pl) | 0.2::det(pl),n(pl)
0.4::np_to(N,sg_dn); 0.4::np_to(N,pl_n); 0.2::np_to(N,pl_dn).
%np_to(N,sg_dn):0.4; np_to(N,pl_n):0.4; np_to(N,pl_dn):0.2.
np(W,sg,np(N,NN),det(D,DD),n(M,MM),v(V,VV)) :-
     np_to(N,sg_dn),
     NI is N+1,
     split(W,W1,W2),
     det(W1,sg,np(NI,N1),det(D,D1),n(M,M1),v(V,V1)),
     n(W2,sg,np(N1,NN),det(D1,DD),n(M1,MM),v(V1,VV)).
np(W,pl,np(N,NN),det(D,DD),n(M,MM),v(V,VV)) :-
     np_to(N,pl_n),
     NI is N+1,
     n(W,pl,np(NI,NN),det(D,DD),n(M,MM),v(V,VV)).
np(W,pl,np(N,NN),det(D,DD),n(M,MM),v(V,VV)) :-
     np_to(N,pl_dn),
     NI is N+1,
     split(W,W1,W2),
     det(W1,pl,np(NI,N1),det(D,D1),n(M,M1),v(V,V1)),
     n(W2,pl,np(N1,NN),det(D1,DD),n(M1,MM),v(V1,VV)).

% vp(Num) -> v(Num), np(_)
vp(W,Num,np(N,NN),det(D,DD),n(M,MM),v(V,VV)) :-
     split(W,W1,W2),
     v(W1,Num,np(N,NI),det(D,DI),n(M,MI),v(V,VI)),
     np(W2,_,np(NI,NN),det(DI,DD),n(MI,MM),v(VI,VV)).

% det(Num) -> 1/3 the(pl) | 1/3 the(sg) | 1/3 a(sg)
% det_to(N,the_pl):1/3; det_to(N,the_sg):1/3; det_to(N,a_sg):1/3.
1/3::det_to(N,the_pl); 1/3::det_to(N,the_sg); 1/3::det_to(N,a_sg).
det([the],pl,np(N,N),det(D,DD),n(M,M),v(V,V)) :-
     det_to(D,the_pl), DD is D+1.
det([the],sg,np(N,N),det(D,DD),n(M,M),v(V,V)) :-
     det_to(D,the_sg), DD is D+1.
det([a],sg,np(N,N),det(D,DD),n(M,M),v(V,V)) :-
     det_to(D,a_sg), DD is D+1.

% n(Num) -> 0.25 cat(sg) | 0.15 mouse(sg) | 0.1 dog(sg) | 0.25 cats(pl) | 0.15 mice(pl) | 0.1 dogs(pl)
0.25::n_to(N,cat_sg);  0.15::n_to(N, mouse_sg) ; 0.1::n_to(N, dog_sg) ; 0.25::n_to(N, cats_pl) ; 0.15::n_to(N, mice_pl) ; 0.1::n_to(N, dogs_pl).
%n_to(N,cat_sg):0.25;  n_to(N, mouse_sg):0.15 ; n_to(N, dog_sg):0.1 ; n_to(N, cats_pl):0.25 ; n_to(N, mice_pl):0.15 ; n_to(N, dogs_pl):0.1.
%n_to(N,cat_sg):0.5;  n_to(N, mouse_sg):0.3 ; n_to(N, dog_sg):0.2.
% n_to(N, cats_pl):0.5 ; n_to(N, mice_pl):0.3 ; n_to(N, dogs_pl):0.2.
n([cat],sg,np(N,N),det(D,D),n(M,MM),v(V,V)) :-
     n_to(M,cat_sg), MM is M+1.
n([mouse],sg,np(N,N),det(D,D),n(M,MM),v(V,V)) :-
     n_to(M,mouse_sg), MM is M+1.
n([dog],sg,np(N,N),det(D,D),n(M,MM),v(V,V)) :-
     n_to(M,dog_sg), MM is M+1.
n([cats],pl,np(N,N),det(D,D),n(M,MM),v(V,V)) :-
     n_to(M,cats_pl), MM is M+1.
n([mice],pl,np(N,N),det(D,D),n(M,MM),v(V,V)) :-
     n_to(M,mice_pl), MM is M+1.
n([dogs],pl,np(N,N),det(D,D),n(M,MM),v(V,V)) :-
     n_to(M,dogs_pl), MM is M+1.

% v(Num) -> 0.35 chases(sg) | 0.15 sees(sg) | 0.2 chase(pl) | 0.3 see(pl)
0.35::v_to(N, chases_sg) ; 0.15::v_to(N, sees_sg) ; 0.2::v_to(N, chase_pl) ; 0.3::v_to(N, see_pl).
%v_to(N, chases_sg):0.35 ; v_to(N, sees_sg):0.15 ; v_to(N, chase_pl):0.2 ; v_to(N, see_pl):0.3.
v([chases],sg,np(N,N),det(D,D),n(M,M),v(V,VV)) :-
     v_to(V,chases_sg), VV is V+1.
v([sees],sg,np(N,N),det(D,D),n(M,M),v(V,VV)) :-
     v_to(V,sees_sg), VV is V+1.
v([chase],pl,np(N,N),det(D,D),n(M,M),v(V,VV)) :-
     v_to(V,chase_pl), VV is V+1.
v([see],pl,np(N,N),det(D,D),n(M,M),v(V,VV)) :-
     v_to(V,see_pl), VV is V+1.

% initialize all counters to 0, and leave number open
word(L) :- s(L,_Num,np(0,_),det(0,_),n(0,_),v(0,_)).

% split(+T,-P,-S) splits the list T into two non-empty sublists P(refix) and S(uffix)
% note that T needs to have fixed length for this to terminate
split([A,B|C],[A],[B|C]).
split([A,B|C],[A|D],E) :-
  split([B|C],D,E).

% compute posteriors of correct sentences (or sample a correct sentence)
% we need to fix the length of the list to ensure split/3 has a finite number of solutions
query(X):-X=[_,_,_,_,_],word(X).
query(X):-X=[_,_,_,_],word(X).
query(X):-X=[_,_,_],word(X).
evidence(is_word).

% this grammar can only produce words of length 3/4/5
% the probability of getting a word is 0.067222
is_word :- word([_,_,_]).
is_word :- word([_,_,_,_]).
is_word :- word([_,_,_,_,_]).

:- end_lpad.


/** <examples>
?- mc_sample(is_word,1000,P).
% the probability of getting a word of length 3/4/5
% Expected result 0.067222
?- mc_mh_sample_arg(word([A,B,C]),word([_,_,_]),10,1,[A,B,C],V).
% take 10 samples of 3 token words given that the lenght of the word is 3
?- mc_mh_sample_arg_bar(word([A,B,C]),word([_,_,_]),10,1,[A,B,C],V).
% take 10 samples of 3 token words given that the lenght of the word is 3
% and draw a bar chart of the results
?- mc_mh_sample_arg(query(X),is_word,10,1,X,V).
% take 10 samples of 3, 4 or 5 token words given that the lenght of the word is
% 3, 5 or 5
%
*/
 
