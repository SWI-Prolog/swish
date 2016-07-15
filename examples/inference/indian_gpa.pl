/*
The Indian GPA Problem. From 
http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=indian-gpa 
"This example was inspired by Stuart Russell...the problem is: if you observe 
that a student GPA is exactly 4.04.0 in a model of transcripts of students 
from the USA (GPA's from 0.00.0 to 4.04.0 and India (GPA's from 0.00.0 to 
10.010.0) what is the probability that the student is from India?... 
As we know from statistics, given the mixture distribution and given the 
fact that his/her GPA is exactly 4.04.0, the probability that the student 
is American must be 1.01.0 
(i.e. zero probability that the student is from India)."
Probabilistic logic program from 
https://github.com/davidenitti/DC/blob/master/examples/indian-gpa.pl
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

is_density_A:0.95;is_discrete_A:0.05.
% the probability distribution of GPA scores for American students is
% continuous with probability 0.95 and discrete with probability 0.05

agpa(A): beta(A,8,2) :- is_density_A.
% the GPA of American students follows a beta distribution if the
% distribution is continuous

american_gpa(G) : finite(G,[4.0:0.85,0.0:0.15]) :- is_discrete_A.
% the GPA of American students is 4.0 with probability 0.85 and 0.0 with 
% probability 0.15 if the
% distribution is discrete
american_gpa(A):- agpa(A0), A is A0*4.0.
% the GPA of American students is obtained by rescaling the value of agpa
% to the (0.0,4.0) interval
is_density_I : 0.99; is_discrete_I:0.01.
% the probability distribution of GPA scores for Indian students is
% continuous with probability 0.99 and discrete with probability 0.01
igpa(I): beta(I,5,5) :- is_density_I.
% the GPA of Indian students follows a beta distribution if the
% distribution is continuous
indian_gpa(I): finite(I,[0.0:0.1,10.0:0.9]):-  is_discrete_I.
% the GPA of Indian students is 10.0 with probability 0.9 and 0.0 with
% probability 0.1 if the
% distribution is discrete
indian_gpa(I) :- igpa(I0), I is I0*10.0.
% the GPA of Indian students is obtained by rescaling the value of igpa
% to the (0.0,4.0) interval
nation(N) : finite(N,[a:0.25,i:0.75]).
% the nation is America with probability 0.25 and India with probability 0.75
student_gpa(G):- nation(a),american_gpa(G).
% the GPA of the student is given by american_gpa if the nation is America
student_gpa(G) :- nation(i),indian_gpa(G).
% the GPA of the student is given by indian_gpa if the nation is India

:- end_lpad.


/** <examples>
?- mc_lw_sample(nation(a),student_gpa(4.0),1000,PPost).
% probability that the nation is America given that the student got 4.0
% in his GPA
% expected result: 1.0
?- mc_sample(nation(a),1000,_T,_F,PPrior).
% prior probability that the nation is America 
% expected result: 0.25

*/
 
