/*
UWCSE model describing a university domain.
This theory is the result of translating a Logical Bayesian Network (LBN) constructed on the UWCSE dataset into CP-logic
From
http://dtai.cs.kuleuven.be/cplve/ilp09/
Reference:
Wannes Meert, Jan Struyf, Hendrik Blockeel: CP-Logic Theory Inference with Contextual Variable Elimination and Comparison to BDD Based Inference Methods. ILP 2009:96-109
*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.

:- pita.

:- begin_lpad.

course(c1).

professor(p1).

student(s1).

student(s2).

advised_by(A,B):0.10708782742681 :-
    student(A),
    professor(B),
    position(B,faculty).

advised_by(A,B):0.0278422273781903 :-
    student(A),
    professor(B),
    \+position(B,faculty).

course_level(A,level_300):0.0666666666666667; course_level(A,level_400):0.318518518518519; course_level(A,level_500):0.614814814814815 :-
    course(A).

phase(A,post_Generals):0.935483870967742; phase(A,post_Quals):0.032258064516129; phase(A,pre_Quals):0.032258064516129 :-
    student(A),
    years_in_program(A,year_6_or_more).

phase(A,post_Generals):0.04; phase(A,post_Quals):0.04; phase(A,pre_Quals):0.92 :-
    student(A),
    \+years_in_program(A,year_6_or_more),
    years_in_program(A,year_1).

phase(A,post_Generals):0.566666666666667; phase(A,post_Quals):0.366666666666667; phase(A,pre_Quals):0.0666666666666667 :-
    student(A),
    \+years_in_program(A,year_6_or_more),
    \+years_in_program(A,year_1),
    years_in_program(A,year_5).

phase(A,post_Generals):0.222222222222222; phase(A,post_Quals):0.666666666666667; phase(A,pre_Quals):0.111111111111111 :-
    student(A),
    \+years_in_program(A,year_6_or_more),
    \+years_in_program(A,year_1),
    \+years_in_program(A,year_5),
    years_in_program(A,year_4).

phase(A,post_Generals):0.0476190476190476; phase(A,post_Quals):0.547619047619048; phase(A,pre_Quals):0.404761904761905 :-
    student(A),
    \+years_in_program(A,year_6_or_more),
    \+years_in_program(A,year_1),
    \+years_in_program(A,year_5),
    \+years_in_program(A,year_4).

position(A,faculty):0.732142857142857; position(A,faculty_adjunct):0.125; position(A,faculty_affiliate):0.0714285714285714; position(A,faculty_emeritus):0.0714285714285714 :-
    professor(A).

aux1_prof_nb_publications(A) :-
    advised_by(B,A),
    student_nb_publications(B,three_or_more).

prof_nb_publications(A,none):0.0416666666666667; prof_nb_publications(A,one_to_nine):0.416666666666667; prof_nb_publications(A,ten_or_more):0.541666666666667 :-
    professor(A),
    aux1_prof_nb_publications(A).

prof_nb_publications(A,none):0.5; prof_nb_publications(A,one_to_nine):0.294117647058824; prof_nb_publications(A,ten_or_more):0.205882352941176 :-
    professor(A),
    \+aux1_prof_nb_publications(A).

shared_publication(A,B):0.00101010101010101 :-
    professor(A),
    student(B),
    student_nb_publications(B,none).

shared_publication(A,B):0.956521739130435 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    advised_by(B,A),
    years_in_program(B,year_6_or_more).

shared_publication(A,B):0.125 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    advised_by(B,A),
    \+years_in_program(B,year_6_or_more),
    prof_nb_publications(A,none).

shared_publication(A,B):0.631578947368421 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    advised_by(B,A),
    \+years_in_program(B,year_6_or_more),
    \+prof_nb_publications(A,none).

shared_publication(A,B):0.00515463917525773 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    \+advised_by(B,A),
    prof_nb_publications(A,none).

shared_publication(A,B):0.099236641221374 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    \+advised_by(B,A),
    \+prof_nb_publications(A,none),
    prof_nb_publications(A,one_to_nine),
    student_nb_publications(B,one_or_two).

shared_publication(A,B):0.0152671755725191 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    \+advised_by(B,A),
    \+prof_nb_publications(A,none),
    prof_nb_publications(A,one_to_nine),
    \+student_nb_publications(B,one_or_two).

shared_publication(A,B):0.146017699115044 :-
    professor(A),
    student(B),
    \+student_nb_publications(B,none),
    \+advised_by(B,A),
    \+prof_nb_publications(A,none),
    \+prof_nb_publications(A,one_to_nine).

student_nb_publications(A,none):0.594405594405594; student_nb_publications(A,one_or_two):0.195804195804196; student_nb_publications(A,three_or_more):0.20979020979021 :-
    student(A).

aux1_taught_by(A,B) :-
    teaching_assistant(A,C),
    advised_by(C,B).

aux2_taught_by(A,B) :-
    teaching_assistant(A,C),
    advised_by(C,B),
    shared_publication(B,C).

taught_by(A,B):0.777777777777778 :-
    course(A),
    professor(B),
    aux1_taught_by(A,B),
    aux2_taught_by(A,B).

taught_by(A,B):0.333333333333333 :-
    course(A),
    professor(B),
    aux1_taught_by(A,B),
    \+aux2_taught_by(A,B).

taught_by(A,B):0.266666666666667 :-
    course(A),
    professor(B),
    \+aux1_taught_by(A,B),
    position(B,faculty),
    course_level(A,level_300).

taught_by(A,B):0.102941176470588 :-
    course(A),
    professor(B),
    \+aux1_taught_by(A,B),
    position(B,faculty),
    \+course_level(A,level_300).

taught_by(A,B):0.029673590504451 :-
    course(A),
    professor(B),
    \+aux1_taught_by(A,B),
    \+position(B,faculty).

teaching_assistant(A,B):0.0333333333333333 :-
    course(A),
    student(B),
    course_level(A,level_300),
    student_nb_publications(B,three_or_more).

teaching_assistant(A,B):0.158227848101266 :-
    course(A),
    student(B),
    course_level(A,level_300),
    \+student_nb_publications(B,three_or_more).

teaching_assistant(A,B):0.0337519623233909 :-
    course(A),
    student(B),
    \+course_level(A,level_300),
    course_level(A,level_400).

teaching_assistant(A,B):0.0120999219359875 :-
    course(A),
    student(B),
    \+course_level(A,level_300),
    \+course_level(A,level_400).

years_in_program(A,year_1):0.244444444444444; years_in_program(A,year_2):0.155555555555556; years_in_program(A,year_3):0.233333333333333; years_in_program(A,year_4):0.144444444444444; years_in_program(A,year_5):0.111111111111111; years_in_program(A,year_6_or_more):0.111111111111111 :-
    student(A),
    student_nb_publications(A,none).

years_in_program(A,year_1):0.032258064516129; years_in_program(A,year_2):0.0645161290322581; years_in_program(A,year_3):0.0645161290322581; years_in_program(A,year_4):0.209677419354839; years_in_program(A,year_5):0.306451612903226; years_in_program(A,year_6_or_more):0.32258064516129 :-
    student(A),
    \+student_nb_publications(A,none).

:- end_lpad.

/** <examples>

?- prob(taught_by(c1,p1),Prob).  % what is the probability that course c1 is taught by professor p1?
% expected result 0.09265809305111444
?- prob_bar(taught_by(c1,p1),Prob).  % what is the probability that course c1 is taught by professor p1?
% expected result 0.09265809305111444


*/

