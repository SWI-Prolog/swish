/* 
Registration dataset, it contains information about participants in a recent 
Seminar on Data Mining. 
We would like to find out what type of people attend the parties at the seminar.
From 
L. De Raedt, H. Blockeel, L. Dehaspe, and W. Van Laer. Three companions for data mining in first order logic. In S. Dzeroski and N. Lavrac, editors, Relational Data Mining, pages 105-139.  Springer-Verlag, 2001.

See also The ACE Data Mining System User's Manual
http://dtai.cs.kuleuven.be/ACE/doc/ACEuser-1.2.16.pdf

Downloaded from 
http://dtai.cs.kuleuven.be/static/ACE/doc/
*/

/** <examples>
?- induce_par([rand_train],P),test(P,[rand_test],LL,AUCROC,ROC,AUCPR,PR).
?- in(P),test(P,[all],LL,AUCROC,ROC,AUCPR,PR).
?- induce([all],P),test(P,[all],LL,AUCROC,ROC,AUCPR,PR).
*/

:-use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-sc.

:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).
:- set_sc(megaex_bottom,7).
%:- set_sc(max_iter,2).
%:- set_sc(max_iter_structure,5).
:- set_sc(verbosity,1).

:- begin_bg.
company_info(jvt,commercial).
company_info(scuf,university).
company_info(ucro,university).
course(cso,2,introductory).
course(erm,3,introductory).
course(so2,4,introductory).
course(srw,3,advanced).



job(J):-
	participant(J, _, _, _).
company(C):-
	participant(_, C, _, _).

party_yes :- party(yes).
party_no :- party(no).

company_type(T):-
	company(C),
	company_info(C, T).

not_company_type(commercial):-
  \+ company_type(commercial).

not_company_type(university):-
  \+ company_type(university).

course_len(C, L):-
	course(C, L, _).
	
course_type(C, T):-
	course(C, _, T).

:- end_bg.

:- begin_in.
party(yes):0.5:-
  company_type(commercial).

party(no):0.5:-
  subscription(A),
  course_len(A,4),
  \+ company_type(commercial).
:- end_in.

fold(all,F):-
  findall(I,int(I),F).

fold(test,[adams,scott]).
fold(train,[blake, king, miller, turner]).

output(party/1).

input_cw(job/1).
input_cw(not_company_type/1).
input_cw(company_type/1).
input_cw(subscription/1).
input_cw(course_len/2).
input_cw(course_type/2).
input_cw(company/1).
input_cw(company_info/2).
input_cw(participant/4).
input_cw(course/3)/

determination(party/1,job/1).
determination(party/1,not_company_type/1).
determination(party/1,company_type/1).
determination(party/1,subscription/1).
determination(party/1,course_len/2).
determination(party/1,course_type/2).

%modeh(*,[party(yes),party(no)],
%  [party(yes),party(no)],
%  [job/1,company_type/1,subscription/1,course_len/2,course_type/2]).


modeh(*,party(yes)).
modeh(*,party(no)).

modeb(*,job(-#job)).
modeb(*,company_type(-#ctype)).
modeb(*,not_company_type(-#ctype)).
modeb(*,subscription(-sub)).
modeb(*,course_len(+sub,-#cl)).
modeb(*,course_type(+sub,-#ct)).

neg(party(M,yes)):- party(M,no).
neg(party(M,no)):- party(M,yes).

party(M,P):-
  participant(M,_, _, P, _).


begin(model(adams)).
participant(researcher,scuf,no,23).
subscription(erm).
subscription(so2).
subscription(srw).
end(model(adams)).

begin(model(blake)).
participant(president,jvt,yes,5).
subscription(cso).
subscription(erm).
end(model(blake)).

begin(model(king)).
participant(manager,ucro,no,78).
subscription(cso).
subscription(erm).
subscription(so2).
subscription(srw).
end(model(king)).

begin(model(miller)).
participant(manager,jvt,yes,14).
subscription(so2).
end(model(miller)).

begin(model(scott)).
participant(researcher,scuf,yes,94).
subscription(erm).
subscription(srw).
end(model(scott)).

begin(model(turner)).
participant(researcher,ucro,no,81).
subscription(so2).
subscription(srw).
end(model(turner)).

:- fold(all,F),
   sample(4,F,FTr,FTe),
   assert(fold(rand_train,FTr)),
   assert(fold(rand_test,FTe)).


