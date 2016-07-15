/* Shop dataset from
Fabrizio Riguzzi and Nicola Di Mauro. Applying the information bottleneck
to statistical relational learning. Machine Learning, 86(1):89-114, 2012.
Meert, W., Struyf, J., and Blockeel, H. 2008.
Learning ground CP-Logic theories by leveraging Bayesian network learning
techniques. Fundamenta Informaticae 89, 131-160

The training examples are all possible worlds of the target programi (shop4). 
The prob fact in each model/interpretation/world indicates its probability 
(it can be interpreted as frequency in a sampled dataset).
The task is to recover
the values of the parameters of the input program. When learning, the initial
parameters are randomly set. A test set is also provided generated randomly from
the target program.

*/
/** <examples>
?- induce_par([train],P),test(P,[test],LL,AUCROC,ROC,AUCPR,PR).  % learn the parameteters and test the result
?- induce_par([train],P).  % learn the parameteters 
?- in(P),test(P,[test],LL,AUCROC,ROC,AUCPR,PR). % test the input theory
*/


:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-sc.

:- set_sc(verbosity,1).
:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).

bg([]).

fold(train,[train1,train2,train3,train4,train5,train6,train7,train8,train9]).
fold(test,L):-
  findall(V,between(1,1000,V),L).

output(bought/1).

output(shops/1).

modeh(*,shops(+p)).
modeh(*,bought(+f)).

in([
(shops(john) : 0.2),
(shops(mary) : 0.9),
(bought(spaghetti ) : 0.5; bought(steak) : 0.5 :- shops(john)),
(bought(spaghetti ) : 0.3; bought(fish) : 0.7:-  shops(mary))]).

begin(model(train1)).
neg(bought(fish)).
bought(steak).
bought(spaghetti).
shops(mary).
shops(john).
prob(0.027000).
end(model(train1)).

begin(model(train2)).
bought(fish).
neg(bought(steak)).
bought(spaghetti).
shops(mary).
shops(john).
prob(0.063000).
end(model(train2)).

begin(model(train3)).
neg(bought(fish)).
neg(bought(steak)).
bought(spaghetti).
shops(mary).
shops(john).
prob(0.027000).
end(model(train3)).

begin(model(train4)).
bought(fish).
bought(steak).
neg(bought(spaghetti)).
shops(mary).
shops(john).
prob(0.063000).
end(model(train4)).

begin(model(train5)).
neg(bought(fish)).
neg(bought(steak)).
bought(spaghetti).
neg(shops(mary)).
shops(john).
prob(0.010000).
end(model(train5)).

begin(model(train6)).
neg(bought(fish)).
bought(steak).
neg(bought(spaghetti)).
neg(shops(mary)).
shops(john).
prob(0.010000).
end(model(train6)).

begin(model(train7)).
neg(bought(fish)).
neg(bought(steak)).
bought(spaghetti).
shops(mary).
neg(shops(john)).
prob(0.216000).
end(model(train7)).

begin(model(train8)).
bought(fish).
neg(bought(steak)).
neg(bought(spaghetti)).
shops(mary).
neg(shops(john)).
prob(0.504000).
end(model(train8)).

begin(model(train9)).
neg(bought(fish)).
neg(bought(steak)).
neg(bought(spaghetti)).
neg(shops(mary)).
neg(shops(john)).
prob(0.080000).
end(model(train9)).

neg(shops(A,john)):-
  number(A),
  \+ shops(A,john).

neg(shops(A,mary)):-
  number(A),
  \+ shops(A,mary).

neg(bought(A,spaghetti)):-
  number(A),
  \+ shops(A,spaghetti).

neg(bought(A,fish)):-
  number(A),
  \+ shops(A,fish).

neg(bought(A,steak)):-
  number(A),
  \+ shops(A,steak).

begin(model(1)).
bought(fish).
shops(mary).
end(model(1)).

begin(model(2)).
bought(spaghetti).
shops(mary).
end(model(2)).

begin(model(3)).
bought(fish).
shops(mary).
end(model(3)).

begin(model(4)).
bought(fish).
shops(mary).
end(model(4)).

begin(model(5)).
bought(fish).
shops(mary).
end(model(5)).

begin(model(6)).
bought(fish).
shops(mary).
end(model(6)).

begin(model(7)).
bought(fish).
shops(mary).
end(model(7)).

begin(model(8)).
bought(fish).
shops(mary).
end(model(8)).

begin(model(9)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(9)).

begin(model(10)).
bought(fish).
shops(mary).
end(model(10)).

begin(model(11)).
bought(fish).
shops(mary).
end(model(11)).

begin(model(12)).
bought(fish).
shops(mary).
end(model(12)).

begin(model(13)).
bought(spaghetti).
shops(mary).
end(model(13)).

begin(model(14)).
bought(fish).
shops(mary).
end(model(14)).

begin(model(15)).
bought(spaghetti).
shops(john).
end(model(15)).

begin(model(16)).
end(model(16)).

begin(model(17)).
bought(fish).
shops(mary).
end(model(17)).

begin(model(18)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(18)).

begin(model(19)).
bought(spaghetti).
shops(mary).
end(model(19)).

begin(model(20)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(20)).

begin(model(21)).
bought(fish).
shops(mary).
end(model(21)).

begin(model(22)).
bought(fish).
shops(mary).
end(model(22)).

begin(model(23)).
bought(fish).
shops(mary).
end(model(23)).

begin(model(24)).
bought(fish).
shops(mary).
end(model(24)).

begin(model(25)).
bought(fish).
shops(mary).
end(model(25)).

begin(model(26)).
end(model(26)).

begin(model(27)).
bought(fish).
shops(mary).
end(model(27)).

begin(model(28)).
bought(fish).
shops(mary).
end(model(28)).

begin(model(29)).
bought(spaghetti).
shops(mary).
end(model(29)).

begin(model(30)).
bought(fish).
shops(mary).
end(model(30)).

begin(model(31)).
bought(fish).
shops(mary).
end(model(31)).

begin(model(32)).
bought(fish).
shops(mary).
end(model(32)).

begin(model(33)).
bought(fish).
shops(mary).
end(model(33)).

begin(model(34)).
bought(spaghetti).
shops(mary).
end(model(34)).

begin(model(35)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(35)).

begin(model(36)).
bought(spaghetti).
shops(mary).
end(model(36)).

begin(model(37)).
bought(fish).
shops(mary).
end(model(37)).

begin(model(38)).
bought(spaghetti).
shops(mary).
end(model(38)).

begin(model(39)).
bought(fish).
shops(mary).
end(model(39)).

begin(model(40)).
bought(fish).
shops(mary).
end(model(40)).

begin(model(41)).
bought(spaghetti).
shops(mary).
end(model(41)).

begin(model(42)).
bought(spaghetti).
shops(mary).
end(model(42)).

begin(model(43)).
bought(spaghetti).
shops(mary).
end(model(43)).

begin(model(44)).
bought(spaghetti).
shops(john).
end(model(44)).

begin(model(45)).
bought(fish).
shops(mary).
end(model(45)).

begin(model(46)).
end(model(46)).

begin(model(47)).
bought(fish).
shops(mary).
end(model(47)).

begin(model(48)).
bought(fish).
shops(mary).
end(model(48)).

begin(model(49)).
bought(fish).
shops(mary).
end(model(49)).

begin(model(50)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(50)).

begin(model(51)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(51)).

begin(model(52)).
bought(fish).
shops(mary).
end(model(52)).

begin(model(53)).
bought(fish).
shops(mary).
end(model(53)).

begin(model(54)).
bought(fish).
shops(mary).
end(model(54)).

begin(model(55)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(55)).

begin(model(56)).
bought(fish).
shops(mary).
end(model(56)).

begin(model(57)).
bought(fish).
shops(mary).
end(model(57)).

begin(model(58)).
bought(spaghetti).
shops(mary).
end(model(58)).

begin(model(59)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(59)).

begin(model(60)).
bought(spaghetti).
shops(mary).
end(model(60)).

begin(model(61)).
bought(spaghetti).
shops(mary).
end(model(61)).

begin(model(62)).
bought(spaghetti).
shops(mary).
end(model(62)).

begin(model(63)).
bought(fish).
shops(mary).
end(model(63)).

begin(model(64)).
bought(spaghetti).
shops(mary).
end(model(64)).

begin(model(65)).
bought(fish).
shops(mary).
end(model(65)).

begin(model(66)).
bought(fish).
shops(mary).
end(model(66)).

begin(model(67)).
bought(fish).
shops(mary).
end(model(67)).

begin(model(68)).
bought(spaghetti).
shops(mary).
end(model(68)).

begin(model(69)).
bought(fish).
shops(mary).
end(model(69)).

begin(model(70)).
bought(fish).
shops(mary).
end(model(70)).

begin(model(71)).
bought(fish).
shops(mary).
end(model(71)).

begin(model(72)).
bought(fish).
shops(mary).
end(model(72)).

begin(model(73)).
bought(fish).
shops(mary).
end(model(73)).

begin(model(74)).
bought(fish).
shops(mary).
end(model(74)).

begin(model(75)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(75)).

begin(model(76)).
bought(steak).
shops(john).
end(model(76)).

begin(model(77)).
bought(fish).
shops(mary).
end(model(77)).

begin(model(78)).
bought(spaghetti).
shops(mary).
end(model(78)).

begin(model(79)).
bought(steak).
shops(john).
end(model(79)).

begin(model(80)).
bought(fish).
shops(mary).
end(model(80)).

begin(model(81)).
bought(spaghetti).
shops(mary).
end(model(81)).

begin(model(82)).
bought(fish).
shops(mary).
end(model(82)).

begin(model(83)).
bought(spaghetti).
shops(mary).
end(model(83)).

begin(model(84)).
bought(fish).
shops(mary).
end(model(84)).

begin(model(85)).
bought(fish).
shops(mary).
end(model(85)).

begin(model(86)).
bought(fish).
shops(mary).
end(model(86)).

begin(model(87)).
bought(spaghetti).
shops(mary).
end(model(87)).

begin(model(88)).
bought(spaghetti).
shops(john).
end(model(88)).

begin(model(89)).
bought(spaghetti).
shops(mary).
end(model(89)).

begin(model(90)).
bought(fish).
shops(mary).
end(model(90)).

begin(model(91)).
bought(spaghetti).
shops(mary).
end(model(91)).

begin(model(92)).
bought(fish).
shops(mary).
end(model(92)).

begin(model(93)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(93)).

begin(model(94)).
bought(spaghetti).
shops(mary).
end(model(94)).

begin(model(95)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(95)).

begin(model(96)).
bought(fish).
shops(mary).
end(model(96)).

begin(model(97)).
bought(fish).
shops(mary).
end(model(97)).

begin(model(98)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(98)).

begin(model(99)).
bought(spaghetti).
shops(mary).
end(model(99)).

begin(model(100)).
bought(spaghetti).
shops(mary).
end(model(100)).

begin(model(101)).
bought(fish).
shops(mary).
end(model(101)).

begin(model(102)).
bought(fish).
shops(mary).
end(model(102)).

begin(model(103)).
bought(fish).
shops(mary).
end(model(103)).

begin(model(104)).
bought(fish).
shops(mary).
end(model(104)).

begin(model(105)).
end(model(105)).

begin(model(106)).
bought(fish).
shops(mary).
end(model(106)).

begin(model(107)).
bought(spaghetti).
shops(mary).
end(model(107)).

begin(model(108)).
bought(fish).
shops(mary).
end(model(108)).

begin(model(109)).
bought(fish).
shops(mary).
end(model(109)).

begin(model(110)).
bought(fish).
shops(mary).
end(model(110)).

begin(model(111)).
bought(fish).
shops(mary).
end(model(111)).

begin(model(112)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(112)).

begin(model(113)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(113)).

begin(model(114)).
bought(fish).
shops(mary).
end(model(114)).

begin(model(115)).
bought(spaghetti).
shops(mary).
end(model(115)).

begin(model(116)).
bought(fish).
shops(mary).
end(model(116)).

begin(model(117)).
bought(spaghetti).
shops(mary).
end(model(117)).

begin(model(118)).
bought(fish).
shops(mary).
end(model(118)).

begin(model(119)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(119)).

begin(model(120)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(120)).

begin(model(121)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(121)).

begin(model(122)).
bought(fish).
shops(mary).
end(model(122)).

begin(model(123)).
end(model(123)).

begin(model(124)).
bought(fish).
shops(mary).
end(model(124)).

begin(model(125)).
bought(fish).
shops(mary).
end(model(125)).

begin(model(126)).
bought(spaghetti).
shops(mary).
end(model(126)).

begin(model(127)).
bought(fish).
shops(mary).
end(model(127)).

begin(model(128)).
bought(fish).
shops(mary).
end(model(128)).

begin(model(129)).
bought(fish).
shops(mary).
end(model(129)).

begin(model(130)).
bought(fish).
shops(mary).
end(model(130)).

begin(model(131)).
bought(fish).
shops(mary).
end(model(131)).

begin(model(132)).
bought(fish).
shops(mary).
end(model(132)).

begin(model(133)).
bought(fish).
shops(mary).
end(model(133)).

begin(model(134)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(134)).

begin(model(135)).
bought(fish).
shops(mary).
end(model(135)).

begin(model(136)).
bought(fish).
shops(mary).
end(model(136)).

begin(model(137)).
bought(fish).
shops(mary).
end(model(137)).

begin(model(138)).
bought(fish).
shops(mary).
end(model(138)).

begin(model(139)).
bought(fish).
shops(mary).
end(model(139)).

begin(model(140)).
bought(fish).
shops(mary).
end(model(140)).

begin(model(141)).
end(model(141)).

begin(model(142)).
bought(spaghetti).
shops(mary).
end(model(142)).

begin(model(143)).
bought(spaghetti).
shops(mary).
end(model(143)).

begin(model(144)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(144)).

begin(model(145)).
bought(spaghetti).
shops(mary).
end(model(145)).

begin(model(146)).
end(model(146)).

begin(model(147)).
bought(spaghetti).
shops(mary).
end(model(147)).

begin(model(148)).
bought(fish).
shops(mary).
end(model(148)).

begin(model(149)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(149)).

begin(model(150)).
end(model(150)).

begin(model(151)).
bought(steak).
shops(john).
end(model(151)).

begin(model(152)).
bought(spaghetti).
shops(mary).
end(model(152)).

begin(model(153)).
bought(fish).
shops(mary).
end(model(153)).

begin(model(154)).
bought(fish).
shops(mary).
end(model(154)).

begin(model(155)).
bought(fish).
shops(mary).
end(model(155)).

begin(model(156)).
bought(fish).
shops(mary).
end(model(156)).

begin(model(157)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(157)).

begin(model(158)).
bought(spaghetti).
shops(mary).
end(model(158)).

begin(model(159)).
bought(fish).
shops(mary).
end(model(159)).

begin(model(160)).
bought(spaghetti).
shops(mary).
end(model(160)).

begin(model(161)).
bought(fish).
shops(mary).
end(model(161)).

begin(model(162)).
bought(fish).
shops(mary).
end(model(162)).

begin(model(163)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(163)).

begin(model(164)).
bought(fish).
shops(mary).
end(model(164)).

begin(model(165)).
bought(spaghetti).
shops(mary).
end(model(165)).

begin(model(166)).
bought(spaghetti).
shops(mary).
end(model(166)).

begin(model(167)).
bought(fish).
shops(mary).
end(model(167)).

begin(model(168)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(168)).

begin(model(169)).
bought(fish).
shops(mary).
end(model(169)).

begin(model(170)).
bought(spaghetti).
shops(mary).
end(model(170)).

begin(model(171)).
bought(spaghetti).
shops(mary).
end(model(171)).

begin(model(172)).
bought(fish).
shops(mary).
end(model(172)).

begin(model(173)).
bought(fish).
shops(mary).
end(model(173)).

begin(model(174)).
bought(fish).
shops(mary).
end(model(174)).

begin(model(175)).
bought(fish).
shops(mary).
end(model(175)).

begin(model(176)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(176)).

begin(model(177)).
bought(fish).
shops(mary).
end(model(177)).

begin(model(178)).
bought(spaghetti).
shops(mary).
end(model(178)).

begin(model(179)).
bought(fish).
shops(mary).
end(model(179)).

begin(model(180)).
bought(fish).
shops(mary).
end(model(180)).

begin(model(181)).
bought(fish).
shops(mary).
end(model(181)).

begin(model(182)).
bought(fish).
shops(mary).
end(model(182)).

begin(model(183)).
bought(fish).
shops(mary).
end(model(183)).

begin(model(184)).
bought(spaghetti).
shops(mary).
end(model(184)).

begin(model(185)).
bought(fish).
shops(mary).
end(model(185)).

begin(model(186)).
bought(spaghetti).
shops(mary).
end(model(186)).

begin(model(187)).
bought(fish).
shops(mary).
end(model(187)).

begin(model(188)).
bought(spaghetti).
shops(mary).
end(model(188)).

begin(model(189)).
bought(spaghetti).
shops(mary).
end(model(189)).

begin(model(190)).
bought(spaghetti).
shops(mary).
end(model(190)).

begin(model(191)).
end(model(191)).

begin(model(192)).
bought(spaghetti).
shops(mary).
end(model(192)).

begin(model(193)).
bought(fish).
shops(mary).
end(model(193)).

begin(model(194)).
bought(fish).
shops(mary).
end(model(194)).

begin(model(195)).
bought(fish).
shops(mary).
end(model(195)).

begin(model(196)).
bought(fish).
shops(mary).
end(model(196)).

begin(model(197)).
bought(fish).
shops(mary).
end(model(197)).

begin(model(198)).
bought(fish).
shops(mary).
end(model(198)).

begin(model(199)).
bought(fish).
shops(mary).
end(model(199)).

begin(model(200)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(200)).

begin(model(201)).
bought(fish).
shops(mary).
end(model(201)).

begin(model(202)).
bought(fish).
shops(mary).
end(model(202)).

begin(model(203)).
bought(fish).
shops(mary).
end(model(203)).

begin(model(204)).
bought(fish).
shops(mary).
end(model(204)).

begin(model(205)).
bought(fish).
shops(mary).
end(model(205)).

begin(model(206)).
bought(fish).
shops(mary).
end(model(206)).

begin(model(207)).
bought(spaghetti).
shops(mary).
end(model(207)).

begin(model(208)).
bought(fish).
shops(mary).
end(model(208)).

begin(model(209)).
end(model(209)).

begin(model(210)).
bought(fish).
shops(mary).
end(model(210)).

begin(model(211)).
bought(fish).
shops(mary).
end(model(211)).

begin(model(212)).
bought(spaghetti).
shops(mary).
end(model(212)).

begin(model(213)).
bought(spaghetti).
shops(mary).
end(model(213)).

begin(model(214)).
bought(fish).
shops(mary).
end(model(214)).

begin(model(215)).
bought(fish).
shops(mary).
end(model(215)).

begin(model(216)).
bought(fish).
shops(mary).
end(model(216)).

begin(model(217)).
bought(fish).
shops(mary).
end(model(217)).

begin(model(218)).
bought(spaghetti).
shops(mary).
end(model(218)).

begin(model(219)).
bought(spaghetti).
shops(mary).
end(model(219)).

begin(model(220)).
bought(fish).
shops(mary).
end(model(220)).

begin(model(221)).
bought(fish).
shops(mary).
end(model(221)).

begin(model(222)).
bought(spaghetti).
shops(mary).
end(model(222)).

begin(model(223)).
bought(fish).
shops(mary).
end(model(223)).

begin(model(224)).
end(model(224)).

begin(model(225)).
end(model(225)).

begin(model(226)).
bought(fish).
shops(mary).
end(model(226)).

begin(model(227)).
bought(fish).
shops(mary).
end(model(227)).

begin(model(228)).
bought(fish).
shops(mary).
end(model(228)).

begin(model(229)).
bought(fish).
shops(mary).
end(model(229)).

begin(model(230)).
bought(spaghetti).
shops(mary).
end(model(230)).

begin(model(231)).
bought(spaghetti).
shops(mary).
end(model(231)).

begin(model(232)).
bought(fish).
shops(mary).
end(model(232)).

begin(model(233)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(233)).

begin(model(234)).
bought(spaghetti).
shops(mary).
end(model(234)).

begin(model(235)).
bought(fish).
shops(mary).
end(model(235)).

begin(model(236)).
bought(fish).
shops(mary).
end(model(236)).

begin(model(237)).
bought(fish).
shops(mary).
end(model(237)).

begin(model(238)).
bought(fish).
shops(mary).
end(model(238)).

begin(model(239)).
bought(fish).
shops(mary).
end(model(239)).

begin(model(240)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(240)).

begin(model(241)).
bought(fish).
shops(mary).
end(model(241)).

begin(model(242)).
bought(spaghetti).
shops(mary).
end(model(242)).

begin(model(243)).
bought(fish).
shops(mary).
end(model(243)).

begin(model(244)).
bought(spaghetti).
shops(mary).
end(model(244)).

begin(model(245)).
bought(fish).
shops(mary).
end(model(245)).

begin(model(246)).
bought(fish).
shops(mary).
end(model(246)).

begin(model(247)).
bought(fish).
shops(mary).
end(model(247)).

begin(model(248)).
bought(spaghetti).
shops(mary).
end(model(248)).

begin(model(249)).
bought(fish).
shops(mary).
end(model(249)).

begin(model(250)).
bought(spaghetti).
shops(mary).
end(model(250)).

begin(model(251)).
bought(spaghetti).
shops(mary).
end(model(251)).

begin(model(252)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(252)).

begin(model(253)).
bought(fish).
shops(mary).
end(model(253)).

begin(model(254)).
bought(fish).
shops(mary).
end(model(254)).

begin(model(255)).
bought(fish).
shops(mary).
end(model(255)).

begin(model(256)).
bought(fish).
shops(mary).
end(model(256)).

begin(model(257)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(257)).

begin(model(258)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(258)).

begin(model(259)).
bought(spaghetti).
shops(mary).
end(model(259)).

begin(model(260)).
bought(fish).
shops(mary).
end(model(260)).

begin(model(261)).
bought(fish).
shops(mary).
end(model(261)).

begin(model(262)).
bought(fish).
shops(mary).
end(model(262)).

begin(model(263)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(263)).

begin(model(264)).
end(model(264)).

begin(model(265)).
end(model(265)).

begin(model(266)).
bought(spaghetti).
shops(mary).
end(model(266)).

begin(model(267)).
bought(fish).
shops(mary).
end(model(267)).

begin(model(268)).
bought(fish).
shops(mary).
end(model(268)).

begin(model(269)).
bought(fish).
shops(mary).
end(model(269)).

begin(model(270)).
bought(spaghetti).
shops(mary).
end(model(270)).

begin(model(271)).
bought(fish).
shops(mary).
end(model(271)).

begin(model(272)).
bought(fish).
shops(mary).
end(model(272)).

begin(model(273)).
bought(fish).
shops(mary).
end(model(273)).

begin(model(274)).
bought(fish).
shops(mary).
end(model(274)).

begin(model(275)).
bought(fish).
shops(mary).
end(model(275)).

begin(model(276)).
bought(steak).
shops(john).
end(model(276)).

begin(model(277)).
bought(spaghetti).
shops(mary).
end(model(277)).

begin(model(278)).
bought(fish).
shops(mary).
end(model(278)).

begin(model(279)).
bought(fish).
shops(mary).
end(model(279)).

begin(model(280)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(280)).

begin(model(281)).
bought(fish).
shops(mary).
end(model(281)).

begin(model(282)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(282)).

begin(model(283)).
bought(spaghetti).
shops(mary).
end(model(283)).

begin(model(284)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(284)).

begin(model(285)).
bought(fish).
shops(mary).
end(model(285)).

begin(model(286)).
bought(fish).
shops(mary).
end(model(286)).

begin(model(287)).
bought(fish).
shops(mary).
end(model(287)).

begin(model(288)).
bought(fish).
shops(mary).
end(model(288)).

begin(model(289)).
bought(fish).
shops(mary).
end(model(289)).

begin(model(290)).
bought(fish).
shops(mary).
end(model(290)).

begin(model(291)).
end(model(291)).

begin(model(292)).
bought(fish).
shops(mary).
end(model(292)).

begin(model(293)).
bought(fish).
shops(mary).
end(model(293)).

begin(model(294)).
bought(steak).
shops(john).
end(model(294)).

begin(model(295)).
bought(fish).
shops(mary).
end(model(295)).

begin(model(296)).
bought(spaghetti).
shops(mary).
end(model(296)).

begin(model(297)).
bought(fish).
shops(mary).
end(model(297)).

begin(model(298)).
bought(spaghetti).
shops(mary).
end(model(298)).

begin(model(299)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(299)).

begin(model(300)).
bought(fish).
shops(mary).
end(model(300)).

begin(model(301)).
bought(fish).
shops(mary).
end(model(301)).

begin(model(302)).
bought(fish).
shops(mary).
end(model(302)).

begin(model(303)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(303)).

begin(model(304)).
bought(fish).
shops(mary).
end(model(304)).

begin(model(305)).
bought(fish).
shops(mary).
end(model(305)).

begin(model(306)).
bought(fish).
shops(mary).
end(model(306)).

begin(model(307)).
bought(fish).
shops(mary).
end(model(307)).

begin(model(308)).
bought(fish).
shops(mary).
end(model(308)).

begin(model(309)).
bought(fish).
shops(mary).
end(model(309)).

begin(model(310)).
bought(fish).
shops(mary).
end(model(310)).

begin(model(311)).
bought(spaghetti).
shops(mary).
end(model(311)).

begin(model(312)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(312)).

begin(model(313)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(313)).

begin(model(314)).
bought(spaghetti).
shops(john).
end(model(314)).

begin(model(315)).
bought(fish).
shops(mary).
end(model(315)).

begin(model(316)).
bought(fish).
shops(mary).
end(model(316)).

begin(model(317)).
bought(spaghetti).
shops(mary).
end(model(317)).

begin(model(318)).
bought(spaghetti).
shops(mary).
end(model(318)).

begin(model(319)).
bought(spaghetti).
shops(mary).
end(model(319)).

begin(model(320)).
bought(fish).
shops(mary).
end(model(320)).

begin(model(321)).
bought(fish).
shops(mary).
end(model(321)).

begin(model(322)).
bought(fish).
shops(mary).
end(model(322)).

begin(model(323)).
bought(fish).
shops(mary).
end(model(323)).

begin(model(324)).
bought(fish).
shops(mary).
end(model(324)).

begin(model(325)).
bought(fish).
shops(mary).
end(model(325)).

begin(model(326)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(326)).

begin(model(327)).
bought(fish).
shops(mary).
end(model(327)).

begin(model(328)).
bought(spaghetti).
shops(mary).
end(model(328)).

begin(model(329)).
bought(fish).
shops(mary).
end(model(329)).

begin(model(330)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(330)).

begin(model(331)).
bought(spaghetti).
shops(mary).
end(model(331)).

begin(model(332)).
bought(fish).
shops(mary).
end(model(332)).

begin(model(333)).
bought(spaghetti).
shops(mary).
end(model(333)).

begin(model(334)).
bought(spaghetti).
shops(mary).
end(model(334)).

begin(model(335)).
bought(fish).
shops(mary).
end(model(335)).

begin(model(336)).
bought(spaghetti).
shops(mary).
end(model(336)).

begin(model(337)).
bought(fish).
shops(mary).
end(model(337)).

begin(model(338)).
bought(fish).
shops(mary).
end(model(338)).

begin(model(339)).
bought(spaghetti).
shops(mary).
end(model(339)).

begin(model(340)).
bought(fish).
shops(mary).
end(model(340)).

begin(model(341)).
bought(fish).
shops(mary).
end(model(341)).

begin(model(342)).
bought(fish).
shops(mary).
end(model(342)).

begin(model(343)).
bought(fish).
shops(mary).
end(model(343)).

begin(model(344)).
bought(fish).
shops(mary).
end(model(344)).

begin(model(345)).
bought(fish).
shops(mary).
end(model(345)).

begin(model(346)).
bought(fish).
shops(mary).
end(model(346)).

begin(model(347)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(347)).

begin(model(348)).
bought(fish).
shops(mary).
end(model(348)).

begin(model(349)).
bought(fish).
shops(mary).
end(model(349)).

begin(model(350)).
bought(fish).
shops(mary).
end(model(350)).

begin(model(351)).
bought(spaghetti).
shops(mary).
end(model(351)).

begin(model(352)).
bought(fish).
shops(mary).
end(model(352)).

begin(model(353)).
bought(fish).
shops(mary).
end(model(353)).

begin(model(354)).
bought(fish).
shops(mary).
end(model(354)).

begin(model(355)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(355)).

begin(model(356)).
bought(fish).
shops(mary).
end(model(356)).

begin(model(357)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(357)).

begin(model(358)).
bought(fish).
shops(mary).
end(model(358)).

begin(model(359)).
bought(fish).
shops(mary).
end(model(359)).

begin(model(360)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(360)).

begin(model(361)).
bought(spaghetti).
shops(mary).
end(model(361)).

begin(model(362)).
bought(spaghetti).
shops(mary).
end(model(362)).

begin(model(363)).
bought(fish).
shops(mary).
end(model(363)).

begin(model(364)).
bought(spaghetti).
shops(mary).
end(model(364)).

begin(model(365)).
bought(fish).
shops(mary).
end(model(365)).

begin(model(366)).
bought(fish).
shops(mary).
end(model(366)).

begin(model(367)).
bought(fish).
shops(mary).
end(model(367)).

begin(model(368)).
bought(fish).
shops(mary).
end(model(368)).

begin(model(369)).
end(model(369)).

begin(model(370)).
bought(fish).
shops(mary).
end(model(370)).

begin(model(371)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(371)).

begin(model(372)).
bought(fish).
shops(mary).
end(model(372)).

begin(model(373)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(373)).

begin(model(374)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(374)).

begin(model(375)).
bought(fish).
shops(mary).
end(model(375)).

begin(model(376)).
bought(fish).
shops(mary).
end(model(376)).

begin(model(377)).
bought(fish).
shops(mary).
end(model(377)).

begin(model(378)).
bought(fish).
shops(mary).
end(model(378)).

begin(model(379)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(379)).

begin(model(380)).
bought(spaghetti).
shops(mary).
end(model(380)).

begin(model(381)).
bought(fish).
shops(mary).
end(model(381)).

begin(model(382)).
bought(fish).
shops(mary).
end(model(382)).

begin(model(383)).
bought(fish).
shops(mary).
end(model(383)).

begin(model(384)).
bought(fish).
shops(mary).
end(model(384)).

begin(model(385)).
bought(fish).
shops(mary).
end(model(385)).

begin(model(386)).
bought(fish).
shops(mary).
end(model(386)).

begin(model(387)).
bought(spaghetti).
shops(john).
end(model(387)).

begin(model(388)).
bought(fish).
shops(mary).
end(model(388)).

begin(model(389)).
bought(fish).
shops(mary).
end(model(389)).

begin(model(390)).
bought(fish).
shops(mary).
end(model(390)).

begin(model(391)).
bought(spaghetti).
shops(mary).
end(model(391)).

begin(model(392)).
bought(spaghetti).
shops(john).
end(model(392)).

begin(model(393)).
bought(fish).
shops(mary).
end(model(393)).

begin(model(394)).
bought(fish).
shops(mary).
end(model(394)).

begin(model(395)).
bought(spaghetti).
shops(mary).
end(model(395)).

begin(model(396)).
bought(fish).
shops(mary).
end(model(396)).

begin(model(397)).
bought(fish).
shops(mary).
end(model(397)).

begin(model(398)).
bought(fish).
shops(mary).
end(model(398)).

begin(model(399)).
bought(spaghetti).
shops(mary).
end(model(399)).

begin(model(400)).
bought(fish).
shops(mary).
end(model(400)).

begin(model(401)).
bought(fish).
shops(mary).
end(model(401)).

begin(model(402)).
bought(spaghetti).
shops(mary).
end(model(402)).

begin(model(403)).
bought(fish).
shops(mary).
end(model(403)).

begin(model(404)).
bought(fish).
shops(mary).
end(model(404)).

begin(model(405)).
bought(spaghetti).
shops(mary).
end(model(405)).

begin(model(406)).
bought(fish).
shops(mary).
end(model(406)).

begin(model(407)).
bought(fish).
shops(mary).
end(model(407)).

begin(model(408)).
bought(fish).
shops(mary).
end(model(408)).

begin(model(409)).
bought(steak).
shops(john).
end(model(409)).

begin(model(410)).
bought(fish).
shops(mary).
end(model(410)).

begin(model(411)).
bought(fish).
shops(mary).
end(model(411)).

begin(model(412)).
bought(fish).
shops(mary).
end(model(412)).

begin(model(413)).
bought(fish).
shops(mary).
end(model(413)).

begin(model(414)).
bought(fish).
shops(mary).
end(model(414)).

begin(model(415)).
bought(fish).
shops(mary).
end(model(415)).

begin(model(416)).
end(model(416)).

begin(model(417)).
bought(spaghetti).
shops(mary).
end(model(417)).

begin(model(418)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(418)).

begin(model(419)).
bought(spaghetti).
shops(mary).
end(model(419)).

begin(model(420)).
bought(spaghetti).
shops(mary).
end(model(420)).

begin(model(421)).
bought(fish).
shops(mary).
end(model(421)).

begin(model(422)).
bought(spaghetti).
shops(mary).
end(model(422)).

begin(model(423)).
bought(fish).
shops(mary).
end(model(423)).

begin(model(424)).
bought(fish).
shops(mary).
end(model(424)).

begin(model(425)).
bought(fish).
shops(mary).
end(model(425)).

begin(model(426)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(426)).

begin(model(427)).
bought(spaghetti).
shops(mary).
end(model(427)).

begin(model(428)).
bought(fish).
shops(mary).
end(model(428)).

begin(model(429)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(429)).

begin(model(430)).
bought(spaghetti).
shops(mary).
end(model(430)).

begin(model(431)).
bought(fish).
shops(mary).
end(model(431)).

begin(model(432)).
bought(fish).
shops(mary).
end(model(432)).

begin(model(433)).
bought(fish).
shops(mary).
end(model(433)).

begin(model(434)).
bought(fish).
shops(mary).
end(model(434)).

begin(model(435)).
bought(fish).
shops(mary).
end(model(435)).

begin(model(436)).
bought(fish).
shops(mary).
end(model(436)).

begin(model(437)).
bought(fish).
shops(mary).
end(model(437)).

begin(model(438)).
bought(fish).
shops(mary).
end(model(438)).

begin(model(439)).
bought(fish).
shops(mary).
end(model(439)).

begin(model(440)).
bought(spaghetti).
shops(mary).
end(model(440)).

begin(model(441)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(441)).

begin(model(442)).
bought(fish).
shops(mary).
end(model(442)).

begin(model(443)).
bought(spaghetti).
shops(mary).
end(model(443)).

begin(model(444)).
bought(fish).
shops(mary).
end(model(444)).

begin(model(445)).
bought(fish).
shops(mary).
end(model(445)).

begin(model(446)).
bought(spaghetti).
shops(mary).
end(model(446)).

begin(model(447)).
bought(fish).
shops(mary).
end(model(447)).

begin(model(448)).
bought(fish).
shops(mary).
end(model(448)).

begin(model(449)).
bought(fish).
shops(mary).
end(model(449)).

begin(model(450)).
bought(fish).
shops(mary).
end(model(450)).

begin(model(451)).
bought(fish).
shops(mary).
end(model(451)).

begin(model(452)).
bought(spaghetti).
shops(mary).
end(model(452)).

begin(model(453)).
bought(fish).
shops(mary).
end(model(453)).

begin(model(454)).
bought(fish).
shops(mary).
end(model(454)).

begin(model(455)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(455)).

begin(model(456)).
bought(spaghetti).
shops(mary).
end(model(456)).

begin(model(457)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(457)).

begin(model(458)).
bought(fish).
shops(mary).
end(model(458)).

begin(model(459)).
bought(fish).
shops(mary).
end(model(459)).

begin(model(460)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(460)).

begin(model(461)).
bought(spaghetti).
shops(mary).
end(model(461)).

begin(model(462)).
bought(fish).
shops(mary).
end(model(462)).

begin(model(463)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(463)).

begin(model(464)).
bought(spaghetti).
shops(mary).
end(model(464)).

begin(model(465)).
bought(fish).
shops(mary).
end(model(465)).

begin(model(466)).
bought(fish).
shops(mary).
end(model(466)).

begin(model(467)).
bought(fish).
shops(mary).
end(model(467)).

begin(model(468)).
bought(steak).
shops(john).
end(model(468)).

begin(model(469)).
bought(steak).
shops(john).
end(model(469)).

begin(model(470)).
bought(fish).
shops(mary).
end(model(470)).

begin(model(471)).
bought(fish).
shops(mary).
end(model(471)).

begin(model(472)).
bought(fish).
shops(mary).
end(model(472)).

begin(model(473)).
bought(fish).
shops(mary).
end(model(473)).

begin(model(474)).
bought(spaghetti).
shops(mary).
end(model(474)).

begin(model(475)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(475)).

begin(model(476)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(476)).

begin(model(477)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(477)).

begin(model(478)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(478)).

begin(model(479)).
bought(spaghetti).
shops(mary).
end(model(479)).

begin(model(480)).
bought(fish).
shops(mary).
end(model(480)).

begin(model(481)).
bought(fish).
shops(mary).
end(model(481)).

begin(model(482)).
bought(fish).
shops(mary).
end(model(482)).

begin(model(483)).
bought(fish).
shops(mary).
end(model(483)).

begin(model(484)).
bought(fish).
shops(mary).
end(model(484)).

begin(model(485)).
bought(spaghetti).
shops(mary).
end(model(485)).

begin(model(486)).
bought(spaghetti).
shops(mary).
end(model(486)).

begin(model(487)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(487)).

begin(model(488)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(488)).

begin(model(489)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(489)).

begin(model(490)).
bought(fish).
shops(mary).
end(model(490)).

begin(model(491)).
bought(spaghetti).
shops(mary).
end(model(491)).

begin(model(492)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(492)).

begin(model(493)).
bought(fish).
shops(mary).
end(model(493)).

begin(model(494)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(494)).

begin(model(495)).
bought(spaghetti).
shops(mary).
end(model(495)).

begin(model(496)).
bought(spaghetti).
shops(mary).
end(model(496)).

begin(model(497)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(497)).

begin(model(498)).
bought(fish).
shops(mary).
end(model(498)).

begin(model(499)).
bought(fish).
shops(mary).
end(model(499)).

begin(model(500)).
bought(fish).
shops(mary).
end(model(500)).

begin(model(501)).
bought(spaghetti).
shops(mary).
end(model(501)).

begin(model(502)).
bought(fish).
shops(mary).
end(model(502)).

begin(model(503)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(503)).

begin(model(504)).
bought(fish).
shops(mary).
end(model(504)).

begin(model(505)).
bought(spaghetti).
shops(mary).
end(model(505)).

begin(model(506)).
bought(fish).
shops(mary).
end(model(506)).

begin(model(507)).
bought(fish).
shops(mary).
end(model(507)).

begin(model(508)).
bought(spaghetti).
shops(mary).
end(model(508)).

begin(model(509)).
bought(fish).
shops(mary).
end(model(509)).

begin(model(510)).
bought(fish).
shops(mary).
end(model(510)).

begin(model(511)).
bought(spaghetti).
shops(mary).
end(model(511)).

begin(model(512)).
bought(fish).
shops(mary).
end(model(512)).

begin(model(513)).
bought(fish).
shops(mary).
end(model(513)).

begin(model(514)).
bought(fish).
shops(mary).
end(model(514)).

begin(model(515)).
bought(spaghetti).
shops(mary).
end(model(515)).

begin(model(516)).
bought(spaghetti).
shops(john).
end(model(516)).

begin(model(517)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(517)).

begin(model(518)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(518)).

begin(model(519)).
bought(fish).
shops(mary).
end(model(519)).

begin(model(520)).
bought(fish).
shops(mary).
end(model(520)).

begin(model(521)).
bought(fish).
shops(mary).
end(model(521)).

begin(model(522)).
bought(fish).
shops(mary).
end(model(522)).

begin(model(523)).
bought(fish).
shops(mary).
end(model(523)).

begin(model(524)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(524)).

begin(model(525)).
bought(spaghetti).
shops(mary).
end(model(525)).

begin(model(526)).
bought(fish).
shops(mary).
end(model(526)).

begin(model(527)).
bought(fish).
shops(mary).
end(model(527)).

begin(model(528)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(528)).

begin(model(529)).
bought(spaghetti).
shops(mary).
end(model(529)).

begin(model(530)).
bought(spaghetti).
shops(mary).
end(model(530)).

begin(model(531)).
bought(fish).
shops(mary).
end(model(531)).

begin(model(532)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(532)).

begin(model(533)).
end(model(533)).

begin(model(534)).
bought(fish).
shops(mary).
end(model(534)).

begin(model(535)).
bought(spaghetti).
shops(mary).
end(model(535)).

begin(model(536)).
bought(spaghetti).
shops(mary).
end(model(536)).

begin(model(537)).
bought(fish).
shops(mary).
end(model(537)).

begin(model(538)).
bought(fish).
shops(mary).
end(model(538)).

begin(model(539)).
bought(fish).
shops(mary).
end(model(539)).

begin(model(540)).
bought(fish).
shops(mary).
end(model(540)).

begin(model(541)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(541)).

begin(model(542)).
bought(fish).
shops(mary).
end(model(542)).

begin(model(543)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(543)).

begin(model(544)).
bought(spaghetti).
shops(mary).
end(model(544)).

begin(model(545)).
bought(fish).
shops(mary).
end(model(545)).

begin(model(546)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(546)).

begin(model(547)).
bought(fish).
shops(mary).
end(model(547)).

begin(model(548)).
bought(spaghetti).
shops(mary).
end(model(548)).

begin(model(549)).
bought(fish).
shops(mary).
end(model(549)).

begin(model(550)).
bought(fish).
shops(mary).
end(model(550)).

begin(model(551)).
bought(fish).
shops(mary).
end(model(551)).

begin(model(552)).
bought(spaghetti).
shops(mary).
end(model(552)).

begin(model(553)).
bought(spaghetti).
shops(mary).
end(model(553)).

begin(model(554)).
bought(fish).
shops(mary).
end(model(554)).

begin(model(555)).
bought(fish).
shops(mary).
end(model(555)).

begin(model(556)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(556)).

begin(model(557)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(557)).

begin(model(558)).
bought(fish).
shops(mary).
end(model(558)).

begin(model(559)).
bought(spaghetti).
shops(mary).
end(model(559)).

begin(model(560)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(560)).

begin(model(561)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(561)).

begin(model(562)).
bought(spaghetti).
shops(mary).
end(model(562)).

begin(model(563)).
bought(spaghetti).
shops(mary).
end(model(563)).

begin(model(564)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(564)).

begin(model(565)).
bought(fish).
shops(mary).
end(model(565)).

begin(model(566)).
bought(spaghetti).
shops(mary).
end(model(566)).

begin(model(567)).
bought(spaghetti).
shops(mary).
end(model(567)).

begin(model(568)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(568)).

begin(model(569)).
bought(fish).
shops(mary).
end(model(569)).

begin(model(570)).
bought(fish).
shops(mary).
end(model(570)).

begin(model(571)).
bought(fish).
shops(mary).
end(model(571)).

begin(model(572)).
bought(fish).
shops(mary).
end(model(572)).

begin(model(573)).
bought(fish).
shops(mary).
end(model(573)).

begin(model(574)).
bought(fish).
shops(mary).
end(model(574)).

begin(model(575)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(575)).

begin(model(576)).
bought(fish).
shops(mary).
end(model(576)).

begin(model(577)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(577)).

begin(model(578)).
bought(fish).
shops(mary).
end(model(578)).

begin(model(579)).
bought(spaghetti).
shops(mary).
end(model(579)).

begin(model(580)).
bought(fish).
shops(mary).
end(model(580)).

begin(model(581)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(581)).

begin(model(582)).
bought(spaghetti).
shops(mary).
end(model(582)).

begin(model(583)).
bought(fish).
shops(mary).
end(model(583)).

begin(model(584)).
bought(fish).
shops(mary).
end(model(584)).

begin(model(585)).
bought(fish).
shops(mary).
end(model(585)).

begin(model(586)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(586)).

begin(model(587)).
bought(fish).
shops(mary).
end(model(587)).

begin(model(588)).
bought(fish).
shops(mary).
end(model(588)).

begin(model(589)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(589)).

begin(model(590)).
bought(fish).
shops(mary).
end(model(590)).

begin(model(591)).
bought(fish).
shops(mary).
end(model(591)).

begin(model(592)).
bought(spaghetti).
shops(mary).
end(model(592)).

begin(model(593)).
bought(spaghetti).
shops(mary).
end(model(593)).

begin(model(594)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(594)).

begin(model(595)).
bought(fish).
shops(mary).
end(model(595)).

begin(model(596)).
bought(spaghetti).
shops(mary).
end(model(596)).

begin(model(597)).
bought(spaghetti).
shops(mary).
end(model(597)).

begin(model(598)).
end(model(598)).

begin(model(599)).
bought(fish).
shops(mary).
end(model(599)).

begin(model(600)).
bought(fish).
shops(mary).
end(model(600)).

begin(model(601)).
bought(spaghetti).
shops(mary).
end(model(601)).

begin(model(602)).
bought(fish).
shops(mary).
end(model(602)).

begin(model(603)).
bought(fish).
shops(mary).
end(model(603)).

begin(model(604)).
bought(fish).
shops(mary).
end(model(604)).

begin(model(605)).
bought(fish).
shops(mary).
end(model(605)).

begin(model(606)).
end(model(606)).

begin(model(607)).
bought(spaghetti).
shops(mary).
end(model(607)).

begin(model(608)).
bought(spaghetti).
shops(mary).
end(model(608)).

begin(model(609)).
bought(fish).
shops(mary).
end(model(609)).

begin(model(610)).
bought(spaghetti).
shops(john).
end(model(610)).

begin(model(611)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(611)).

begin(model(612)).
bought(spaghetti).
shops(mary).
end(model(612)).

begin(model(613)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(613)).

begin(model(614)).
bought(fish).
shops(mary).
end(model(614)).

begin(model(615)).
bought(fish).
shops(mary).
end(model(615)).

begin(model(616)).
bought(fish).
shops(mary).
end(model(616)).

begin(model(617)).
bought(spaghetti).
shops(mary).
end(model(617)).

begin(model(618)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(618)).

begin(model(619)).
end(model(619)).

begin(model(620)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(620)).

begin(model(621)).
bought(fish).
shops(mary).
end(model(621)).

begin(model(622)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(622)).

begin(model(623)).
bought(spaghetti).
shops(mary).
end(model(623)).

begin(model(624)).
bought(fish).
shops(mary).
end(model(624)).

begin(model(625)).
bought(steak).
shops(john).
end(model(625)).

begin(model(626)).
bought(fish).
shops(mary).
end(model(626)).

begin(model(627)).
bought(fish).
shops(mary).
end(model(627)).

begin(model(628)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(628)).

begin(model(629)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(629)).

begin(model(630)).
bought(spaghetti).
shops(mary).
end(model(630)).

begin(model(631)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(631)).

begin(model(632)).
bought(spaghetti).
shops(mary).
end(model(632)).

begin(model(633)).
bought(spaghetti).
shops(mary).
end(model(633)).

begin(model(634)).
bought(fish).
shops(mary).
end(model(634)).

begin(model(635)).
bought(fish).
shops(mary).
end(model(635)).

begin(model(636)).
end(model(636)).

begin(model(637)).
bought(fish).
shops(mary).
end(model(637)).

begin(model(638)).
bought(fish).
shops(mary).
end(model(638)).

begin(model(639)).
bought(spaghetti).
shops(mary).
end(model(639)).

begin(model(640)).
end(model(640)).

begin(model(641)).
bought(fish).
shops(mary).
end(model(641)).

begin(model(642)).
bought(fish).
shops(mary).
end(model(642)).

begin(model(643)).
bought(fish).
shops(mary).
end(model(643)).

begin(model(644)).
bought(spaghetti).
shops(mary).
end(model(644)).

begin(model(645)).
bought(spaghetti).
shops(mary).
end(model(645)).

begin(model(646)).
bought(spaghetti).
shops(mary).
end(model(646)).

begin(model(647)).
bought(fish).
shops(mary).
end(model(647)).

begin(model(648)).
bought(fish).
shops(mary).
end(model(648)).

begin(model(649)).
bought(fish).
shops(mary).
end(model(649)).

begin(model(650)).
bought(fish).
shops(mary).
end(model(650)).

begin(model(651)).
bought(fish).
shops(mary).
end(model(651)).

begin(model(652)).
bought(fish).
shops(mary).
end(model(652)).

begin(model(653)).
bought(fish).
shops(mary).
end(model(653)).

begin(model(654)).
bought(fish).
shops(mary).
end(model(654)).

begin(model(655)).
bought(spaghetti).
shops(john).
end(model(655)).

begin(model(656)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(656)).

begin(model(657)).
bought(fish).
shops(mary).
end(model(657)).

begin(model(658)).
bought(fish).
shops(mary).
end(model(658)).

begin(model(659)).
bought(spaghetti).
shops(mary).
end(model(659)).

begin(model(660)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(660)).

begin(model(661)).
bought(fish).
shops(mary).
end(model(661)).

begin(model(662)).
bought(fish).
shops(mary).
end(model(662)).

begin(model(663)).
bought(spaghetti).
shops(mary).
end(model(663)).

begin(model(664)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(664)).

begin(model(665)).
bought(fish).
shops(mary).
end(model(665)).

begin(model(666)).
bought(spaghetti).
shops(mary).
end(model(666)).

begin(model(667)).
bought(spaghetti).
shops(mary).
end(model(667)).

begin(model(668)).
bought(fish).
shops(mary).
end(model(668)).

begin(model(669)).
bought(spaghetti).
shops(mary).
end(model(669)).

begin(model(670)).
bought(fish).
shops(mary).
end(model(670)).

begin(model(671)).
bought(fish).
shops(mary).
end(model(671)).

begin(model(672)).
bought(fish).
shops(mary).
end(model(672)).

begin(model(673)).
bought(fish).
shops(mary).
end(model(673)).

begin(model(674)).
bought(fish).
shops(mary).
end(model(674)).

begin(model(675)).
bought(spaghetti).
shops(mary).
end(model(675)).

begin(model(676)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(676)).

begin(model(677)).
bought(spaghetti).
shops(mary).
end(model(677)).

begin(model(678)).
bought(fish).
shops(mary).
end(model(678)).

begin(model(679)).
bought(fish).
shops(mary).
end(model(679)).

begin(model(680)).
bought(spaghetti).
shops(mary).
end(model(680)).

begin(model(681)).
bought(fish).
shops(mary).
end(model(681)).

begin(model(682)).
bought(fish).
shops(mary).
end(model(682)).

begin(model(683)).
bought(fish).
shops(mary).
end(model(683)).

begin(model(684)).
bought(fish).
shops(mary).
end(model(684)).

begin(model(685)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(685)).

begin(model(686)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(686)).

begin(model(687)).
bought(fish).
shops(mary).
end(model(687)).

begin(model(688)).
bought(fish).
shops(mary).
end(model(688)).

begin(model(689)).
bought(fish).
shops(mary).
end(model(689)).

begin(model(690)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(690)).

begin(model(691)).
bought(fish).
shops(mary).
end(model(691)).

begin(model(692)).
bought(fish).
shops(mary).
end(model(692)).

begin(model(693)).
bought(spaghetti).
shops(mary).
end(model(693)).

begin(model(694)).
bought(spaghetti).
shops(mary).
end(model(694)).

begin(model(695)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(695)).

begin(model(696)).
bought(fish).
shops(mary).
end(model(696)).

begin(model(697)).
bought(spaghetti).
shops(mary).
end(model(697)).

begin(model(698)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(698)).

begin(model(699)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(699)).

begin(model(700)).
bought(fish).
shops(mary).
end(model(700)).

begin(model(701)).
bought(fish).
shops(mary).
end(model(701)).

begin(model(702)).
bought(fish).
shops(mary).
end(model(702)).

begin(model(703)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(703)).

begin(model(704)).
bought(fish).
shops(mary).
end(model(704)).

begin(model(705)).
bought(spaghetti).
shops(mary).
end(model(705)).

begin(model(706)).
bought(spaghetti).
shops(mary).
end(model(706)).

begin(model(707)).
bought(fish).
shops(mary).
end(model(707)).

begin(model(708)).
bought(spaghetti).
shops(mary).
end(model(708)).

begin(model(709)).
bought(fish).
shops(mary).
end(model(709)).

begin(model(710)).
bought(fish).
shops(mary).
end(model(710)).

begin(model(711)).
end(model(711)).

begin(model(712)).
bought(fish).
shops(mary).
end(model(712)).

begin(model(713)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(713)).

begin(model(714)).
bought(fish).
shops(mary).
end(model(714)).

begin(model(715)).
bought(spaghetti).
shops(mary).
end(model(715)).

begin(model(716)).
bought(fish).
shops(mary).
end(model(716)).

begin(model(717)).
bought(spaghetti).
shops(john).
end(model(717)).

begin(model(718)).
bought(fish).
shops(mary).
end(model(718)).

begin(model(719)).
bought(fish).
shops(mary).
end(model(719)).

begin(model(720)).
bought(fish).
shops(mary).
end(model(720)).

begin(model(721)).
bought(fish).
shops(mary).
end(model(721)).

begin(model(722)).
bought(spaghetti).
shops(mary).
end(model(722)).

begin(model(723)).
bought(fish).
shops(mary).
end(model(723)).

begin(model(724)).
bought(spaghetti).
shops(mary).
end(model(724)).

begin(model(725)).
bought(fish).
shops(mary).
end(model(725)).

begin(model(726)).
bought(fish).
shops(mary).
end(model(726)).

begin(model(727)).
bought(spaghetti).
shops(mary).
end(model(727)).

begin(model(728)).
bought(fish).
shops(mary).
end(model(728)).

begin(model(729)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(729)).

begin(model(730)).
bought(spaghetti).
shops(mary).
end(model(730)).

begin(model(731)).
bought(spaghetti).
shops(mary).
end(model(731)).

begin(model(732)).
bought(fish).
shops(mary).
end(model(732)).

begin(model(733)).
bought(fish).
shops(mary).
end(model(733)).

begin(model(734)).
bought(fish).
shops(mary).
end(model(734)).

begin(model(735)).
bought(spaghetti).
shops(mary).
end(model(735)).

begin(model(736)).
bought(fish).
shops(mary).
end(model(736)).

begin(model(737)).
bought(fish).
shops(mary).
end(model(737)).

begin(model(738)).
bought(spaghetti).
shops(mary).
end(model(738)).

begin(model(739)).
bought(spaghetti).
shops(mary).
end(model(739)).

begin(model(740)).
bought(fish).
shops(mary).
end(model(740)).

begin(model(741)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(741)).

begin(model(742)).
bought(fish).
shops(mary).
end(model(742)).

begin(model(743)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(743)).

begin(model(744)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(744)).

begin(model(745)).
bought(spaghetti).
shops(mary).
end(model(745)).

begin(model(746)).
bought(spaghetti).
shops(mary).
end(model(746)).

begin(model(747)).
bought(fish).
shops(mary).
end(model(747)).

begin(model(748)).
bought(spaghetti).
shops(mary).
end(model(748)).

begin(model(749)).
bought(fish).
shops(mary).
end(model(749)).

begin(model(750)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(750)).

begin(model(751)).
bought(spaghetti).
shops(mary).
end(model(751)).

begin(model(752)).
bought(spaghetti).
shops(mary).
end(model(752)).

begin(model(753)).
bought(fish).
shops(mary).
end(model(753)).

begin(model(754)).
bought(fish).
shops(mary).
end(model(754)).

begin(model(755)).
bought(fish).
shops(mary).
end(model(755)).

begin(model(756)).
bought(fish).
shops(mary).
end(model(756)).

begin(model(757)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(757)).

begin(model(758)).
bought(fish).
shops(mary).
end(model(758)).

begin(model(759)).
bought(fish).
shops(mary).
end(model(759)).

begin(model(760)).
bought(fish).
shops(mary).
end(model(760)).

begin(model(761)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(761)).

begin(model(762)).
bought(spaghetti).
shops(mary).
end(model(762)).

begin(model(763)).
bought(fish).
shops(mary).
end(model(763)).

begin(model(764)).
bought(fish).
shops(mary).
end(model(764)).

begin(model(765)).
bought(fish).
shops(mary).
end(model(765)).

begin(model(766)).
bought(fish).
shops(mary).
end(model(766)).

begin(model(767)).
bought(fish).
shops(mary).
end(model(767)).

begin(model(768)).
bought(spaghetti).
shops(mary).
end(model(768)).

begin(model(769)).
bought(fish).
shops(mary).
end(model(769)).

begin(model(770)).
bought(spaghetti).
shops(mary).
end(model(770)).

begin(model(771)).
bought(fish).
shops(mary).
end(model(771)).

begin(model(772)).
bought(fish).
shops(mary).
end(model(772)).

begin(model(773)).
bought(spaghetti).
shops(mary).
end(model(773)).

begin(model(774)).
bought(fish).
shops(mary).
end(model(774)).

begin(model(775)).
bought(fish).
shops(mary).
end(model(775)).

begin(model(776)).
bought(spaghetti).
shops(mary).
end(model(776)).

begin(model(777)).
bought(spaghetti).
shops(mary).
end(model(777)).

begin(model(778)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(778)).

begin(model(779)).
bought(spaghetti).
shops(john).
end(model(779)).

begin(model(780)).
bought(fish).
shops(mary).
end(model(780)).

begin(model(781)).
bought(spaghetti).
shops(mary).
end(model(781)).

begin(model(782)).
bought(fish).
shops(mary).
end(model(782)).

begin(model(783)).
bought(fish).
shops(mary).
end(model(783)).

begin(model(784)).
bought(fish).
shops(mary).
end(model(784)).

begin(model(785)).
bought(spaghetti).
shops(mary).
end(model(785)).

begin(model(786)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(786)).

begin(model(787)).
bought(fish).
shops(mary).
end(model(787)).

begin(model(788)).
bought(spaghetti).
shops(mary).
end(model(788)).

begin(model(789)).
end(model(789)).

begin(model(790)).
bought(fish).
shops(mary).
end(model(790)).

begin(model(791)).
bought(fish).
shops(mary).
end(model(791)).

begin(model(792)).
bought(fish).
shops(mary).
end(model(792)).

begin(model(793)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(793)).

begin(model(794)).
bought(spaghetti).
shops(mary).
end(model(794)).

begin(model(795)).
bought(fish).
shops(mary).
end(model(795)).

begin(model(796)).
bought(fish).
shops(mary).
end(model(796)).

begin(model(797)).
bought(fish).
shops(mary).
end(model(797)).

begin(model(798)).
bought(spaghetti).
shops(mary).
end(model(798)).

begin(model(799)).
bought(fish).
shops(mary).
end(model(799)).

begin(model(800)).
bought(fish).
shops(mary).
end(model(800)).

begin(model(801)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(801)).

begin(model(802)).
bought(spaghetti).
shops(mary).
end(model(802)).

begin(model(803)).
bought(fish).
shops(mary).
end(model(803)).

begin(model(804)).
bought(spaghetti).
shops(mary).
end(model(804)).

begin(model(805)).
bought(spaghetti).
shops(mary).
end(model(805)).

begin(model(806)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(806)).

begin(model(807)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(807)).

begin(model(808)).
bought(fish).
shops(mary).
end(model(808)).

begin(model(809)).
bought(fish).
shops(mary).
end(model(809)).

begin(model(810)).
bought(fish).
shops(mary).
end(model(810)).

begin(model(811)).
bought(fish).
shops(mary).
end(model(811)).

begin(model(812)).
bought(fish).
shops(mary).
end(model(812)).

begin(model(813)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(813)).

begin(model(814)).
bought(fish).
shops(mary).
end(model(814)).

begin(model(815)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(815)).

begin(model(816)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(816)).

begin(model(817)).
bought(fish).
shops(mary).
end(model(817)).

begin(model(818)).
bought(spaghetti).
shops(mary).
end(model(818)).

begin(model(819)).
bought(spaghetti).
shops(mary).
end(model(819)).

begin(model(820)).
bought(fish).
shops(mary).
end(model(820)).

begin(model(821)).
bought(spaghetti).
shops(john).
end(model(821)).

begin(model(822)).
bought(fish).
shops(mary).
end(model(822)).

begin(model(823)).
bought(fish).
shops(mary).
end(model(823)).

begin(model(824)).
bought(fish).
shops(mary).
end(model(824)).

begin(model(825)).
bought(fish).
shops(mary).
end(model(825)).

begin(model(826)).
bought(fish).
shops(mary).
end(model(826)).

begin(model(827)).
bought(fish).
shops(mary).
end(model(827)).

begin(model(828)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(828)).

begin(model(829)).
bought(fish).
shops(mary).
end(model(829)).

begin(model(830)).
bought(fish).
shops(mary).
end(model(830)).

begin(model(831)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(831)).

begin(model(832)).
bought(spaghetti).
shops(mary).
end(model(832)).

begin(model(833)).
bought(fish).
shops(mary).
end(model(833)).

begin(model(834)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(834)).

begin(model(835)).
bought(fish).
shops(mary).
end(model(835)).

begin(model(836)).
bought(fish).
shops(mary).
end(model(836)).

begin(model(837)).
bought(spaghetti).
shops(mary).
end(model(837)).

begin(model(838)).
bought(fish).
shops(mary).
end(model(838)).

begin(model(839)).
bought(fish).
shops(mary).
end(model(839)).

begin(model(840)).
bought(fish).
shops(mary).
end(model(840)).

begin(model(841)).
bought(fish).
shops(mary).
end(model(841)).

begin(model(842)).
bought(fish).
shops(mary).
end(model(842)).

begin(model(843)).
bought(fish).
shops(mary).
end(model(843)).

begin(model(844)).
bought(fish).
shops(mary).
end(model(844)).

begin(model(845)).
bought(fish).
shops(mary).
end(model(845)).

begin(model(846)).
bought(fish).
shops(mary).
end(model(846)).

begin(model(847)).
bought(spaghetti).
shops(mary).
end(model(847)).

begin(model(848)).
bought(spaghetti).
shops(mary).
end(model(848)).

begin(model(849)).
bought(fish).
shops(mary).
end(model(849)).

begin(model(850)).
bought(spaghetti).
shops(mary).
end(model(850)).

begin(model(851)).
bought(fish).
shops(mary).
end(model(851)).

begin(model(852)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(852)).

begin(model(853)).
bought(fish).
shops(mary).
end(model(853)).

begin(model(854)).
bought(spaghetti).
shops(mary).
end(model(854)).

begin(model(855)).
bought(fish).
shops(mary).
end(model(855)).

begin(model(856)).
bought(fish).
shops(mary).
end(model(856)).

begin(model(857)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(857)).

begin(model(858)).
bought(fish).
shops(mary).
end(model(858)).

begin(model(859)).
bought(spaghetti).
shops(mary).
end(model(859)).

begin(model(860)).
bought(spaghetti).
shops(mary).
end(model(860)).

begin(model(861)).
bought(fish).
shops(mary).
end(model(861)).

begin(model(862)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(862)).

begin(model(863)).
bought(fish).
shops(mary).
end(model(863)).

begin(model(864)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(864)).

begin(model(865)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(865)).

begin(model(866)).
bought(spaghetti).
shops(mary).
end(model(866)).

begin(model(867)).
bought(fish).
shops(mary).
end(model(867)).

begin(model(868)).
bought(fish).
shops(mary).
end(model(868)).

begin(model(869)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(869)).

begin(model(870)).
bought(fish).
shops(mary).
end(model(870)).

begin(model(871)).
bought(spaghetti).
shops(mary).
end(model(871)).

begin(model(872)).
bought(fish).
shops(mary).
end(model(872)).

begin(model(873)).
bought(spaghetti).
shops(mary).
end(model(873)).

begin(model(874)).
bought(spaghetti).
shops(mary).
end(model(874)).

begin(model(875)).
bought(spaghetti).
shops(mary).
end(model(875)).

begin(model(876)).
bought(fish).
shops(mary).
end(model(876)).

begin(model(877)).
bought(steak).
shops(john).
end(model(877)).

begin(model(878)).
bought(steak).
shops(john).
end(model(878)).

begin(model(879)).
bought(fish).
shops(mary).
end(model(879)).

begin(model(880)).
bought(spaghetti).
shops(mary).
end(model(880)).

begin(model(881)).
bought(spaghetti).
shops(john).
end(model(881)).

begin(model(882)).
bought(fish).
shops(mary).
end(model(882)).

begin(model(883)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(883)).

begin(model(884)).
bought(fish).
shops(mary).
end(model(884)).

begin(model(885)).
bought(fish).
shops(mary).
end(model(885)).

begin(model(886)).
bought(fish).
shops(mary).
end(model(886)).

begin(model(887)).
bought(fish).
shops(mary).
end(model(887)).

begin(model(888)).
bought(spaghetti).
shops(mary).
end(model(888)).

begin(model(889)).
bought(fish).
shops(mary).
end(model(889)).

begin(model(890)).
bought(fish).
shops(mary).
end(model(890)).

begin(model(891)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(891)).

begin(model(892)).
bought(spaghetti).
shops(mary).
end(model(892)).

begin(model(893)).
bought(fish).
shops(mary).
end(model(893)).

begin(model(894)).
bought(fish).
shops(mary).
end(model(894)).

begin(model(895)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(895)).

begin(model(896)).
bought(spaghetti).
shops(mary).
end(model(896)).

begin(model(897)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(897)).

begin(model(898)).
bought(fish).
shops(mary).
end(model(898)).

begin(model(899)).
bought(fish).
shops(mary).
end(model(899)).

begin(model(900)).
bought(fish).
shops(mary).
end(model(900)).

begin(model(901)).
bought(fish).
shops(mary).
end(model(901)).

begin(model(902)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(902)).

begin(model(903)).
bought(steak).
shops(john).
end(model(903)).

begin(model(904)).
bought(fish).
shops(mary).
end(model(904)).

begin(model(905)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(905)).

begin(model(906)).
bought(spaghetti).
shops(mary).
end(model(906)).

begin(model(907)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(907)).

begin(model(908)).
bought(fish).
shops(mary).
end(model(908)).

begin(model(909)).
bought(fish).
shops(mary).
end(model(909)).

begin(model(910)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(910)).

begin(model(911)).
bought(fish).
shops(mary).
end(model(911)).

begin(model(912)).
bought(fish).
shops(mary).
end(model(912)).

begin(model(913)).
bought(fish).
shops(mary).
end(model(913)).

begin(model(914)).
bought(fish).
shops(mary).
end(model(914)).

begin(model(915)).
bought(fish).
shops(mary).
end(model(915)).

begin(model(916)).
bought(fish).
shops(mary).
end(model(916)).

begin(model(917)).
bought(spaghetti).
shops(mary).
end(model(917)).

begin(model(918)).
end(model(918)).

begin(model(919)).
bought(fish).
shops(mary).
end(model(919)).

begin(model(920)).
bought(fish).
shops(mary).
end(model(920)).

begin(model(921)).
bought(fish).
shops(mary).
end(model(921)).

begin(model(922)).
bought(fish).
shops(mary).
end(model(922)).

begin(model(923)).
bought(steak).
shops(john).
end(model(923)).

begin(model(924)).
bought(fish).
shops(mary).
end(model(924)).

begin(model(925)).
bought(spaghetti).
shops(mary).
end(model(925)).

begin(model(926)).
bought(fish).
shops(mary).
end(model(926)).

begin(model(927)).
bought(fish).
shops(mary).
end(model(927)).

begin(model(928)).
bought(spaghetti).
shops(mary).
end(model(928)).

begin(model(929)).
bought(fish).
shops(mary).
end(model(929)).

begin(model(930)).
end(model(930)).

begin(model(931)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(931)).

begin(model(932)).
bought(fish).
shops(mary).
end(model(932)).

begin(model(933)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(933)).

begin(model(934)).
bought(fish).
shops(mary).
end(model(934)).

begin(model(935)).
bought(fish).
shops(mary).
end(model(935)).

begin(model(936)).
bought(fish).
shops(mary).
end(model(936)).

begin(model(937)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(937)).

begin(model(938)).
end(model(938)).

begin(model(939)).
bought(spaghetti).
shops(mary).
end(model(939)).

begin(model(940)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(940)).

begin(model(941)).
bought(fish).
shops(mary).
end(model(941)).

begin(model(942)).
bought(fish).
shops(mary).
end(model(942)).

begin(model(943)).
bought(spaghetti).
shops(mary).
end(model(943)).

begin(model(944)).
bought(spaghetti).
shops(mary).
end(model(944)).

begin(model(945)).
bought(spaghetti).
shops(mary).
end(model(945)).

begin(model(946)).
bought(spaghetti).
shops(mary).
end(model(946)).

begin(model(947)).
bought(fish).
shops(mary).
end(model(947)).

begin(model(948)).
bought(spaghetti).
shops(mary).
end(model(948)).

begin(model(949)).
bought(fish).
shops(mary).
end(model(949)).

begin(model(950)).
bought(spaghetti).
shops(mary).
end(model(950)).

begin(model(951)).
bought(spaghetti).
shops(mary).
end(model(951)).

begin(model(952)).
bought(fish).
shops(mary).
end(model(952)).

begin(model(953)).
bought(fish).
shops(mary).
end(model(953)).

begin(model(954)).
bought(fish).
shops(mary).
end(model(954)).

begin(model(955)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(955)).

begin(model(956)).
bought(fish).
shops(mary).
end(model(956)).

begin(model(957)).
bought(fish).
shops(mary).
end(model(957)).

begin(model(958)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(958)).

begin(model(959)).
bought(fish).
shops(mary).
end(model(959)).

begin(model(960)).
bought(fish).
shops(mary).
end(model(960)).

begin(model(961)).
bought(fish).
shops(mary).
end(model(961)).

begin(model(962)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(962)).

begin(model(963)).
bought(fish).
shops(mary).
end(model(963)).

begin(model(964)).
bought(fish).
shops(mary).
end(model(964)).

begin(model(965)).
bought(fish).
shops(mary).
end(model(965)).

begin(model(966)).
bought(fish).
shops(mary).
end(model(966)).

begin(model(967)).
bought(spaghetti).
shops(mary).
end(model(967)).

begin(model(968)).
bought(fish).
shops(mary).
end(model(968)).

begin(model(969)).
bought(fish).
shops(mary).
end(model(969)).

begin(model(970)).
bought(spaghetti).
shops(mary).
end(model(970)).

begin(model(971)).
bought(spaghetti).
shops(mary).
end(model(971)).

begin(model(972)).
bought(spaghetti).
shops(mary).
end(model(972)).

begin(model(973)).
bought(spaghetti).
shops(mary).
end(model(973)).

begin(model(974)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(974)).

begin(model(975)).
bought(fish).
shops(mary).
end(model(975)).

begin(model(976)).
bought(fish).
shops(mary).
end(model(976)).

begin(model(977)).
bought(fish).
shops(mary).
end(model(977)).

begin(model(978)).
end(model(978)).

begin(model(979)).
bought(fish).
shops(mary).
end(model(979)).

begin(model(980)).
bought(fish).
shops(mary).
end(model(980)).

begin(model(981)).
bought(spaghetti).
shops(mary).
end(model(981)).

begin(model(982)).
bought(fish).
shops(mary).
end(model(982)).

begin(model(983)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end(model(983)).

begin(model(984)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(984)).

begin(model(985)).
bought(spaghetti).
shops(mary).
end(model(985)).

begin(model(986)).
bought(fish).
shops(mary).
end(model(986)).

begin(model(987)).
bought(fish).
shops(mary).
end(model(987)).

begin(model(988)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end(model(988)).

begin(model(989)).
bought(spaghetti).
shops(john).
end(model(989)).

begin(model(990)).
bought(fish).
shops(mary).
end(model(990)).

begin(model(991)).
bought(fish).
shops(mary).
end(model(991)).

begin(model(992)).
bought(spaghetti).
shops(mary).
end(model(992)).

begin(model(993)).
bought(fish).
shops(mary).
end(model(993)).

begin(model(994)).
bought(spaghetti).
shops(mary).
end(model(994)).

begin(model(995)).
bought(fish).
shops(mary).
end(model(995)).

begin(model(996)).
bought(fish).
shops(mary).
end(model(996)).

begin(model(997)).
bought(fish).
shops(mary).
end(model(997)).

begin(model(998)).
bought(fish).
shops(mary).
end(model(998)).

begin(model(999)).
bought(fish).
shops(mary).
end(model(999)).

begin(model(1000)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end(model(1000)).

