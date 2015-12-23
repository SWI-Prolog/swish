:-use_module(library(slipcover)).

/** <examples>
?- induce([train],[test],CLL,AUCROC,ROC,AUCPR,PR).
?- induce_par([all],P).
?- induce([all],P).
*/
:- use_rendering(c3).
:- use_rendering(lpad).
:- dynamic modeh/2,modeh/4,fixed_rule/3,banned/2,lookahead/2,
  lookahead_cons/2,lookahead_cons_var/2,prob/2,input/1,input_cw/1,
  ref_clause/1,ref/1.

:-sc.

:- set_sc(megaex_bottom,20).
:- set_sc(max_iter,2).
:- set_sc(max_iter_structure,5).
:- set_sc(verbosity,2).
bg([]).

in([
(
 pos:0.197575 :-
 	circle(A),
 	in(B,A)
),
( 
 pos:0.000303421 :-
 	circle(A),
 	triangle(B)
), 
( pos:0.000448807 :-
 	triangle(A),
 	circle(B)
)]).

fold(train,[2,3,5,6,9,12,14,15,17,20,24,25,28,29,31,36,37,40,41,50,52,55,56,57,
  59,62,63,65,66,67,69,74,76,77,79,83,93,95,99,101,103,104,105,106,107,109,110,
  111,112,117,120,121,125,126,127,128,131,135,137,140,143,144,151,154,155,156,
  159,167,168,169,172,175,176,177,178,181,184,188,190,192,193,194,196,198,202,
  206,208,209,211,214,219,222,223,224,225,227,230,231,233,238,241,243,244,248,
  249,250,256,258,260,268,270,273,280,282,286,287,288,289,290,295,300,301,303,
  304,307,309,314,316,319,321,324,326,327,328,329,331,334,337,343,345,348,352,
  353,355,358,366,369,370,373,375,376,378,379,381,382,390,393,402,404,408,411,
  412,416,417,419,420,421,424,425,427,428,431,432,433,437,444,445,447,453,456,
  457,459,462,463,464,465,468,470,473,474,476,477,479,481,482,483,485,488,489]).

fold(test,
  [490,491,494,497,499,500,512,513,516,517,520,521,527,529,531,533,534,539,540,
  542,543,544,546,550,552,553,555,559,565,567,568,572,578,582,583,591,595,597,
  600,602,609,612,614,615,616,617,622,625,628,634,635,637,639,640,641,642,647,
  648,649,650,654,656,657,658,662,667,669,671,679,682,683,685,686,691,693,698,
  700,701,705,708,709,710,719,722,723,725,728,732,734,737,740,741,743,744,745,
  747,748,750,751,753,754,755,765,766,768,769,772,773,777,779,780,781,787,794,
  795,797,803,805,807,814,815,816,818,819,822,829,832,833,835,836,837,838,841,
  844,845,846,847,848,849,856,859,862,864,867,872,874,876,880,882,887,890,892,
  893,897,899,900,904,909,910,912,915,917,926,927,929,930,931,932,933,938,939,
  940,941,944,945,946,947,955,957,961,971,973,974,975,977,978,979,984,989,991,
  995,997,1000]).


fold(all,[2,3,5,6,9,12,14,15,17,20,24,25,28,29,31,36,37,40,41,50,52,55,56,57,
  59,62,63,65,66,67,69,74,76,77,79,83,93,95,99,101,103,104,105,106,107,109,110,
  111,112,117,120,121,125,126,127,128,131,135,137,140,143,144,151,154,155,156,
  159,167,168,169,172,175,176,177,178,181,184,188,190,192,193,194,196,198,202,
  206,208,209,211,214,219,222,223,224,225,227,230,231,233,238,241,243,244,248,
  249,250,256,258,260,268,270,273,280,282,286,287,288,289,290,295,300,301,303,
  304,307,309,314,316,319,321,324,326,327,328,329,331,334,337,343,345,348,352,
  353,355,358,366,369,370,373,375,376,378,379,381,382,390,393,402,404,408,411,
  412,416,417,419,420,421,424,425,427,428,431,432,433,437,444,445,447,453,456,
  457,459,462,463,464,465,468,470,473,474,476,477,479,481,482,483,485,488,489,
  490,491,494,497,499,500,512,513,516,517,520,521,527,529,531,533,534,539,540,
  542,543,544,546,550,552,553,555,559,565,567,568,572,578,582,583,591,595,597,
  600,602,609,612,614,615,616,617,622,625,628,634,635,637,639,640,641,642,647,
  648,649,650,654,656,657,658,662,667,669,671,679,682,683,685,686,691,693,698,
  700,701,705,708,709,710,719,722,723,725,728,732,734,737,740,741,743,744,745,
  747,748,750,751,753,754,755,765,766,768,769,772,773,777,779,780,781,787,794,
  795,797,803,805,807,814,815,816,818,819,822,829,832,833,835,836,837,838,841,
  844,845,846,847,848,849,856,859,862,864,867,872,874,876,880,882,887,890,892,
  893,897,899,900,904,909,910,912,915,917,926,927,929,930,931,932,933,938,939,
  940,941,944,945,946,947,955,957,961,971,973,974,975,977,978,979,984,989,991,
  995,997,1000]).

output(pos/0).

input_cw(triangle/1).
input_cw(square/1).
input_cw(circle/1).
input_cw(in/2).
input_cw(config/2).

determination(pos/0,triangle/1).
determination(pos/0,square/1).
determination(pos/0,circle/1).
determination(pos/0,in/2).
determination(pos/0,config/2).

modeh(*,pos).
modeb(*,triangle(-obj)).
modeb(*,square(-obj)).
modeb(*,circle(-obj)).
modeb(*,in(+obj,-obj)).
modeb(*,in(-obj,+obj)).
modeb(*,config(+obj,-#dir)).

% database
pos(2).
triangle(2,o5).
config(2,o5,up).
square(2,o4).
in(2,o4,o5).
circle(2,o3).
triangle(2,o2).
config(2,o2,up).
in(2,o2,o3).
triangle(2,o1).
config(2,o1,up).

neg(pos(3)).
circle(3,o4).
circle(3,o3).
in(3,o3,o4).
square(3,o2).
circle(3,o1).
in(3,o1,o2).

neg(pos(5)).
square(5,o3).
square(5,o2).
in(5,o2,o3).
square(5,o1).

pos(6).
triangle(6,o5).
config(6,o5,down).
triangle(6,o4).
config(6,o4,up).
in(6,o4,o5).
circle(6,o3).
square(6,o2).
in(6,o2,o3).
triangle(6,o1).
config(6,o1,up).

pos(9).
circle(9,o4).
triangle(9,o3).
config(9,o3,up).
in(9,o3,o4).
triangle(9,o2).
config(9,o2,down).
square(9,o1).
in(9,o1,o2).

neg(pos(12)).
triangle(12,o5).
config(12,o5,down).
square(12,o4).
in(12,o4,o5).
circle(12,o3).
circle(12,o2).
in(12,o2,o3).
triangle(12,o1).
config(12,o1,down).

neg(pos(14)).
triangle(14,o3).
config(14,o3,down).
circle(14,o2).
in(14,o2,o3).
triangle(14,o1).
config(14,o1,down).

neg(pos(15)).
triangle(15,o4).
config(15,o4,down).
circle(15,o3).
in(15,o3,o4).
triangle(15,o2).
config(15,o2,up).
circle(15,o1).
in(15,o1,o2).

pos(17).
triangle(17,o2).
config(17,o2,down).
triangle(17,o1).
config(17,o1,down).
in(17,o1,o2).

pos(20).
triangle(20,o6).
config(20,o6,up).
triangle(20,o5).
config(20,o5,up).
in(20,o5,o6).
square(20,o4).
triangle(20,o3).
config(20,o3,up).
in(20,o3,o4).
square(20,o2).
triangle(20,o1).
config(20,o1,up).
in(20,o1,o2).

pos(24).
triangle(24,o4).
config(24,o4,up).
circle(24,o3).
in(24,o3,o4).
triangle(24,o2).
config(24,o2,up).
triangle(24,o1).
config(24,o1,up).
in(24,o1,o2).

neg(pos(25)).
square(25,o2).
square(25,o1).
in(25,o1,o2).

neg(pos(28)).
square(28,o5).
square(28,o4).
in(28,o4,o5).
circle(28,o3).
circle(28,o2).
in(28,o2,o3).
triangle(28,o1).
config(28,o1,up).

neg(pos(29)).
triangle(29,o2).
config(29,o2,up).
square(29,o1).
in(29,o1,o2).

neg(pos(31)).
circle(31,o4).
square(31,o3).
in(31,o3,o4).
square(31,o2).
circle(31,o1).
in(31,o1,o2).

neg(pos(36)).
triangle(36,o3).
config(36,o3,up).
circle(36,o2).
in(36,o2,o3).
circle(36,o1).

neg(pos(37)).
square(37,o6).
circle(37,o5).
in(37,o5,o6).
square(37,o4).
circle(37,o3).
in(37,o3,o4).
square(37,o2).
triangle(37,o1).
config(37,o1,down).
in(37,o1,o2).

neg(pos(40)).
circle(40,o4).
square(40,o3).
in(40,o3,o4).
square(40,o2).
circle(40,o1).
in(40,o1,o2).

neg(pos(41)).
square(41,o3).
circle(41,o2).
in(41,o2,o3).
triangle(41,o1).
config(41,o1,down).

pos(50).
triangle(50,o6).
config(50,o6,up).
square(50,o5).
in(50,o5,o6).
triangle(50,o4).
config(50,o4,down).
circle(50,o3).
in(50,o3,o4).
triangle(50,o2).
config(50,o2,up).
triangle(50,o1).
config(50,o1,down).
in(50,o1,o2).

neg(pos(52)).
square(52,o3).
circle(52,o2).
in(52,o2,o3).
circle(52,o1).

neg(pos(55)).
triangle(55,o2).
config(55,o2,down).
square(55,o1).
in(55,o1,o2).

neg(pos(56)).
circle(56,o5).
circle(56,o4).
in(56,o4,o5).
circle(56,o3).
circle(56,o2).
in(56,o2,o3).
circle(56,o1).

neg(pos(57)).
circle(57,o4).
circle(57,o3).
in(57,o3,o4).
square(57,o2).
triangle(57,o1).
config(57,o1,up).
in(57,o1,o2).

neg(pos(59)).
square(59,o5).
triangle(59,o4).
config(59,o4,up).
in(59,o4,o5).
triangle(59,o3).
config(59,o3,down).
circle(59,o2).
in(59,o2,o3).
circle(59,o1).

pos(62).
triangle(62,o5).
config(62,o5,up).
triangle(62,o4).
config(62,o4,down).
in(62,o4,o5).
triangle(62,o3).
config(62,o3,up).
square(62,o2).
in(62,o2,o3).
triangle(62,o1).
config(62,o1,down).

neg(pos(63)).
triangle(63,o2).
config(63,o2,up).
square(63,o1).
in(63,o1,o2).

neg(pos(65)).
circle(65,o6).
circle(65,o5).
in(65,o5,o6).
circle(65,o4).
circle(65,o3).
in(65,o3,o4).
square(65,o2).
square(65,o1).
in(65,o1,o2).

neg(pos(66)).
triangle(66,o2).
config(66,o2,up).
circle(66,o1).
in(66,o1,o2).

neg(pos(67)).
circle(67,o4).
circle(67,o3).
in(67,o3,o4).
square(67,o2).
triangle(67,o1).
config(67,o1,down).
in(67,o1,o2).

pos(69).
circle(69,o3).
triangle(69,o2).
config(69,o2,up).
in(69,o2,o3).
triangle(69,o1).
config(69,o1,down).

pos(74).
circle(74,o6).
triangle(74,o5).
config(74,o5,up).
in(74,o5,o6).
triangle(74,o4).
config(74,o4,down).
square(74,o3).
in(74,o3,o4).
triangle(74,o2).
config(74,o2,down).
triangle(74,o1).
config(74,o1,down).
in(74,o1,o2).

pos(76).
square(76,o6).
square(76,o5).
in(76,o5,o6).
triangle(76,o4).
config(76,o4,up).
square(76,o3).
in(76,o3,o4).
triangle(76,o2).
config(76,o2,up).
triangle(76,o1).
config(76,o1,down).
in(76,o1,o2).

neg(pos(77)).
square(77,o3).
triangle(77,o2).
config(77,o2,up).
in(77,o2,o3).
square(77,o1).

pos(79).
triangle(79,o5).
config(79,o5,up).
triangle(79,o4).
config(79,o4,down).
in(79,o4,o5).
triangle(79,o3).
config(79,o3,down).
triangle(79,o2).
config(79,o2,up).
in(79,o2,o3).
square(79,o1).

pos(83).
triangle(83,o3).
config(83,o3,up).
triangle(83,o2).
config(83,o2,down).
in(83,o2,o3).
square(83,o1).

neg(pos(93)).
square(93,o4).
triangle(93,o3).
config(93,o3,up).
in(93,o3,o4).
circle(93,o2).
circle(93,o1).
in(93,o1,o2).

pos(95).
circle(95,o6).
square(95,o5).
in(95,o5,o6).
square(95,o4).
triangle(95,o3).
config(95,o3,up).
in(95,o3,o4).
square(95,o2).
triangle(95,o1).
config(95,o1,down).
in(95,o1,o2).

neg(pos(99)).
triangle(99,o4).
config(99,o4,down).
square(99,o3).
in(99,o3,o4).
square(99,o2).
circle(99,o1).
in(99,o1,o2).

neg(pos(101)).
square(101,o6).
square(101,o5).
in(101,o5,o6).
triangle(101,o4).
config(101,o4,down).
circle(101,o3).
in(101,o3,o4).
triangle(101,o2).
config(101,o2,down).
circle(101,o1).
in(101,o1,o2).

pos(103).
circle(103,o4).
square(103,o3).
in(103,o3,o4).
square(103,o2).
triangle(103,o1).
config(103,o1,up).
in(103,o1,o2).

pos(104).
circle(104,o5).
triangle(104,o4).
config(104,o4,up).
in(104,o4,o5).
circle(104,o3).
square(104,o2).
in(104,o2,o3).
circle(104,o1).

neg(pos(105)).
triangle(105,o2).
config(105,o2,up).
square(105,o1).
in(105,o1,o2).

pos(106).
triangle(106,o5).
config(106,o5,up).
triangle(106,o4).
config(106,o4,up).
in(106,o4,o5).
circle(106,o3).
circle(106,o2).
in(106,o2,o3).
square(106,o1).

pos(107).
square(107,o4).
triangle(107,o3).
config(107,o3,up).
in(107,o3,o4).
circle(107,o2).
square(107,o1).
in(107,o1,o2).

neg(pos(109)).
square(109,o4).
circle(109,o3).
in(109,o3,o4).
square(109,o2).
triangle(109,o1).
config(109,o1,up).
in(109,o1,o2).

neg(pos(110)).
triangle(110,o3).
config(110,o3,up).
circle(110,o2).
in(110,o2,o3).
triangle(110,o1).
config(110,o1,up).

neg(pos(111)).
square(111,o6).
circle(111,o5).
in(111,o5,o6).
square(111,o4).
square(111,o3).
in(111,o3,o4).
triangle(111,o2).
config(111,o2,up).
square(111,o1).
in(111,o1,o2).

neg(pos(112)).
square(112,o4).
circle(112,o3).
in(112,o3,o4).
square(112,o2).
circle(112,o1).
in(112,o1,o2).

neg(pos(117)).
square(117,o2).
square(117,o1).
in(117,o1,o2).

pos(120).
circle(120,o3).
square(120,o2).
in(120,o2,o3).
triangle(120,o1).
config(120,o1,down).

neg(pos(121)).
triangle(121,o3).
config(121,o3,down).
square(121,o2).
in(121,o2,o3).
triangle(121,o1).
config(121,o1,down).

neg(pos(125)).
circle(125,o4).
circle(125,o3).
in(125,o3,o4).
square(125,o2).
square(125,o1).
in(125,o1,o2).

neg(pos(126)).
circle(126,o3).
circle(126,o2).
in(126,o2,o3).
square(126,o1).

neg(pos(127)).
triangle(127,o3).
config(127,o3,up).
square(127,o2).
in(127,o2,o3).
square(127,o1).

neg(pos(128)).
circle(128,o3).
circle(128,o2).
in(128,o2,o3).
square(128,o1).

neg(pos(131)).
circle(131,o2).
circle(131,o1).
in(131,o1,o2).

pos(135).
triangle(135,o5).
config(135,o5,down).
triangle(135,o4).
config(135,o4,up).
in(135,o4,o5).
triangle(135,o3).
config(135,o3,down).
square(135,o2).
in(135,o2,o3).
square(135,o1).

neg(pos(137)).
square(137,o6).
circle(137,o5).
in(137,o5,o6).
circle(137,o4).
circle(137,o3).
in(137,o3,o4).
circle(137,o2).
circle(137,o1).
in(137,o1,o2).

neg(pos(140)).
square(140,o3).
triangle(140,o2).
config(140,o2,down).
in(140,o2,o3).
triangle(140,o1).
config(140,o1,down).

neg(pos(143)).
circle(143,o4).
triangle(143,o3).
config(143,o3,down).
in(143,o3,o4).
circle(143,o2).
circle(143,o1).
in(143,o1,o2).

neg(pos(144)).
triangle(144,o5).
config(144,o5,down).
circle(144,o4).
in(144,o4,o5).
square(144,o3).
square(144,o2).
in(144,o2,o3).
square(144,o1).

neg(pos(151)).
circle(151,o3).
circle(151,o2).
in(151,o2,o3).
square(151,o1).

neg(pos(154)).
circle(154,o4).
square(154,o3).
in(154,o3,o4).
circle(154,o2).
circle(154,o1).
in(154,o1,o2).

neg(pos(155)).
square(155,o3).
circle(155,o2).
in(155,o2,o3).
square(155,o1).

neg(pos(156)).
square(156,o6).
square(156,o5).
in(156,o5,o6).
triangle(156,o4).
config(156,o4,down).
square(156,o3).
in(156,o3,o4).
circle(156,o2).
circle(156,o1).
in(156,o1,o2).

neg(pos(159)).
triangle(159,o6).
config(159,o6,up).
square(159,o5).
in(159,o5,o6).
triangle(159,o4).
config(159,o4,down).
square(159,o3).
in(159,o3,o4).
square(159,o2).
triangle(159,o1).
config(159,o1,up).
in(159,o1,o2).

pos(167).
circle(167,o4).
circle(167,o3).
in(167,o3,o4).
triangle(167,o2).
config(167,o2,up).
triangle(167,o1).
config(167,o1,down).
in(167,o1,o2).

pos(168).
triangle(168,o4).
config(168,o4,up).
triangle(168,o3).
config(168,o3,down).
in(168,o3,o4).
square(168,o2).
triangle(168,o1).
config(168,o1,down).
in(168,o1,o2).

neg(pos(169)).
triangle(169,o6).
config(169,o6,up).
square(169,o5).
in(169,o5,o6).
circle(169,o4).
triangle(169,o3).
config(169,o3,down).
in(169,o3,o4).
circle(169,o2).
circle(169,o1).
in(169,o1,o2).

neg(pos(172)).
square(172,o4).
circle(172,o3).
in(172,o3,o4).
triangle(172,o2).
config(172,o2,up).
circle(172,o1).
in(172,o1,o2).

neg(pos(175)).
circle(175,o2).
circle(175,o1).
in(175,o1,o2).

neg(pos(176)).
circle(176,o5).
circle(176,o4).
in(176,o4,o5).
circle(176,o3).
square(176,o2).
in(176,o2,o3).
circle(176,o1).

neg(pos(177)).
circle(177,o4).
circle(177,o3).
in(177,o3,o4).
square(177,o2).
circle(177,o1).
in(177,o1,o2).

pos(178).
triangle(178,o4).
config(178,o4,up).
triangle(178,o3).
config(178,o3,up).
in(178,o3,o4).
triangle(178,o2).
config(178,o2,up).
square(178,o1).
in(178,o1,o2).

neg(pos(181)).
triangle(181,o2).
config(181,o2,up).
circle(181,o1).
in(181,o1,o2).

neg(pos(184)).
square(184,o2).
circle(184,o1).
in(184,o1,o2).

neg(pos(188)).
circle(188,o2).
circle(188,o1).
in(188,o1,o2).

neg(pos(190)).
square(190,o2).
triangle(190,o1).
config(190,o1,up).
in(190,o1,o2).

pos(192).
circle(192,o3).
square(192,o2).
in(192,o2,o3).
triangle(192,o1).
config(192,o1,down).

neg(pos(193)).
triangle(193,o4).
config(193,o4,up).
circle(193,o3).
in(193,o3,o4).
square(193,o2).
circle(193,o1).
in(193,o1,o2).

neg(pos(194)).
circle(194,o4).
square(194,o3).
in(194,o3,o4).
circle(194,o2).
circle(194,o1).
in(194,o1,o2).

neg(pos(196)).
square(196,o3).
circle(196,o2).
in(196,o2,o3).
triangle(196,o1).
config(196,o1,up).

pos(198).
triangle(198,o3).
config(198,o3,down).
triangle(198,o2).
config(198,o2,down).
in(198,o2,o3).
square(198,o1).

neg(pos(202)).
circle(202,o5).
circle(202,o4).
in(202,o4,o5).
square(202,o3).
square(202,o2).
in(202,o2,o3).
triangle(202,o1).
config(202,o1,down).

neg(pos(206)).
circle(206,o3).
circle(206,o2).
in(206,o2,o3).
circle(206,o1).

neg(pos(208)).
square(208,o4).
square(208,o3).
in(208,o3,o4).
triangle(208,o2).
config(208,o2,down).
circle(208,o1).
in(208,o1,o2).

neg(pos(209)).
square(209,o5).
square(209,o4).
in(209,o4,o5).
circle(209,o3).
circle(209,o2).
in(209,o2,o3).
circle(209,o1).

pos(211).
triangle(211,o2).
config(211,o2,down).
triangle(211,o1).
config(211,o1,down).
in(211,o1,o2).

pos(214).
triangle(214,o5).
config(214,o5,down).
square(214,o4).
in(214,o4,o5).
circle(214,o3).
square(214,o2).
in(214,o2,o3).
square(214,o1).

pos(219).
square(219,o4).
triangle(219,o3).
config(219,o3,up).
in(219,o3,o4).
triangle(219,o2).
config(219,o2,up).
triangle(219,o1).
config(219,o1,down).
in(219,o1,o2).

pos(222).
triangle(222,o4).
config(222,o4,up).
square(222,o3).
in(222,o3,o4).
triangle(222,o2).
config(222,o2,down).
triangle(222,o1).
config(222,o1,down).
in(222,o1,o2).

pos(223).
circle(223,o6).
circle(223,o5).
in(223,o5,o6).
circle(223,o4).
circle(223,o3).
in(223,o3,o4).
triangle(223,o2).
config(223,o2,up).
triangle(223,o1).
config(223,o1,down).
in(223,o1,o2).

pos(224).
circle(224,o6).
square(224,o5).
in(224,o5,o6).
square(224,o4).
square(224,o3).
in(224,o3,o4).
triangle(224,o2).
config(224,o2,up).
square(224,o1).
in(224,o1,o2).

pos(225).
circle(225,o3).
triangle(225,o2).
config(225,o2,up).
in(225,o2,o3).
triangle(225,o1).
config(225,o1,up).

pos(227).
circle(227,o3).
triangle(227,o2).
config(227,o2,up).
in(227,o2,o3).
circle(227,o1).

neg(pos(230)).
square(230,o5).
square(230,o4).
in(230,o4,o5).
square(230,o3).
triangle(230,o2).
config(230,o2,up).
in(230,o2,o3).
triangle(230,o1).
config(230,o1,down).

pos(231).
triangle(231,o2).
config(231,o2,up).
triangle(231,o1).
config(231,o1,down).
in(231,o1,o2).

neg(pos(233)).
square(233,o2).
circle(233,o1).
in(233,o1,o2).

pos(238).
triangle(238,o5).
config(238,o5,up).
triangle(238,o4).
config(238,o4,down).
in(238,o4,o5).
triangle(238,o3).
config(238,o3,down).
triangle(238,o2).
config(238,o2,up).
in(238,o2,o3).
circle(238,o1).

neg(pos(241)).
triangle(241,o5).
config(241,o5,up).
circle(241,o4).
in(241,o4,o5).
square(241,o3).
circle(241,o2).
in(241,o2,o3).
triangle(241,o1).
config(241,o1,down).

neg(pos(243)).
circle(243,o2).
circle(243,o1).
in(243,o1,o2).

pos(244).
square(244,o5).
square(244,o4).
in(244,o4,o5).
triangle(244,o3).
config(244,o3,down).
triangle(244,o2).
config(244,o2,down).
in(244,o2,o3).
square(244,o1).

pos(248).
circle(248,o3).
triangle(248,o2).
config(248,o2,up).
in(248,o2,o3).
triangle(248,o1).
config(248,o1,down).

neg(pos(249)).
circle(249,o5).
circle(249,o4).
in(249,o4,o5).
square(249,o3).
circle(249,o2).
in(249,o2,o3).
triangle(249,o1).
config(249,o1,down).

neg(pos(250)).
square(250,o6).
circle(250,o5).
in(250,o5,o6).
circle(250,o4).
square(250,o3).
in(250,o3,o4).
circle(250,o2).
square(250,o1).
in(250,o1,o2).

pos(256).
circle(256,o4).
triangle(256,o3).
config(256,o3,up).
in(256,o3,o4).
triangle(256,o2).
config(256,o2,up).
triangle(256,o1).
config(256,o1,down).
in(256,o1,o2).

pos(258).
triangle(258,o4).
config(258,o4,up).
triangle(258,o3).
config(258,o3,down).
in(258,o3,o4).
circle(258,o2).
triangle(258,o1).
config(258,o1,down).
in(258,o1,o2).

pos(260).
square(260,o4).
triangle(260,o3).
config(260,o3,down).
in(260,o3,o4).
triangle(260,o2).
config(260,o2,up).
triangle(260,o1).
config(260,o1,up).
in(260,o1,o2).

neg(pos(268)).
square(268,o5).
circle(268,o4).
in(268,o4,o5).
circle(268,o3).
square(268,o2).
in(268,o2,o3).
square(268,o1).

pos(270).
circle(270,o4).
triangle(270,o3).
config(270,o3,up).
in(270,o3,o4).
square(270,o2).
triangle(270,o1).
config(270,o1,down).
in(270,o1,o2).

neg(pos(273)).
square(273,o4).
triangle(273,o3).
config(273,o3,down).
in(273,o3,o4).
square(273,o2).
square(273,o1).
in(273,o1,o2).

neg(pos(280)).
square(280,o3).
square(280,o2).
in(280,o2,o3).
square(280,o1).

pos(282).
triangle(282,o2).
config(282,o2,up).
triangle(282,o1).
config(282,o1,up).
in(282,o1,o2).

neg(pos(286)).
triangle(286,o4).
config(286,o4,down).
circle(286,o3).
in(286,o3,o4).
triangle(286,o2).
config(286,o2,down).
square(286,o1).
in(286,o1,o2).

neg(pos(287)).
triangle(287,o4).
config(287,o4,up).
square(287,o3).
in(287,o3,o4).
triangle(287,o2).
config(287,o2,down).
circle(287,o1).
in(287,o1,o2).

pos(288).
circle(288,o5).
triangle(288,o4).
config(288,o4,up).
in(288,o4,o5).
circle(288,o3).
square(288,o2).
in(288,o2,o3).
square(288,o1).

neg(pos(289)).
triangle(289,o6).
config(289,o6,up).
square(289,o5).
in(289,o5,o6).
triangle(289,o4).
config(289,o4,up).
square(289,o3).
in(289,o3,o4).
square(289,o2).
circle(289,o1).
in(289,o1,o2).

neg(pos(290)).
triangle(290,o5).
config(290,o5,down).
circle(290,o4).
in(290,o4,o5).
triangle(290,o3).
config(290,o3,up).
circle(290,o2).
in(290,o2,o3).
square(290,o1).

pos(295).
circle(295,o6).
triangle(295,o5).
config(295,o5,down).
in(295,o5,o6).
triangle(295,o4).
config(295,o4,down).
triangle(295,o3).
config(295,o3,down).
in(295,o3,o4).
circle(295,o2).
triangle(295,o1).
config(295,o1,up).
in(295,o1,o2).

neg(pos(300)).
square(300,o3).
triangle(300,o2).
config(300,o2,down).
in(300,o2,o3).
triangle(300,o1).
config(300,o1,down).

neg(pos(301)).
square(301,o6).
triangle(301,o5).
config(301,o5,up).
in(301,o5,o6).
square(301,o4).
square(301,o3).
in(301,o3,o4).
triangle(301,o2).
config(301,o2,up).
circle(301,o1).
in(301,o1,o2).

neg(pos(303)).
circle(303,o3).
circle(303,o2).
in(303,o2,o3).
triangle(303,o1).
config(303,o1,up).

neg(pos(304)).
square(304,o3).
square(304,o2).
in(304,o2,o3).
triangle(304,o1).
config(304,o1,up).

neg(pos(307)).
square(307,o3).
circle(307,o2).
in(307,o2,o3).
square(307,o1).

neg(pos(309)).
triangle(309,o3).
config(309,o3,down).
circle(309,o2).
in(309,o2,o3).
circle(309,o1).

pos(314).
square(314,o5).
triangle(314,o4).
config(314,o4,down).
in(314,o4,o5).
triangle(314,o3).
config(314,o3,down).
triangle(314,o2).
config(314,o2,down).
in(314,o2,o3).
circle(314,o1).

neg(pos(316)).
square(316,o4).
triangle(316,o3).
config(316,o3,up).
in(316,o3,o4).
triangle(316,o2).
config(316,o2,up).
circle(316,o1).
in(316,o1,o2).

pos(319).
triangle(319,o5).
config(319,o5,down).
triangle(319,o4).
config(319,o4,down).
in(319,o4,o5).
square(319,o3).
square(319,o2).
in(319,o2,o3).
circle(319,o1).

neg(pos(321)).
triangle(321,o2).
config(321,o2,down).
circle(321,o1).
in(321,o1,o2).

neg(pos(324)).
square(324,o5).
square(324,o4).
in(324,o4,o5).
triangle(324,o3).
config(324,o3,down).
square(324,o2).
in(324,o2,o3).
square(324,o1).

neg(pos(326)).
square(326,o6).
circle(326,o5).
in(326,o5,o6).
circle(326,o4).
square(326,o3).
in(326,o3,o4).
square(326,o2).
circle(326,o1).
in(326,o1,o2).

neg(pos(327)).
square(327,o2).
circle(327,o1).
in(327,o1,o2).

neg(pos(328)).
square(328,o2).
circle(328,o1).
in(328,o1,o2).

neg(pos(329)).
triangle(329,o3).
config(329,o3,up).
circle(329,o2).
in(329,o2,o3).
square(329,o1).

pos(331).
triangle(331,o4).
config(331,o4,up).
triangle(331,o3).
config(331,o3,up).
in(331,o3,o4).
triangle(331,o2).
config(331,o2,down).
triangle(331,o1).
config(331,o1,down).
in(331,o1,o2).

neg(pos(334)).
triangle(334,o4).
config(334,o4,up).
circle(334,o3).
in(334,o3,o4).
square(334,o2).
square(334,o1).
in(334,o1,o2).

neg(pos(337)).
circle(337,o4).
circle(337,o3).
in(337,o3,o4).
triangle(337,o2).
config(337,o2,down).
square(337,o1).
in(337,o1,o2).

pos(343).
triangle(343,o6).
config(343,o6,down).
triangle(343,o5).
config(343,o5,up).
in(343,o5,o6).
square(343,o4).
square(343,o3).
in(343,o3,o4).
square(343,o2).
circle(343,o1).
in(343,o1,o2).

pos(345).
circle(345,o5).
triangle(345,o4).
config(345,o4,up).
in(345,o4,o5).
circle(345,o3).
triangle(345,o2).
config(345,o2,up).
in(345,o2,o3).
square(345,o1).

neg(pos(348)).
square(348,o4).
square(348,o3).
in(348,o3,o4).
square(348,o2).
circle(348,o1).
in(348,o1,o2).

neg(pos(352)).
triangle(352,o6).
config(352,o6,down).
circle(352,o5).
in(352,o5,o6).
triangle(352,o4).
config(352,o4,up).
circle(352,o3).
in(352,o3,o4).
square(352,o2).
square(352,o1).
in(352,o1,o2).

neg(pos(353)).
square(353,o2).
square(353,o1).
in(353,o1,o2).

neg(pos(355)).
square(355,o2).
triangle(355,o1).
config(355,o1,up).
in(355,o1,o2).

pos(358).
circle(358,o6).
square(358,o5).
in(358,o5,o6).
circle(358,o4).
triangle(358,o3).
config(358,o3,up).
in(358,o3,o4).
triangle(358,o2).
config(358,o2,up).
triangle(358,o1).
config(358,o1,down).
in(358,o1,o2).

pos(366).
square(366,o6).
triangle(366,o5).
config(366,o5,down).
in(366,o5,o6).
circle(366,o4).
triangle(366,o3).
config(366,o3,down).
in(366,o3,o4).
triangle(366,o2).
config(366,o2,down).
triangle(366,o1).
config(366,o1,up).
in(366,o1,o2).

neg(pos(369)).
triangle(369,o6).
config(369,o6,down).
circle(369,o5).
in(369,o5,o6).
square(369,o4).
square(369,o3).
in(369,o3,o4).
triangle(369,o2).
config(369,o2,up).
square(369,o1).
in(369,o1,o2).

neg(pos(370)).
circle(370,o6).
circle(370,o5).
in(370,o5,o6).
square(370,o4).
triangle(370,o3).
config(370,o3,down).
in(370,o3,o4).
square(370,o2).
square(370,o1).
in(370,o1,o2).

pos(373).
triangle(373,o3).
config(373,o3,up).
triangle(373,o2).
config(373,o2,down).
in(373,o2,o3).
square(373,o1).

pos(375).
triangle(375,o4).
config(375,o4,down).
circle(375,o3).
in(375,o3,o4).
triangle(375,o2).
config(375,o2,up).
triangle(375,o1).
config(375,o1,up).
in(375,o1,o2).

neg(pos(376)).
square(376,o4).
triangle(376,o3).
config(376,o3,down).
in(376,o3,o4).
square(376,o2).
square(376,o1).
in(376,o1,o2).

neg(pos(378)).
square(378,o3).
circle(378,o2).
in(378,o2,o3).
square(378,o1).

pos(379).
triangle(379,o3).
config(379,o3,up).
triangle(379,o2).
config(379,o2,up).
in(379,o2,o3).
triangle(379,o1).
config(379,o1,up).

pos(381).
circle(381,o5).
square(381,o4).
in(381,o4,o5).
square(381,o3).
triangle(381,o2).
config(381,o2,down).
in(381,o2,o3).
triangle(381,o1).
config(381,o1,up).

pos(382).
circle(382,o4).
triangle(382,o3).
config(382,o3,up).
in(382,o3,o4).
square(382,o2).
square(382,o1).
in(382,o1,o2).

pos(390).
triangle(390,o4).
config(390,o4,down).
circle(390,o3).
in(390,o3,o4).
triangle(390,o2).
config(390,o2,down).
triangle(390,o1).
config(390,o1,down).
in(390,o1,o2).

neg(pos(393)).
triangle(393,o3).
config(393,o3,up).
square(393,o2).
in(393,o2,o3).
square(393,o1).

pos(402).
triangle(402,o5).
config(402,o5,down).
square(402,o4).
in(402,o4,o5).
triangle(402,o3).
config(402,o3,down).
triangle(402,o2).
config(402,o2,up).
in(402,o2,o3).
circle(402,o1).

neg(pos(404)).
square(404,o3).
circle(404,o2).
in(404,o2,o3).
circle(404,o1).

neg(pos(408)).
square(408,o2).
square(408,o1).
in(408,o1,o2).

neg(pos(411)).
circle(411,o5).
triangle(411,o4).
config(411,o4,down).
in(411,o4,o5).
circle(411,o3).
circle(411,o2).
in(411,o2,o3).
square(411,o1).

neg(pos(412)).
circle(412,o6).
circle(412,o5).
in(412,o5,o6).
square(412,o4).
circle(412,o3).
in(412,o3,o4).
circle(412,o2).
square(412,o1).
in(412,o1,o2).

neg(pos(416)).
circle(416,o4).
square(416,o3).
in(416,o3,o4).
square(416,o2).
circle(416,o1).
in(416,o1,o2).

neg(pos(417)).
triangle(417,o6).
config(417,o6,up).
square(417,o5).
in(417,o5,o6).
square(417,o4).
circle(417,o3).
in(417,o3,o4).
triangle(417,o2).
config(417,o2,up).
square(417,o1).
in(417,o1,o2).

neg(pos(419)).
square(419,o2).
circle(419,o1).
in(419,o1,o2).

pos(420).
triangle(420,o4).
config(420,o4,up).
triangle(420,o3).
config(420,o3,up).
in(420,o3,o4).
triangle(420,o2).
config(420,o2,up).
triangle(420,o1).
config(420,o1,down).
in(420,o1,o2).

neg(pos(421)).
square(421,o2).
triangle(421,o1).
config(421,o1,up).
in(421,o1,o2).

neg(pos(424)).
square(424,o2).
circle(424,o1).
in(424,o1,o2).

neg(pos(425)).
triangle(425,o3).
config(425,o3,up).
circle(425,o2).
in(425,o2,o3).
square(425,o1).

pos(427).
square(427,o6).
circle(427,o5).
in(427,o5,o6).
square(427,o4).
square(427,o3).
in(427,o3,o4).
triangle(427,o2).
config(427,o2,up).
triangle(427,o1).
config(427,o1,up).
in(427,o1,o2).

pos(428).
circle(428,o4).
square(428,o3).
in(428,o3,o4).
square(428,o2).
triangle(428,o1).
config(428,o1,down).
in(428,o1,o2).

neg(pos(431)).
square(431,o2).
triangle(431,o1).
config(431,o1,up).
in(431,o1,o2).

neg(pos(432)).
square(432,o3).
circle(432,o2).
in(432,o2,o3).
circle(432,o1).

pos(433).
triangle(433,o6).
config(433,o6,down).
square(433,o5).
in(433,o5,o6).
circle(433,o4).
square(433,o3).
in(433,o3,o4).
circle(433,o2).
square(433,o1).
in(433,o1,o2).

neg(pos(437)).
triangle(437,o2).
config(437,o2,up).
circle(437,o1).
in(437,o1,o2).

neg(pos(444)).
triangle(444,o3).
config(444,o3,down).
circle(444,o2).
in(444,o2,o3).
triangle(444,o1).
config(444,o1,up).

neg(pos(445)).
square(445,o5).
triangle(445,o4).
config(445,o4,up).
in(445,o4,o5).
triangle(445,o3).
config(445,o3,down).
square(445,o2).
in(445,o2,o3).
square(445,o1).

neg(pos(447)).
square(447,o5).
square(447,o4).
in(447,o4,o5).
square(447,o3).
triangle(447,o2).
config(447,o2,up).
in(447,o2,o3).
triangle(447,o1).
config(447,o1,down).

pos(453).
circle(453,o6).
triangle(453,o5).
config(453,o5,down).
in(453,o5,o6).
triangle(453,o4).
config(453,o4,down).
triangle(453,o3).
config(453,o3,up).
in(453,o3,o4).
triangle(453,o2).
config(453,o2,up).
triangle(453,o1).
config(453,o1,up).
in(453,o1,o2).

pos(456).
triangle(456,o3).
config(456,o3,up).
triangle(456,o2).
config(456,o2,up).
in(456,o2,o3).
square(456,o1).

neg(pos(457)).
square(457,o4).
triangle(457,o3).
config(457,o3,up).
in(457,o3,o4).
triangle(457,o2).
config(457,o2,up).
square(457,o1).
in(457,o1,o2).

pos(459).
triangle(459,o6).
config(459,o6,down).
triangle(459,o5).
config(459,o5,up).
in(459,o5,o6).
circle(459,o4).
circle(459,o3).
in(459,o3,o4).
triangle(459,o2).
config(459,o2,down).
circle(459,o1).
in(459,o1,o2).

neg(pos(462)).
square(462,o2).
triangle(462,o1).
config(462,o1,down).
in(462,o1,o2).

pos(463).
square(463,o6).
triangle(463,o5).
config(463,o5,down).
in(463,o5,o6).
square(463,o4).
square(463,o3).
in(463,o3,o4).
circle(463,o2).
square(463,o1).
in(463,o1,o2).

pos(464).
triangle(464,o4).
config(464,o4,down).
square(464,o3).
in(464,o3,o4).
triangle(464,o2).
config(464,o2,up).
triangle(464,o1).
config(464,o1,up).
in(464,o1,o2).

pos(465).
triangle(465,o3).
config(465,o3,down).
triangle(465,o2).
config(465,o2,down).
in(465,o2,o3).
square(465,o1).

neg(pos(468)).
circle(468,o2).
circle(468,o1).
in(468,o1,o2).

neg(pos(470)).
triangle(470,o4).
config(470,o4,down).
square(470,o3).
in(470,o3,o4).
circle(470,o2).
circle(470,o1).
in(470,o1,o2).

neg(pos(473)).
square(473,o4).
triangle(473,o3).
config(473,o3,up).
in(473,o3,o4).
square(473,o2).
circle(473,o1).
in(473,o1,o2).

neg(pos(474)).
triangle(474,o6).
config(474,o6,up).
square(474,o5).
in(474,o5,o6).
square(474,o4).
circle(474,o3).
in(474,o3,o4).
square(474,o2).
circle(474,o1).
in(474,o1,o2).

neg(pos(476)).
triangle(476,o5).
config(476,o5,down).
square(476,o4).
in(476,o4,o5).
circle(476,o3).
circle(476,o2).
in(476,o2,o3).
square(476,o1).

neg(pos(477)).
square(477,o4).
triangle(477,o3).
config(477,o3,up).
in(477,o3,o4).
square(477,o2).
triangle(477,o1).
config(477,o1,up).
in(477,o1,o2).

pos(479).
circle(479,o2).
triangle(479,o1).
config(479,o1,up).
in(479,o1,o2).

neg(pos(481)).
triangle(481,o2).
config(481,o2,down).
circle(481,o1).
in(481,o1,o2).

pos(482).
triangle(482,o4).
config(482,o4,up).
triangle(482,o3).
config(482,o3,up).
in(482,o3,o4).
circle(482,o2).
triangle(482,o1).
config(482,o1,up).
in(482,o1,o2).

neg(pos(483)).
circle(483,o4).
circle(483,o3).
in(483,o3,o4).
square(483,o2).
square(483,o1).
in(483,o1,o2).

neg(pos(485)).
triangle(485,o2).
config(485,o2,down).
circle(485,o1).
in(485,o1,o2).

neg(pos(488)).
triangle(488,o4).
config(488,o4,down).
circle(488,o3).
in(488,o3,o4).
square(488,o2).
circle(488,o1).
in(488,o1,o2).

neg(pos(489)).
triangle(489,o3).
config(489,o3,down).
square(489,o2).
in(489,o2,o3).
square(489,o1).

neg(pos(490)).
triangle(490,o5).
config(490,o5,down).
circle(490,o4).
in(490,o4,o5).
circle(490,o3).
circle(490,o2).
in(490,o2,o3).
square(490,o1).

neg(pos(491)).
triangle(491,o2).
config(491,o2,up).
square(491,o1).
in(491,o1,o2).

neg(pos(494)).
triangle(494,o2).
config(494,o2,up).
square(494,o1).
in(494,o1,o2).

neg(pos(497)).
triangle(497,o2).
config(497,o2,up).
square(497,o1).
in(497,o1,o2).

neg(pos(499)).
square(499,o5).
triangle(499,o4).
config(499,o4,down).
in(499,o4,o5).
square(499,o3).
square(499,o2).
in(499,o2,o3).
square(499,o1).

neg(pos(500)).
circle(500,o3).
circle(500,o2).
in(500,o2,o3).
square(500,o1).

neg(pos(512)).
square(512,o2).
triangle(512,o1).
config(512,o1,down).
in(512,o1,o2).

neg(pos(513)).
triangle(513,o6).
config(513,o6,up).
square(513,o5).
in(513,o5,o6).
square(513,o4).
triangle(513,o3).
config(513,o3,up).
in(513,o3,o4).
triangle(513,o2).
config(513,o2,up).
square(513,o1).
in(513,o1,o2).

pos(516).
circle(516,o4).
square(516,o3).
in(516,o3,o4).
circle(516,o2).
triangle(516,o1).
config(516,o1,down).
in(516,o1,o2).

neg(pos(517)).
square(517,o5).
square(517,o4).
in(517,o4,o5).
triangle(517,o3).
config(517,o3,down).
square(517,o2).
in(517,o2,o3).
square(517,o1).

pos(520).
triangle(520,o2).
config(520,o2,down).
triangle(520,o1).
config(520,o1,down).
in(520,o1,o2).

neg(pos(521)).
circle(521,o6).
circle(521,o5).
in(521,o5,o6).
square(521,o4).
square(521,o3).
in(521,o3,o4).
circle(521,o2).
square(521,o1).
in(521,o1,o2).

pos(527).
circle(527,o5).
triangle(527,o4).
config(527,o4,up).
in(527,o4,o5).
circle(527,o3).
square(527,o2).
in(527,o2,o3).
triangle(527,o1).
config(527,o1,up).

neg(pos(529)).
square(529,o4).
triangle(529,o3).
config(529,o3,up).
in(529,o3,o4).
square(529,o2).
circle(529,o1).
in(529,o1,o2).

neg(pos(531)).
circle(531,o2).
circle(531,o1).
in(531,o1,o2).

neg(pos(533)).
triangle(533,o4).
config(533,o4,up).
square(533,o3).
in(533,o3,o4).
square(533,o2).
square(533,o1).
in(533,o1,o2).

neg(pos(534)).
circle(534,o2).
circle(534,o1).
in(534,o1,o2).

pos(539).
square(539,o6).
triangle(539,o5).
config(539,o5,down).
in(539,o5,o6).
circle(539,o4).
square(539,o3).
in(539,o3,o4).
square(539,o2).
square(539,o1).
in(539,o1,o2).

neg(pos(540)).
circle(540,o2).
circle(540,o1).
in(540,o1,o2).

neg(pos(542)).
square(542,o5).
square(542,o4).
in(542,o4,o5).
triangle(542,o3).
config(542,o3,up).
circle(542,o2).
in(542,o2,o3).
triangle(542,o1).
config(542,o1,down).

neg(pos(543)).
triangle(543,o4).
config(543,o4,up).
square(543,o3).
in(543,o3,o4).
circle(543,o2).
circle(543,o1).
in(543,o1,o2).

neg(pos(544)).
circle(544,o6).
circle(544,o5).
in(544,o5,o6).
square(544,o4).
circle(544,o3).
in(544,o3,o4).
circle(544,o2).
square(544,o1).
in(544,o1,o2).

neg(pos(546)).
square(546,o3).
circle(546,o2).
in(546,o2,o3).
circle(546,o1).

pos(550).
triangle(550,o4).
config(550,o4,up).
square(550,o3).
in(550,o3,o4).
circle(550,o2).
square(550,o1).
in(550,o1,o2).

pos(552).
circle(552,o4).
triangle(552,o3).
config(552,o3,down).
in(552,o3,o4).
circle(552,o2).
triangle(552,o1).
config(552,o1,up).
in(552,o1,o2).

neg(pos(553)).
circle(553,o2).
circle(553,o1).
in(553,o1,o2).

neg(pos(555)).
square(555,o4).
triangle(555,o3).
config(555,o3,down).
in(555,o3,o4).
square(555,o2).
circle(555,o1).
in(555,o1,o2).

neg(pos(559)).
square(559,o3).
circle(559,o2).
in(559,o2,o3).
triangle(559,o1).
config(559,o1,up).

pos(565).
triangle(565,o6).
config(565,o6,down).
triangle(565,o5).
config(565,o5,up).
in(565,o5,o6).
circle(565,o4).
square(565,o3).
in(565,o3,o4).
triangle(565,o2).
config(565,o2,up).
circle(565,o1).
in(565,o1,o2).

pos(567).
circle(567,o3).
square(567,o2).
in(567,o2,o3).
triangle(567,o1).
config(567,o1,down).

pos(568).
square(568,o6).
square(568,o5).
in(568,o5,o6).
square(568,o4).
triangle(568,o3).
config(568,o3,up).
in(568,o3,o4).
circle(568,o2).
triangle(568,o1).
config(568,o1,up).
in(568,o1,o2).

pos(572).
triangle(572,o4).
config(572,o4,down).
triangle(572,o3).
config(572,o3,down).
in(572,o3,o4).
square(572,o2).
triangle(572,o1).
config(572,o1,up).
in(572,o1,o2).

neg(pos(578)).
square(578,o2).
triangle(578,o1).
config(578,o1,up).
in(578,o1,o2).

pos(582).
triangle(582,o4).
config(582,o4,down).
triangle(582,o3).
config(582,o3,up).
in(582,o3,o4).
circle(582,o2).
triangle(582,o1).
config(582,o1,down).
in(582,o1,o2).

neg(pos(583)).
square(583,o3).
circle(583,o2).
in(583,o2,o3).
square(583,o1).

neg(pos(591)).
square(591,o2).
triangle(591,o1).
config(591,o1,up).
in(591,o1,o2).

pos(595).
square(595,o4).
triangle(595,o3).
config(595,o3,down).
in(595,o3,o4).
triangle(595,o2).
config(595,o2,down).
triangle(595,o1).
config(595,o1,up).
in(595,o1,o2).

pos(597).
circle(597,o4).
square(597,o3).
in(597,o3,o4).
triangle(597,o2).
config(597,o2,up).
square(597,o1).
in(597,o1,o2).

neg(pos(600)).
square(600,o2).
square(600,o1).
in(600,o1,o2).

pos(602).
circle(602,o5).
triangle(602,o4).
config(602,o4,down).
in(602,o4,o5).
circle(602,o3).
triangle(602,o2).
config(602,o2,up).
in(602,o2,o3).
circle(602,o1).

neg(pos(609)).
circle(609,o2).
circle(609,o1).
in(609,o1,o2).

neg(pos(612)).
triangle(612,o5).
config(612,o5,down).
circle(612,o4).
in(612,o4,o5).
square(612,o3).
square(612,o2).
in(612,o2,o3).
square(612,o1).

neg(pos(614)).
triangle(614,o3).
config(614,o3,up).
square(614,o2).
in(614,o2,o3).
square(614,o1).

neg(pos(615)).
square(615,o2).
triangle(615,o1).
config(615,o1,up).
in(615,o1,o2).

neg(pos(616)).
circle(616,o2).
circle(616,o1).
in(616,o1,o2).

neg(pos(617)).
square(617,o5).
triangle(617,o4).
config(617,o4,down).
in(617,o4,o5).
square(617,o3).
square(617,o2).
in(617,o2,o3).
square(617,o1).

pos(622).
triangle(622,o2).
config(622,o2,down).
triangle(622,o1).
config(622,o1,down).
in(622,o1,o2).

neg(pos(625)).
circle(625,o2).
circle(625,o1).
in(625,o1,o2).

neg(pos(628)).
triangle(628,o5).
config(628,o5,down).
circle(628,o4).
in(628,o4,o5).
square(628,o3).
square(628,o2).
in(628,o2,o3).
square(628,o1).

pos(634).
square(634,o6).
triangle(634,o5).
config(634,o5,up).
in(634,o5,o6).
triangle(634,o4).
config(634,o4,down).
triangle(634,o3).
config(634,o3,up).
in(634,o3,o4).
circle(634,o2).
square(634,o1).
in(634,o1,o2).

neg(pos(635)).
triangle(635,o5).
config(635,o5,down).
circle(635,o4).
in(635,o4,o5).
triangle(635,o3).
config(635,o3,up).
square(635,o2).
in(635,o2,o3).
circle(635,o1).

neg(pos(637)).
square(637,o2).
triangle(637,o1).
config(637,o1,up).
in(637,o1,o2).

pos(639).
triangle(639,o5).
config(639,o5,up).
square(639,o4).
in(639,o4,o5).
circle(639,o3).
square(639,o2).
in(639,o2,o3).
triangle(639,o1).
config(639,o1,up).

pos(640).
circle(640,o6).
square(640,o5).
in(640,o5,o6).
triangle(640,o4).
config(640,o4,up).
triangle(640,o3).
config(640,o3,down).
in(640,o3,o4).
triangle(640,o2).
config(640,o2,down).
square(640,o1).
in(640,o1,o2).

pos(641).
circle(641,o2).
triangle(641,o1).
config(641,o1,up).
in(641,o1,o2).

pos(642).
circle(642,o6).
square(642,o5).
in(642,o5,o6).
triangle(642,o4).
config(642,o4,up).
square(642,o3).
in(642,o3,o4).
circle(642,o2).
square(642,o1).
in(642,o1,o2).

pos(647).
triangle(647,o4).
config(647,o4,up).
triangle(647,o3).
config(647,o3,down).
in(647,o3,o4).
square(647,o2).
square(647,o1).
in(647,o1,o2).

pos(648).
triangle(648,o6).
config(648,o6,down).
triangle(648,o5).
config(648,o5,down).
in(648,o5,o6).
square(648,o4).
circle(648,o3).
in(648,o3,o4).
circle(648,o2).
circle(648,o1).
in(648,o1,o2).

pos(649).
square(649,o4).
triangle(649,o3).
config(649,o3,up).
in(649,o3,o4).
circle(649,o2).
square(649,o1).
in(649,o1,o2).

pos(650).
circle(650,o4).
square(650,o3).
in(650,o3,o4).
square(650,o2).
triangle(650,o1).
config(650,o1,up).
in(650,o1,o2).

neg(pos(654)).
triangle(654,o4).
config(654,o4,up).
circle(654,o3).
in(654,o3,o4).
circle(654,o2).
circle(654,o1).
in(654,o1,o2).

neg(pos(656)).
square(656,o2).
triangle(656,o1).
config(656,o1,down).
in(656,o1,o2).

pos(657).
circle(657,o3).
triangle(657,o2).
config(657,o2,up).
in(657,o2,o3).
square(657,o1).

neg(pos(658)).
square(658,o4).
circle(658,o3).
in(658,o3,o4).
square(658,o2).
circle(658,o1).
in(658,o1,o2).

neg(pos(662)).
square(662,o3).
circle(662,o2).
in(662,o2,o3).
circle(662,o1).

pos(667).
circle(667,o6).
circle(667,o5).
in(667,o5,o6).
triangle(667,o4).
config(667,o4,down).
circle(667,o3).
in(667,o3,o4).
triangle(667,o2).
config(667,o2,up).
triangle(667,o1).
config(667,o1,down).
in(667,o1,o2).

neg(pos(669)).
square(669,o4).
square(669,o3).
in(669,o3,o4).
square(669,o2).
triangle(669,o1).
config(669,o1,down).
in(669,o1,o2).

neg(pos(671)).
square(671,o5).
square(671,o4).
in(671,o4,o5).
triangle(671,o3).
config(671,o3,up).
circle(671,o2).
in(671,o2,o3).
square(671,o1).

neg(pos(679)).
triangle(679,o2).
config(679,o2,up).
square(679,o1).
in(679,o1,o2).

neg(pos(682)).
triangle(682,o4).
config(682,o4,up).
square(682,o3).
in(682,o3,o4).
triangle(682,o2).
config(682,o2,up).
square(682,o1).
in(682,o1,o2).

neg(pos(683)).
square(683,o4).
circle(683,o3).
in(683,o3,o4).
triangle(683,o2).
config(683,o2,down).
circle(683,o1).
in(683,o1,o2).

pos(685).
square(685,o6).
circle(685,o5).
in(685,o5,o6).
circle(685,o4).
circle(685,o3).
in(685,o3,o4).
triangle(685,o2).
config(685,o2,up).
triangle(685,o1).
config(685,o1,up).
in(685,o1,o2).

pos(686).
circle(686,o4).
triangle(686,o3).
config(686,o3,up).
in(686,o3,o4).
square(686,o2).
triangle(686,o1).
config(686,o1,down).
in(686,o1,o2).

neg(pos(691)).
square(691,o5).
circle(691,o4).
in(691,o4,o5).
triangle(691,o3).
config(691,o3,up).
square(691,o2).
in(691,o2,o3).
circle(691,o1).

neg(pos(693)).
triangle(693,o6).
config(693,o6,up).
square(693,o5).
in(693,o5,o6).
square(693,o4).
circle(693,o3).
in(693,o3,o4).
square(693,o2).
triangle(693,o1).
config(693,o1,down).
in(693,o1,o2).

neg(pos(698)).
triangle(698,o6).
config(698,o6,up).
circle(698,o5).
in(698,o5,o6).
square(698,o4).
square(698,o3).
in(698,o3,o4).
square(698,o2).
square(698,o1).
in(698,o1,o2).

neg(pos(700)).
triangle(700,o2).
config(700,o2,down).
circle(700,o1).
in(700,o1,o2).

neg(pos(701)).
circle(701,o5).
square(701,o4).
in(701,o4,o5).
circle(701,o3).
circle(701,o2).
in(701,o2,o3).
circle(701,o1).

neg(pos(705)).
circle(705,o5).
circle(705,o4).
in(705,o4,o5).
triangle(705,o3).
config(705,o3,up).
circle(705,o2).
in(705,o2,o3).
circle(705,o1).

neg(pos(708)).
triangle(708,o5).
config(708,o5,up).
square(708,o4).
in(708,o4,o5).
circle(708,o3).
circle(708,o2).
in(708,o2,o3).
square(708,o1).

neg(pos(709)).
circle(709,o5).
circle(709,o4).
in(709,o4,o5).
triangle(709,o3).
config(709,o3,down).
circle(709,o2).
in(709,o2,o3).
square(709,o1).

neg(pos(710)).
square(710,o4).
triangle(710,o3).
config(710,o3,down).
in(710,o3,o4).
square(710,o2).
triangle(710,o1).
config(710,o1,down).
in(710,o1,o2).

neg(pos(719)).
square(719,o4).
square(719,o3).
in(719,o3,o4).
triangle(719,o2).
config(719,o2,down).
circle(719,o1).
in(719,o1,o2).

neg(pos(722)).
triangle(722,o4).
config(722,o4,down).
square(722,o3).
in(722,o3,o4).
triangle(722,o2).
config(722,o2,up).
square(722,o1).
in(722,o1,o2).

neg(pos(723)).
circle(723,o4).
circle(723,o3).
in(723,o3,o4).
triangle(723,o2).
config(723,o2,down).
circle(723,o1).
in(723,o1,o2).

pos(725).
triangle(725,o2).
config(725,o2,down).
triangle(725,o1).
config(725,o1,down).
in(725,o1,o2).

neg(pos(728)).
circle(728,o4).
circle(728,o3).
in(728,o3,o4).
square(728,o2).
circle(728,o1).
in(728,o1,o2).

neg(pos(732)).
circle(732,o5).
triangle(732,o4).
config(732,o4,down).
in(732,o4,o5).
triangle(732,o3).
config(732,o3,up).
circle(732,o2).
in(732,o2,o3).
circle(732,o1).

neg(pos(734)).
square(734,o4).
circle(734,o3).
in(734,o3,o4).
triangle(734,o2).
config(734,o2,down).
circle(734,o1).
in(734,o1,o2).

neg(pos(737)).
square(737,o4).
circle(737,o3).
in(737,o3,o4).
square(737,o2).
triangle(737,o1).
config(737,o1,down).
in(737,o1,o2).

neg(pos(740)).
square(740,o2).
square(740,o1).
in(740,o1,o2).

neg(pos(741)).
triangle(741,o5).
config(741,o5,up).
circle(741,o4).
in(741,o4,o5).
square(741,o3).
circle(741,o2).
in(741,o2,o3).
triangle(741,o1).
config(741,o1,down).

neg(pos(743)).
triangle(743,o2).
config(743,o2,down).
square(743,o1).
in(743,o1,o2).

neg(pos(744)).
triangle(744,o5).
config(744,o5,down).
square(744,o4).
in(744,o4,o5).
triangle(744,o3).
config(744,o3,down).
square(744,o2).
in(744,o2,o3).
triangle(744,o1).
config(744,o1,down).

pos(745).
circle(745,o3).
square(745,o2).
in(745,o2,o3).
triangle(745,o1).
config(745,o1,up).

neg(pos(747)).
square(747,o2).
square(747,o1).
in(747,o1,o2).

neg(pos(748)).
square(748,o5).
circle(748,o4).
in(748,o4,o5).
triangle(748,o3).
config(748,o3,down).
circle(748,o2).
in(748,o2,o3).
triangle(748,o1).
config(748,o1,up).

neg(pos(750)).
square(750,o4).
triangle(750,o3).
config(750,o3,up).
in(750,o3,o4).
triangle(750,o2).
config(750,o2,up).
square(750,o1).
in(750,o1,o2).

neg(pos(751)).
triangle(751,o2).
config(751,o2,down).
square(751,o1).
in(751,o1,o2).

pos(753).
circle(753,o6).
triangle(753,o5).
config(753,o5,up).
in(753,o5,o6).
square(753,o4).
square(753,o3).
in(753,o3,o4).
square(753,o2).
square(753,o1).
in(753,o1,o2).

neg(pos(754)).
square(754,o3).
triangle(754,o2).
config(754,o2,up).
in(754,o2,o3).
triangle(754,o1).
config(754,o1,up).

neg(pos(755)).
triangle(755,o2).
config(755,o2,up).
circle(755,o1).
in(755,o1,o2).

neg(pos(765)).
triangle(765,o2).
config(765,o2,down).
square(765,o1).
in(765,o1,o2).

neg(pos(766)).
triangle(766,o3).
config(766,o3,up).
square(766,o2).
in(766,o2,o3).
triangle(766,o1).
config(766,o1,down).

pos(768).
square(768,o6).
square(768,o5).
in(768,o5,o6).
circle(768,o4).
triangle(768,o3).
config(768,o3,down).
in(768,o3,o4).
triangle(768,o2).
config(768,o2,down).
triangle(768,o1).
config(768,o1,up).
in(768,o1,o2).

neg(pos(769)).
circle(769,o2).
circle(769,o1).
in(769,o1,o2).

neg(pos(772)).
square(772,o5).
circle(772,o4).
in(772,o4,o5).
square(772,o3).
triangle(772,o2).
config(772,o2,down).
in(772,o2,o3).
triangle(772,o1).
config(772,o1,down).

neg(pos(773)).
square(773,o5).
triangle(773,o4).
config(773,o4,up).
in(773,o4,o5).
square(773,o3).
circle(773,o2).
in(773,o2,o3).
square(773,o1).

neg(pos(777)).
triangle(777,o3).
config(777,o3,up).
circle(777,o2).
in(777,o2,o3).
circle(777,o1).

neg(pos(779)).
square(779,o5).
triangle(779,o4).
config(779,o4,down).
in(779,o4,o5).
triangle(779,o3).
config(779,o3,up).
square(779,o2).
in(779,o2,o3).
square(779,o1).

neg(pos(780)).
square(780,o5).
square(780,o4).
in(780,o4,o5).
circle(780,o3).
circle(780,o2).
in(780,o2,o3).
circle(780,o1).

neg(pos(781)).
triangle(781,o6).
config(781,o6,up).
square(781,o5).
in(781,o5,o6).
square(781,o4).
triangle(781,o3).
config(781,o3,up).
in(781,o3,o4).
triangle(781,o2).
config(781,o2,up).
circle(781,o1).
in(781,o1,o2).

pos(787).
square(787,o6).
triangle(787,o5).
config(787,o5,down).
in(787,o5,o6).
triangle(787,o4).
config(787,o4,down).
square(787,o3).
in(787,o3,o4).
circle(787,o2).
square(787,o1).
in(787,o1,o2).

neg(pos(794)).
triangle(794,o3).
config(794,o3,up).
square(794,o2).
in(794,o2,o3).
triangle(794,o1).
config(794,o1,down).

neg(pos(795)).
triangle(795,o3).
config(795,o3,down).
circle(795,o2).
in(795,o2,o3).
triangle(795,o1).
config(795,o1,up).

neg(pos(797)).
circle(797,o5).
circle(797,o4).
in(797,o4,o5).
circle(797,o3).
square(797,o2).
in(797,o2,o3).
circle(797,o1).

neg(pos(803)).
triangle(803,o4).
config(803,o4,up).
circle(803,o3).
in(803,o3,o4).
square(803,o2).
square(803,o1).
in(803,o1,o2).

pos(805).
circle(805,o6).
triangle(805,o5).
config(805,o5,down).
in(805,o5,o6).
square(805,o4).
triangle(805,o3).
config(805,o3,down).
in(805,o3,o4).
triangle(805,o2).
config(805,o2,up).
triangle(805,o1).
config(805,o1,up).
in(805,o1,o2).

neg(pos(807)).
square(807,o5).
triangle(807,o4).
config(807,o4,up).
in(807,o4,o5).
triangle(807,o3).
config(807,o3,down).
circle(807,o2).
in(807,o2,o3).
circle(807,o1).

neg(pos(814)).
square(814,o2).
square(814,o1).
in(814,o1,o2).

neg(pos(815)).
circle(815,o5).
circle(815,o4).
in(815,o4,o5).
square(815,o3).
circle(815,o2).
in(815,o2,o3).
square(815,o1).

pos(816).
triangle(816,o6).
config(816,o6,up).
triangle(816,o5).
config(816,o5,down).
in(816,o5,o6).
triangle(816,o4).
config(816,o4,up).
circle(816,o3).
in(816,o3,o4).
square(816,o2).
triangle(816,o1).
config(816,o1,up).
in(816,o1,o2).

neg(pos(818)).
square(818,o2).
triangle(818,o1).
config(818,o1,up).
in(818,o1,o2).

neg(pos(819)).
circle(819,o5).
circle(819,o4).
in(819,o4,o5).
square(819,o3).
square(819,o2).
in(819,o2,o3).
circle(819,o1).

neg(pos(822)).
square(822,o3).
square(822,o2).
in(822,o2,o3).
square(822,o1).

neg(pos(829)).
square(829,o3).
square(829,o2).
in(829,o2,o3).
triangle(829,o1).
config(829,o1,down).

neg(pos(832)).
square(832,o5).
circle(832,o4).
in(832,o4,o5).
triangle(832,o3).
config(832,o3,down).
circle(832,o2).
in(832,o2,o3).
square(832,o1).

neg(pos(833)).
triangle(833,o3).
config(833,o3,up).
circle(833,o2).
in(833,o2,o3).
circle(833,o1).

pos(835).
triangle(835,o6).
config(835,o6,down).
square(835,o5).
in(835,o5,o6).
triangle(835,o4).
config(835,o4,up).
triangle(835,o3).
config(835,o3,up).
in(835,o3,o4).
circle(835,o2).
triangle(835,o1).
config(835,o1,down).
in(835,o1,o2).

neg(pos(836)).
square(836,o6).
square(836,o5).
in(836,o5,o6).
circle(836,o4).
square(836,o3).
in(836,o3,o4).
square(836,o2).
circle(836,o1).
in(836,o1,o2).

pos(837).
triangle(837,o5).
config(837,o5,down).
triangle(837,o4).
config(837,o4,down).
in(837,o4,o5).
square(837,o3).
triangle(837,o2).
config(837,o2,down).
in(837,o2,o3).
square(837,o1).

neg(pos(838)).
square(838,o4).
triangle(838,o3).
config(838,o3,up).
in(838,o3,o4).
triangle(838,o2).
config(838,o2,up).
square(838,o1).
in(838,o1,o2).

pos(841).
triangle(841,o5).
config(841,o5,down).
triangle(841,o4).
config(841,o4,down).
in(841,o4,o5).
square(841,o3).
circle(841,o2).
in(841,o2,o3).
circle(841,o1).

pos(844).
square(844,o6).
square(844,o5).
in(844,o5,o6).
circle(844,o4).
triangle(844,o3).
config(844,o3,up).
in(844,o3,o4).
circle(844,o2).
square(844,o1).
in(844,o1,o2).

neg(pos(845)).
triangle(845,o4).
config(845,o4,up).
circle(845,o3).
in(845,o3,o4).
triangle(845,o2).
config(845,o2,down).
circle(845,o1).
in(845,o1,o2).

neg(pos(846)).
square(846,o5).
square(846,o4).
in(846,o4,o5).
square(846,o3).
square(846,o2).
in(846,o2,o3).
square(846,o1).

neg(pos(847)).
circle(847,o6).
circle(847,o5).
in(847,o5,o6).
circle(847,o4).
square(847,o3).
in(847,o3,o4).
circle(847,o2).
square(847,o1).
in(847,o1,o2).

neg(pos(848)).
triangle(848,o3).
config(848,o3,down).
circle(848,o2).
in(848,o2,o3).
triangle(848,o1).
config(848,o1,down).

pos(849).
triangle(849,o5).
config(849,o5,down).
triangle(849,o4).
config(849,o4,up).
in(849,o4,o5).
circle(849,o3).
triangle(849,o2).
config(849,o2,up).
in(849,o2,o3).
square(849,o1).

neg(pos(856)).
triangle(856,o2).
config(856,o2,down).
square(856,o1).
in(856,o1,o2).

neg(pos(859)).
square(859,o2).
circle(859,o1).
in(859,o1,o2).

neg(pos(862)).
square(862,o3).
triangle(862,o2).
config(862,o2,down).
in(862,o2,o3).
square(862,o1).

pos(864).
circle(864,o3).
triangle(864,o2).
config(864,o2,up).
in(864,o2,o3).
square(864,o1).

neg(pos(867)).
circle(867,o5).
circle(867,o4).
in(867,o4,o5).
square(867,o3).
triangle(867,o2).
config(867,o2,up).
in(867,o2,o3).
circle(867,o1).

neg(pos(872)).
square(872,o3).
circle(872,o2).
in(872,o2,o3).
circle(872,o1).

pos(874).
triangle(874,o3).
config(874,o3,down).
triangle(874,o2).
config(874,o2,down).
in(874,o2,o3).
square(874,o1).

neg(pos(876)).
square(876,o2).
circle(876,o1).
in(876,o1,o2).

pos(880).
circle(880,o6).
circle(880,o5).
in(880,o5,o6).
triangle(880,o4).
config(880,o4,up).
triangle(880,o3).
config(880,o3,up).
in(880,o3,o4).
circle(880,o2).
circle(880,o1).
in(880,o1,o2).

neg(pos(882)).
triangle(882,o4).
config(882,o4,down).
circle(882,o3).
in(882,o3,o4).
circle(882,o2).
circle(882,o1).
in(882,o1,o2).

neg(pos(887)).
triangle(887,o4).
config(887,o4,down).
square(887,o3).
in(887,o3,o4).
triangle(887,o2).
config(887,o2,up).
square(887,o1).
in(887,o1,o2).

neg(pos(890)).
triangle(890,o3).
config(890,o3,down).
square(890,o2).
in(890,o2,o3).
square(890,o1).

neg(pos(892)).
triangle(892,o5).
config(892,o5,down).
circle(892,o4).
in(892,o4,o5).
square(892,o3).
triangle(892,o2).
config(892,o2,up).
in(892,o2,o3).
square(892,o1).

pos(893).
triangle(893,o2).
config(893,o2,up).
triangle(893,o1).
config(893,o1,down).
in(893,o1,o2).

neg(pos(897)).
square(897,o6).
square(897,o5).
in(897,o5,o6).
triangle(897,o4).
config(897,o4,up).
square(897,o3).
in(897,o3,o4).
triangle(897,o2).
config(897,o2,up).
square(897,o1).
in(897,o1,o2).

neg(pos(899)).
circle(899,o2).
circle(899,o1).
in(899,o1,o2).

neg(pos(900)).
square(900,o6).
triangle(900,o5).
config(900,o5,down).
in(900,o5,o6).
square(900,o4).
triangle(900,o3).
config(900,o3,up).
in(900,o3,o4).
square(900,o2).
square(900,o1).
in(900,o1,o2).

neg(pos(904)).
circle(904,o5).
circle(904,o4).
in(904,o4,o5).
square(904,o3).
circle(904,o2).
in(904,o2,o3).
circle(904,o1).

pos(909).
circle(909,o6).
square(909,o5).
in(909,o5,o6).
triangle(909,o4).
config(909,o4,up).
triangle(909,o3).
config(909,o3,up).
in(909,o3,o4).
square(909,o2).
circle(909,o1).
in(909,o1,o2).

pos(910).
triangle(910,o4).
config(910,o4,down).
square(910,o3).
in(910,o3,o4).
circle(910,o2).
square(910,o1).
in(910,o1,o2).

neg(pos(912)).
circle(912,o6).
triangle(912,o5).
config(912,o5,down).
in(912,o5,o6).
triangle(912,o4).
config(912,o4,up).
square(912,o3).
in(912,o3,o4).
triangle(912,o2).
config(912,o2,up).
circle(912,o1).
in(912,o1,o2).

neg(pos(915)).
triangle(915,o5).
config(915,o5,up).
square(915,o4).
in(915,o4,o5).
triangle(915,o3).
config(915,o3,down).
circle(915,o2).
in(915,o2,o3).
circle(915,o1).

neg(pos(917)).
triangle(917,o3).
config(917,o3,up).
square(917,o2).
in(917,o2,o3).
triangle(917,o1).
config(917,o1,up).

neg(pos(926)).
square(926,o4).
square(926,o3).
in(926,o3,o4).
square(926,o2).
circle(926,o1).
in(926,o1,o2).

neg(pos(927)).
triangle(927,o5).
config(927,o5,down).
circle(927,o4).
in(927,o4,o5).
triangle(927,o3).
config(927,o3,up).
circle(927,o2).
in(927,o2,o3).
circle(927,o1).

neg(pos(929)).
square(929,o2).
circle(929,o1).
in(929,o1,o2).

pos(930).
triangle(930,o3).
config(930,o3,up).
triangle(930,o2).
config(930,o2,up).
in(930,o2,o3).
square(930,o1).

neg(pos(931)).
square(931,o2).
triangle(931,o1).
config(931,o1,up).
in(931,o1,o2).

pos(932).
triangle(932,o5).
config(932,o5,down).
square(932,o4).
in(932,o4,o5).
triangle(932,o3).
config(932,o3,down).
triangle(932,o2).
config(932,o2,up).
in(932,o2,o3).
circle(932,o1).

neg(pos(933)).
square(933,o3).
square(933,o2).
in(933,o2,o3).
triangle(933,o1).
config(933,o1,down).

pos(938).
circle(938,o6).
circle(938,o5).
in(938,o5,o6).
triangle(938,o4).
config(938,o4,down).
triangle(938,o3).
config(938,o3,up).
in(938,o3,o4).
square(938,o2).
triangle(938,o1).
config(938,o1,down).
in(938,o1,o2).

neg(pos(939)).
triangle(939,o2).
config(939,o2,up).
square(939,o1).
in(939,o1,o2).

pos(940).
triangle(940,o4).
config(940,o4,down).
triangle(940,o3).
config(940,o3,up).
in(940,o3,o4).
circle(940,o2).
triangle(940,o1).
config(940,o1,up).
in(940,o1,o2).

neg(pos(941)).
triangle(941,o3).
config(941,o3,down).
circle(941,o2).
in(941,o2,o3).
square(941,o1).

neg(pos(944)).
circle(944,o4).
circle(944,o3).
in(944,o3,o4).
circle(944,o2).
triangle(944,o1).
config(944,o1,down).
in(944,o1,o2).

neg(pos(945)).
square(945,o5).
square(945,o4).
in(945,o4,o5).
square(945,o3).
circle(945,o2).
in(945,o2,o3).
triangle(945,o1).
config(945,o1,down).

pos(946).
triangle(946,o5).
config(946,o5,up).
circle(946,o4).
in(946,o4,o5).
triangle(946,o3).
config(946,o3,down).
triangle(946,o2).
config(946,o2,down).
in(946,o2,o3).
circle(946,o1).

pos(947).
circle(947,o5).
circle(947,o4).
in(947,o4,o5).
triangle(947,o3).
config(947,o3,down).
triangle(947,o2).
config(947,o2,down).
in(947,o2,o3).
circle(947,o1).

neg(pos(955)).
circle(955,o6).
triangle(955,o5).
config(955,o5,down).
in(955,o5,o6).
square(955,o4).
triangle(955,o3).
config(955,o3,up).
in(955,o3,o4).
square(955,o2).
circle(955,o1).
in(955,o1,o2).

pos(957).
triangle(957,o6).
config(957,o6,down).
circle(957,o5).
in(957,o5,o6).
circle(957,o4).
square(957,o3).
in(957,o3,o4).
triangle(957,o2).
config(957,o2,up).
triangle(957,o1).
config(957,o1,up).
in(957,o1,o2).

neg(pos(961)).
square(961,o4).
circle(961,o3).
in(961,o3,o4).
circle(961,o2).
circle(961,o1).
in(961,o1,o2).

pos(971).
circle(971,o5).
triangle(971,o4).
config(971,o4,down).
in(971,o4,o5).
triangle(971,o3).
config(971,o3,up).
triangle(971,o2).
config(971,o2,down).
in(971,o2,o3).
square(971,o1).

neg(pos(973)).
square(973,o4).
circle(973,o3).
in(973,o3,o4).
triangle(973,o2).
config(973,o2,down).
circle(973,o1).
in(973,o1,o2).

neg(pos(974)).
circle(974,o6).
circle(974,o5).
in(974,o5,o6).
square(974,o4).
circle(974,o3).
in(974,o3,o4).
circle(974,o2).
circle(974,o1).
in(974,o1,o2).

neg(pos(975)).
square(975,o3).
triangle(975,o2).
config(975,o2,down).
in(975,o2,o3).
triangle(975,o1).
config(975,o1,up).

neg(pos(977)).
circle(977,o2).
circle(977,o1).
in(977,o1,o2).

pos(978).
triangle(978,o2).
config(978,o2,down).
triangle(978,o1).
config(978,o1,up).
in(978,o1,o2).

pos(979).
circle(979,o2).
triangle(979,o1).
config(979,o1,up).
in(979,o1,o2).

neg(pos(984)).
square(984,o2).
circle(984,o1).
in(984,o1,o2).

neg(pos(989)).
triangle(989,o2).
config(989,o2,up).
circle(989,o1).
in(989,o1,o2).

pos(991).
circle(991,o6).
square(991,o5).
in(991,o5,o6).
triangle(991,o4).
config(991,o4,down).
square(991,o3).
in(991,o3,o4).
triangle(991,o2).
config(991,o2,up).
triangle(991,o1).
config(991,o1,down).
in(991,o1,o2).

pos(995).
triangle(995,o6).
config(995,o6,up).
triangle(995,o5).
config(995,o5,up).
in(995,o5,o6).
square(995,o4).
square(995,o3).
in(995,o3,o4).
circle(995,o2).
circle(995,o1).
in(995,o1,o2).

neg(pos(997)).
square(997,o3).
square(997,o2).
in(997,o2,o3).
triangle(997,o1).
config(997,o1,down).

neg(pos(1000)).
square(1000,o6).
triangle(1000,o5).
config(1000,o5,up).
in(1000,o5,o6).
triangle(1000,o4).
config(1000,o4,up).
circle(1000,o3).
in(1000,o3,o4).
triangle(1000,o2).
config(1000,o2,down).
circle(1000,o1).
in(1000,o1,o2).

