% Render parse trees using a tree, but ignore lists Relies on native SVG
% support in the browser. IF THE ANSWER LOOKS EMPTY, COMMENT OR REMOVE
% THE LINE BELOW.
:- use_rendering(svgtree, [list(false)]).

% A simple English DCG grammar
% ============================

s(s(NP,VP)) --> np(NP, Num), vp(VP, Num).

np(NP, Num) --> pn(NP, Num).
np(np(Det,N), Num) --> det(Det, Num), n(N, Num).
np(np(Det,N,PP), Num) --> det(Det, Num), n(N, Num), pp(PP).

vp(vp(V,NP), Num) --> v(V, Num), np(NP, _).
vp(vp(V,NP,PP), Num) --> v(V, Num), np(NP, _), pp(PP).

pp(pp(P,NP)) --> p(P), np(NP, _).

det(det(a), sg) --> [a].
det(det(the), _) --> [the].

pn(pn(john), sg) --> [john].

n(n(man), sg) --> [man].
n(n(men), pl) --> [men].
n(n(telescope), sg) --> [telescope].

v(v(sees), sg) --> [sees].
v(v(see), pl) --> [see].
v(v(saw), _) --> [saw].

p(p(with)) --> [with].


/** <examples>

?- phrase(s(Tree), [john, saw, a, man, with, a, telescope]).
?- phrase(s(Tree), Sentence).
?- between(1, 8, N), length(S, N), phrase(s(_), S), writeln(S), sleep(0.2), false.

*/
