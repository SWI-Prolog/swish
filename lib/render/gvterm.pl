/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, VU University Amsterdam
			 CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_render_term,
	  [ term_rendering//3                  % +Term, +Vars, +Options
	  ]).
:- use_module(library(dcg/basics)).
:- use_module(graphviz, [render_dot//3]).
:- use_module(library(gensym)).
:- use_module('../render').

:- register_renderer(term, "Render term structure as a graph").

/** <module> View complex terms using Graphviz

This library translates complex Prolog terms  into Graphviz (dot) output
for graphical rendering.
*/

term_rendering(Term, _Vars, Options) -->
	{ with_output_to(string(DotString), term_to_dot(Term))
	},
	render_dot(DotString, dot, Options).

%%	term_to_dot(+Term) is det.
%
%	Emit a dot representation for Term to the curent output.

term_to_dot(Term) :-
	term_to_dot(current_output, Term).


%%	term_to_dot(+Out:stream, Term) is det.
%
%	Emit a dot representation for Term to the stream Out.

term_to_dot(Out, Term) :-
	\+ \+ ( numbervars(Term, 0, _, [singletons(true)]),
		'$factorize_term'(Term, Skel, Subst),
		label_factors(Subst),
		phrase(struct0(Skel), Codes),
		format(Out, 'digraph structs {\n  node [shape=record];\n~s}\n', [Codes])
	      ).


label_factors([]).
label_factors([V='$VAR'(X)|T]) :- !,
	V = '$VAR'(X),
	label_factors(T).
label_factors(['$SKEL'(_,C)=C|T]) :-
	label_factors(T).

%%	struct0(+Term)//
%
%	Deal with the outer term.  Note that labels inside terms are
%	embedded in the term label.

struct0(Prim) -->
	{ number(Prim), !,
	  format(codes(Codes), '~q', [Prim])
	},
	cstring(Codes).
struct0(Prim) -->
	{ primitive(Prim), !,
	  format(codes(Codes), '~q', [Prim])
	},
	"\"", cstring(Codes), "\"".
struct0(Term) -->
	struct(Term, -(_), Links, []),
	links(Links).

%%	struct(+Term, Link, Links, RestLinks)//
%
%	Deal with compound and inner terms.

struct('$SKEL'(Done, C), -(Id), Links, LinksT) -->
	{ var(Done), !,
	  Done = top(Id)
	},
	struct(C, -(Id), Links, LinksT).
struct('$SKEL'(Done, C), Id-Arg, [link_c(Id-Arg, Id2, C)|LinkT], LinkT) -->
	{ var(Done), !,
	  Done = id(Id2)
	},
	".".
struct('$SKEL'(top(Id), _), Id-Arg,
       [link(Id-Arg, Id)|LinksT], LinksT) --> !,
	".".
struct('$SKEL'(id(Id2), _), Id-Arg, [link(Id-Arg, Id2)|LinkT], LinkT) --> !,
	".".
struct(Prim, _, Links, Links) -->
	{ primitive(Prim), !,
	  format(codes(Codes), '~q', [Prim])
	},
	cstring(Codes).
struct(Compound, -(Id), Links, LinkT) --> !,
	{ compound_name_arguments(Compound, F, Args),
	  gensym(struct, Id),
	  format(codes(FCodes), '~q', [F])
	},
	"  ", atom(Id),
	" [", "label=\"<f> ", cstring(FCodes), " ",
	gv_args(Args, 0, Id, Links, LinkT), "\"];\n".
struct(Compound, Id-Arg, [link_c(Id-Arg, _, Compound)|LinkT], LinkT) -->
	".".

gv_args([], _, _, Links, Links) --> [].
gv_args([H|T], N, Id, Links, LinksT) -->
	"|", gv_arg_id(N), " ",
	struct(H, Id-N, Links, LT0),
	{N2 is N + 1},
	gv_args(T, N2, Id, LT0, LinksT).

gv_arg_id(N) -->
	"<a", integer(N), ">".

links(Links) -->
	{ \+ memberchk(link_c(_,_,_), Links)
	}, !,
	"\n",
	link_f(Links).
links(Links) -->
	link_c(Links, RestLinks, []),
	links(RestLinks).

link_c([], Links, Links) --> [].
link_c([link_c(Id-Arg, Id2, Compound)|T0],
       [link(Id-Arg, Id2)|LinksT0], LinkT) --> !,
	struct(Compound, -(Id2), LinksT0, LinkT1),
	link_c(T0, LinkT1, LinkT).
link_c([H|T0], [H|T], Links) -->
	link_c(T0, T, Links).

link_f([]) --> [].
link_f([link(Id-Arg, Id2)|T]) -->
	"  ", atom(Id), ":a", integer(Arg), " -> ", atom(Id2), ":f;\n",
	link_f(T).


primitive('$VAR'(_)) :- !.
primitive(X) :-
	\+ compound(X).

%%	cstring(+Codes)//
%
%	Create a C-string. Normally =dot=  appears   to  be  using UTF-8
%	encoding. Would there be a  safer   way  to  transport non-ascii
%	characters, such as \uXXXX?

cstring([]) -->
	[].
cstring([H|T]) -->
	(   cchar(H)
	->  []
	;   [H]
	),
	cstring(T).

cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".
cchar(0'|) --> "\\|".
cchar(0'[) --> "\\[".
cchar(0']) --> "\\]".
