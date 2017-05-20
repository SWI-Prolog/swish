/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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

:- module(swish_render_table,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(dicts)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module('../render').

:- register_renderer(table, "Render data as tables").

/** <module> SWISH table renderer

Render table-like data.
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders Term as  a  table.   This  renderer  recognises  several
%	representations of table-like data:
%
%	  $ A list of terms of equal arity :
%	  $ A list of lists of equal length :
%
%	@tbd: recognise more formats

term_rendering(Term, _Vars, Options) -->
	{ is_list_of_dicts(Term, _Rows, ColNames), !,
	  partition(is_header, Options, _HeaderOptions, Options1)
	}, !,
	html(div([ style('display:inline-block'),
		   'data-render'('List of terms as a table')
		 ],
		 [ table(class('render-table'),
			 [ \header_row(ColNames),
			   \rows(Term, Options1)
			 ])
		 ])).
term_rendering(Term, _Vars, Options) -->
	{ is_list_of_terms(Term, _Rows, _Cols),
	  header(Term, Header, Options, Options1)
	}, !,
	html(div([ style('display:inline-block'),
		   'data-render'('List of terms as a table')
		 ],
		 [ table(class('render-table'),
			 [ \header_row(Header),
			   \rows(Term, Options1)
			 ])
		 ])).
term_rendering(Term, _Vars, Options) -->
	{ is_list_of_lists(Term, _Rows, _Cols),
	  header(Term, Header, Options, Options1)
	}, !,
	html(div([ style('display:inline-block'),
		   'data-render'('List of lists as a table')
		 ],
		 [ table(class('render-table'),
			 [ \header_row(Header),
			   \rows(Term, Options1)
			 ])
		 ])).

rows([], _) --> [].
rows([H|T], Options) -->
	{ cells(H, Cells) },
	html(tr(\row(Cells, Options))),
	rows(T, Options).

row([], _) --> [].
row([H|T], Options) -->
	html(td(\term(H, Options))),
	row(T, Options).
row([H|T], Options) -->
	html(td(\term(H, []))),
	row(T, Options).

cells(Row, Cells) :-
	is_list(Row), !,
	Cells = Row.
cells(Row, Cells) :-
	is_dict(Row), !,
	dict_pairs(Row, _Tag, Pairs),
	pairs_values(Pairs, Cells).
cells(Row, Cells) :-
	compound(Row),
	compound_name_arguments(Row, _, Cells).

%%	header(+Table, -Header:list(Term), +Options, -RestOptions) is semidet.
%
%	Compute the header to use. Fails if   a  header is specified but
%	does not match.

header(_, _, Options0, Options) :-
	\+ option(header(_), Options0), !,
	Options = Options0.
header([Row|_], ColHead, Options0, Options) :-
	partition(is_header, Options0, HeaderOptions, Options),
	member(HeaderOption, HeaderOptions),
	header(HeaderOption, Header),
	generalise(Row, GRow),
	generalise(Header, GRow), !,
	header_list(Header, ColHead).

is_header(0) :- !, fail.
is_header(header(_)).
is_header(header=_).

header(header(H), H).
header(header=H, H).

generalise(List, VList) :-
	is_list(List), !,
	length(List, Len),
	length(VList0, Len),
	VList = VList0.
generalise(Compound, VCompound) :-
	compound(Compound), !,
	compound_name_arity(Compound, Name, Arity),
	compound_name_arity(VCompound0, Name, Arity),
	VCompound = VCompound0.

header_list(List, List) :- is_list(List), !.
header_list(Compound, List) :-
	Compound =.. [_|List].


%%	header_row(ColNames:list)// is det.
%
%	Include a header row  if ColNames is not unbound.

header_row(ColNames) -->
	{ var(ColNames) }, !.
header_row(ColNames) -->
	html(tr(class(hrow), \header_columns(ColNames))).

header_columns([]) --> [].
header_columns([H|T]) -->
	html(th(\term(H, []))),
	header_columns(T).


%%	is_list_of_terms(@Term, -Rows, -Cols) is semidet.
%
%	Recognises a list of terms with   the  same functor and non-zero
%	ariry.

is_list_of_terms(Term, Rows, Cols) :-
	is_list(Term), Term \== [],
	length(Term, Rows),
	maplist(is_term_row(_Name, Cols), Term),
	Cols > 0.

is_term_row(Name, Arity, Term) :-
	compound(Term),
	compound_name_arity(Term, Name, Arity).

%%	is_list_of_dicts(@Term, -Rows, -ColNames) is semidet.
%
%	True when Term is a list of Rows dicts, each holding ColNames as
%	keys.

is_list_of_dicts(Term, Rows, ColNames) :-
	is_list(Term), Term \== [],
	length(Term, Rows),
	maplist(is_dict_row(ColNames), Term).

is_dict_row(ColNames, Dict) :-
	is_dict(Dict),
	dict_keys(Dict, ColNames).

%%	is_list_of_lists(@Term, -Rows, -Cols) is semidet.
%
%	Recognise a list of lists of equal length.

is_list_of_lists(Term, Rows, Cols) :-
	is_list(Term), Term \== [],
	length(Term, Rows),
	maplist(is_list_row(Cols), Term),
	Cols > 0.

is_list_row(Length, Term) :-
	is_list(Term),
	length(Term, Length).

