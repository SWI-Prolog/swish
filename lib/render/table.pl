/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(swish_render_table,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(apply)).
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
%	@tbd: recogniser more formats, provide options to specify the
%	header, etc.

term_rendering(Term, _Vars, Options) -->
	{ is_list_of_terms(Term, _Rows, Cols)
	}, !,
	html(div([ style('display:inline-block'),
		   'data-render'('List of terms as a table')
		 ],
		 [ table(class('render-table'),
			 [ \header(Cols, Options),
			   \rows(Term)
			 ])
		 ])).
term_rendering(Term, _Vars, Options) -->
	{ is_list_of_lists(Term, _Rows, Cols)
	}, !,
	html(div([ style('display:inline-block'),
		   'data-render'('List of lists as a table')
		 ],
		 [ table(class('render-table'),
			 [ \header(Cols, Options),
			   \rows(Term)
			 ])
		 ])).

rows([]) --> [].
rows([H|T]) -->
	{ cells(H, Cells) },
	html(tr(\row(Cells))),
	rows(T).

row([]) --> [].
row([H|T]) -->
	html(td(\term(H, []))),
	row(T).

cells(Row, Cells) :-
	is_list(Row), !,
	Cells = Row.
cells(Row, Cells) :-
	compound(Row),
	compound_name_arguments(Row, _, Cells).

%%	header(+NCols, +Options)// is det.
%
%	Include a header row  if   an  option header(+ListOfColNames) is
%	present, whose length matches NCols.

header(Cols, Options) -->
	{ option(header(ColNames), Options),
	  length(ColNames, Cols)
	},
	html(tr(class(hrow), \header_row(ColNames))).
header(_, _) --> [].

header_row([]) --> [].
header_row([H|T]) -->
	html(th(\term(H, []))),
	header_row(T).


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

