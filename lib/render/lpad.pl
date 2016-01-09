/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

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

:- module(swish_render_lpad,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ,clauses//1
	  ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(slipcover)).
:- use_module('../render').

:- register_renderer(lpad, "Render a logic program with annotated disjunctions").

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

term_rendering(Term, _Vars, _Options) -->
	{is_list_of_clauses(Term)
	}, !,
	html(pre(\clauses(Term))).

clauses([]) --> [].
clauses([HC|T]) -->
	{
	 numbervars(HC,0,_),
         HC=(H:-B),!,
	 list2or(HL,H),
	 list2and(BL,B)
	},
	disj_clause(HL,BL),
	clauses(T).

disj_clause(H,B)-->
  {format(atom(I),' :-~n',[])},
  head(H,B,I).

head([H:1.0|_Rest],[],_I)-->
  {format(atom(A),"~p.~n~n",[H])},!,
  [A].
 
head([H:1.0|_Rest],B,I)-->
  {format(atom(A),"~p",[H])},!,
  [A,I],
  body(B).

head([H:P,'':_P],[],_I)-->
  {format(atom(A),"~p:~g.~n~n",[H,P])},!,
  [A].

head([H:P,'':_P],B,I)-->
  {format(atom(A),"~p:~g",[H,P])},!,
  [A,I],
  body(B).


head([H:P],true,_I)-->
  {format(atom(A),"~p:~g.~n~n",[H,P])},!,
  [A].

head([H:P],B,I)-->
  {format(atom(A),"~p:~g",[H,P])},!,
  [A,I],
  body(B).

head([H:P|Rest],B,I)-->
  {format(atom(A),"~p:~g ; ",[H,P])},!,
  [A],
  head(Rest,B,I).

body([])-->
  {format(atom(A),"  true.~n~n",[])},
  [A].

body([H])-->
  {format(atom(A),"  ~p.~n~n",[H])},!,
  [A].

body([H|T])-->
  {format(atom(A),"  ~p,~n",[H])},
  [A],body(T).

row([]) --> [].
row([H|T]) -->
	html(td(\term(H, []))),
	row(T).


term_rendering(Term, _Vars, Options) -->
	{ is_list_of_lists(Term, _Rows, _Cols),
	  header(Term, Header, Options)
	}, !,
	html(div([ style('display:inline-block'),
		   'data-render'('List of lists as a table')
		 ],
		 [ table(class('render-table'),
			 [ \header_row(Header),
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

%%	header(+Table, -Header:list(Term), +Options) is semidet.
%
%	Compute the header to use. Fails if   a  header is specified but
%	does not match.

header(_, _, Options) :-
	\+ option(header(_), Options), !.
header([Row|_], ColHead, Options) :-
	member(header(Header), Options),
	generalise(Row, GRow),
	generalise(Header, GRow), !,
	header_list(Header, ColHead).

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

is_list_of_clauses(Term) :-
	is_list(Term), Term \== [],
	maplist(is_clause, Term).

is_clause((_H :- _B)).

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

