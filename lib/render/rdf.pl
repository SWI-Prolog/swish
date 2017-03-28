/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam

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

:- module(swish_render_rdf,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(semweb/rdf11)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(broadcast)).

:- use_module('../render').

:- register_renderer(rdf, "Render RDF terms").

/** <module> SWISH RDF renderer

Render RDF data. This turns URIs into  links to the ClioPatria localview
and prints literal values using Turtle syntax.   Lists  of RDF terms are
rendered as an inline block with one   element  per line. Long lists are
truncated to the `max_depth` option  of the `answer_write_options` flag.
This can be overruled using e.g.,

    :- use_rendering(rdf, [max_length(100)]).
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders Term as  a  uri  or   Turtle  syntax  literal.  List are
%	rendered as a list of these.

term_rendering(List, _Vars, Options) -->
	{ is_list(List), List \== [],
	  maplist(is_rdf, List),
	  truncate(List, Truncated, Options)
	},
	html(span([class('rdf-list'), style('display:inline-block')],
		  \rdf_list(Truncated, Options))).
term_rendering(Term, _Vars, Options) -->
	{ is_rdf(Term) }, !,
	rdf_link(Term, [target('rdf-link')|Options]).

rdf_list([], _) --> [].
rdf_list([H|T], Options) -->
	html(div(class('rdf-list-element'),
		 \rdf_list_elem(H, Options))),
	rdf_list(T, Options).


rdf_list_elem(skipped(Skipped), _) --> !,
	html(span(class('rdf-list-skipped'),
		  '... skipped ~D ...'-[Skipped])).
rdf_list_elem(Elem, Options) -->
	rdf_link(Elem, [target('rdf-link')|Options]).

truncate(List, Truncated, Options) :-
	(   option(max_length(Max), Options)
	->  true
	;   current_prolog_flag(answer_write_options, WrOptions),
	    option(max_depth(Max), WrOptions)
	),
	length(List, Len),
	Len + 1 > Max,
	Start is Max - 1,
	Skipped is Len-Max,
	length(Prefix, Start),
	append(Prefix, _, List),
	last(List, Last),
	append(Prefix, [skipped(Skipped), Last], Truncated).
truncate(List, List, _).

%%	is_rdf(@Term)
%
%	True if Term is an RDF term.

is_rdf(Term) :-
	is_uri(Term), !.
is_rdf(literal(Value)) :-
	is_literal(Value).
is_rdf(^^(Value,Type)) :- atom(Type), ground(Value).
is_rdf(@(Text,Lang)) :- atom(Lang), is_text(Text).

is_uri(Term) :-
	atom(Term),
	(   uri_is_global(Term)
	->  true
	;   rdf_is_bnode(Term)
	).

is_literal(Atomic) :- is_plain_literal(Atomic).
is_literal(type(Type, Literal)) :- is_uri(Type), is_plain_literal(Literal).
is_literal(lang(Lang, Literal)) :- atom(Lang),   is_text(Literal).

is_plain_literal(Value) :-
	atomic(Value).

is_text(Value) :- atom(Value), !.
is_text(Value) :- string(Value).

%!	rdf_link(+Term, +Options)//

rdf_link(Text@Lang, _Options) --> !,
	html(span(class('rdf-lang-literal'),
		  [ '"', span(class('rdf-lexical'), Text), '"@',
		    span(class('rdf-lang'), Lang)
		  ])).
rdf_link(Value^^Type, Options) --> !,
	{ rdf_lexical_form(Value^^Type, String^^Type)
	},
	html(span(class('rdf-typed-literal'),
		  [ '"', span(class('rdf-lexical'), String), '"^^',
		    \rdf_link(Type, Options)
		  ])).
rdf_link(IRI, Options) -->
	{ rdf_global_id(Prefix:Local, IRI),
	  !,
	  a_options(IRI, Extra, Options)
	},
	html(a([href(IRI)|Extra],
	       [ span(class('rdf-pname'), Prefix), :,
		 span(class('rdf-local'), Local)
	       ])).
rdf_link(IRI, Options) -->
	{ must_be(atom, IRI),
	  a_options(IRI, Extra, Options)
	},
	html(a([href(IRI)|Extra],
	       [ span(class('rdf-iri'), IRI)
	       ])).


a_options(IRI, Extra, Options) :-
	(   broadcast_request(rdf_label(IRI, Label))
	->  Extra = [title(Label)|E1]
	;   E1 = Extra
	),
	(   option(target(Target), Options)
	->  E1 = [target(Target)]
	;   E1 = []
	).

