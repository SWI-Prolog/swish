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

:- module(swish_svgtree,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).
:- use_module('../render').

:- register_renderer(svgtree, "Render term as a tree").

/** <module> SWISH SVG tree renderer

Render a term as an SVG tree
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Render a compound term as a tree.  Options processed:
%
%	  - list(Boolean)
%	  If `false`, do not render lists.

term_rendering(Term, _Vars, Options) -->
	{ is_term_tree(Term, How, Options),
	  call(How, Term, Dict),
          gensym('svg-tree-', Id)
	},
	html(div([ class('render-svg-tree'),
		   'data-render'('Term as SVG tree')
		 ],
		 [ span(id(Id), []),
		   \js_script({|javascript(Id, Dict)||
require(["render/svg-tree-drawer", "jquery"], function(svgtree) {
  var span = document.getElementById(Id);
  var tree = new TreeDrawer(span, Dict);
  if ( !tree.filters.label ) {
    tree.addFilter('label', function(label,node) {
      return typeof(label) == "object" ? $(label.html)[0] : label;
    });
  }
  tree.draw();
});
		  |})
		 ])).

is_term_tree(Term, compound_tree(Options), Options) :-
	compound(Term),
	(   is_list(Term)
	->  \+ option(list(false), Options)
	;   true
	),
	acyclic_term(Term), !.

:- public
	compound_tree/3.

compound_tree(Options, Term, Tree) :-
	compound(Term), Term \= '$VAR'(_), !,
	Tree = json{label:Label, children:Children},
	compound_name_arguments(Term, Functor, Args),
	term_string(Functor, Label),
	maplist(compound_tree(Options), Args, Children).
compound_tree(Options, Term, Simple) :-
	term_html_string(Term, Label, Options),
	Simple = json{label:json{html:Label}}.

term_html_string(Term, String, Options) :-
	phrase(term(Term, Options), Tokens),
	with_output_to(string(String), print_html(Tokens)).
