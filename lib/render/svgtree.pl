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
:- use_module(library(http/js_write)).
:- use_module('../render').

:- register_renderer(svgtree, "Render term as a tree").

/** <module> SWISH SVG tree renderer

Render a term as an SVG tree
*/

%%	term_rendering(+Term, +Vars, +Options)//
%

term_rendering(Term, _Vars, _Options) -->
	{ is_term_tree(Term, How),
	  call(How, Term, Dict),
          gensym('svg-tree-', Id)
	},
	html(div([ class('render-svg-tree'),
		   'data-render'('Term as SVG tree')
		 ],
		 [ span(id(Id), []),
		   \js_script({|javascript(Id, Dict)||
require(["render/svg-tree-drawer", "jquery"], function(svgtree) {

  var tree = new TreeDrawer(
		     document.getElementById(Id),
		     Dict);
  tree.draw();
});
		  |})
		 ])).

is_term_tree(Term, compound_tree) :-
	compound(Term),
	acyclic_term(Term), !.

:- public
	compound_tree/2.

compound_tree(Term, Simple) :-
	\+ compound(Term), !,
	term_string(Term, Label),
	Simple = json{label:Label}.
compound_tree(Term, json{label: #(Label), children:Children}) :-
	compound_name_arguments(Term, Label, Args),
	maplist(compound_tree, Args, Children).
