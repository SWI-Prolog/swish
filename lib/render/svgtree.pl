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
:- use_module(library(http/http_wrapper)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(sandbox)).
:- use_module('../render').

:- register_renderer(svgtree, "Render term as a tree").

/** <module> SWISH SVG tree renderer

Render a term as an SVG tree.   This  renderer is intended to illustrate
the shape of terms or display a simple parse tree.

This renderer is also an illustration of  using a JavaScript library and
SVG inside rendered elements. Note  that   the  use  of RequireJS avoids
loading the library multiple times as   well  as poluting the namespace.

Note that while the  script  is   being  evaluated,  `$.ajaxScript` is a
jQuery object pointing to the executing script. This is used to find the
`span`  element  without  using  an  `id`    attribute.  Using  `id`  is
undesirable as it is hard  to   guarantee  their uniqueness. However, we
must find the desired  element  immediately   and  not  in the RequireJS
callback, so we need to put it in   a variable and scope the whole thing
in a function to avoid conflicts.  JavaScript is fun!
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Render a compound term as a tree.  Options processed:
%
%	  - list(Boolean)
%	  If `false`, do not render lists.
%	  - filter(:NodeFilter)
%	  If present, use call(NodeFilter, Term, Label, Children)
%	  to extract the label and children of a term.  Operates
%	  on terms for which this call succeeds on the top node.
%	  If the call fails on a child, the child is rendered as
%	  a term.

term_rendering(Term, _Vars, Options) -->
	{ is_term_tree(Term, How, Options),
	  call(How, Term, Dict)
	},
	html(div([ class('render-svg-tree'),
		   'data-render'('Term as SVG tree')
		 ],
		 [ span([]),
		   \js_script({|javascript(Dict)||
(function() {
  var span = $.ajaxScript.parent().find("span")[0];

  require(["render/svg-tree-drawer", "jquery"], function(svgtree) {
    var tree = new TreeDrawer(span, Dict);
    if ( !tree.filters.label ) {
      tree.addFilter('label', function(label,node) {
	return typeof(label) == "object" ? $(label.html)[0] : label;
      });
    }
    tree.draw();
  });
})();
		  |})
		 ])).

%%	is_term_tree(+Term, -Closure, +Options) is semidet.
%
%	True when Term  is  a  Prolog   term  that  can  meaningfully be
%	displayed as a tree. The  actual   rendering  is done by calling
%	call(Closure, Term, JSON), where the  called closure must return
%	a nested dict and each node contains:
%
%	  * label
%	  The label to display.  This is either a string or a dict
%	  containing html:HTMLString
%	  * children
%	  If present, this is a list of child nodes. If not, it is
%	  a leaf node.

is_term_tree(Term, filtered_tree(QFilter, Options1), Options) :-
	option(filter(Filter), Options),
	callable(Filter),
	Filter \= _:_,
	option(module(Module), Options),
	QFilter = Module:Filter,
	catch(safe_filter(QFilter), _, fail),
	call(QFilter, Term, _Label, _Children), !,
	browser_option(Options, Options1).
is_term_tree(Term, compound_tree(Options1), Options) :-
	compound(Term),
	(   is_list(Term)
	->  \+ option(list(false), Options)
	;   true
	), !,
	browser_option(Options, Options1).

:- public
	compound_tree/3,
	filtered_tree/4.

%%	compound_tree(+Options, +Term, -JSON) is det.
%
%	Render Term as a tree, considering every   compound term to be a
%	node.  Renders leafs using term//1.

compound_tree(Options, Term, Tree) :-
	compound(Term), Term \= '$VAR'(_), !,
	Tree = json{label:Label, children:Children},
	compound_name_arguments(Term, Functor, Args),
	term_string(Functor, Label),
	maplist(compound_tree(Options), Args, Children).
compound_tree(Options, Term, json{label:Label}) :-
	term_label(Term, Label, Options).

%%	term_label(+Term, -Label, +Options) is det.
%
%	Create a label for a term.  If   we  can, we generate HTML using
%	term//2, which is translated into   an  SVG `foreignObject`. The
%	Trident engine used by IE does  not support foreignObject though
%	:-(

term_label(Term, String, Options) :-
	option(engine(trident), Options), !,
	term_string(Term, String, Options).
term_label(Term, json{html:String}, Options) :-
	phrase(term(Term, Options), Tokens),
	with_output_to(string(String), print_html(Tokens)).

%%	filtered_tree(:Filter, +Options, +Term, -JSON) is det.
%
%	Render a filtered tree.

:- meta_predicate filtered_tree(3,+,+,-).

filtered_tree(Filter, Options, Term, Tree) :-
	nonvar(Term),
	call(Filter, Term, LabelTerm, ChildNodes),
	is_list(ChildNodes), !,
	Tree = json{label:Label, children:Children},
	term_label(LabelTerm, Label, Options),
	maplist(filtered_tree(Filter, Options), ChildNodes, Children).
filtered_tree(_, Options, Term, json{label:Label}) :-
	term_label(Term, Label, Options).

safe_filter(Module:Filter) :-
	Filter =.. List0,
	append(List0, [_, _, _], List),
	Filter1 =.. List,
	safe_goal(Module:Filter1).


		 /*******************************
		 *	      BROWSER		*
		 *******************************/

browser_option(Options0, Options) :-
	is_trident, !,
	Options = [engine(trident)|Options0].
browser_option(Options, Options).

%%	is_trident is semidet.
%
%	True if we know that the client is Trident-based (IE)

is_trident :-
	http_current_request(Request),
	option(user_agent(Agent), Request),
	sub_string(Agent, _, _, _, " Trident/"), !.

