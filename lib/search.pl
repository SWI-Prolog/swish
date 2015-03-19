/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(swish_search,
	  [ search_box//1		% +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(config).

:- multifile
	typeahead/3.			% +Set, +Query, -Match

/** <module> SWISH search from the navigation bar

This library supports both typeahead of the   search  box and the actual
search from the server side. What do we want to search for?

  - Predicates (built-in, library)
    - How should we handle documentation?  PlDoc?  Manual?
  - Source files (name, tags, meta-data, content?)
    - Show matching sources in modal dialog and allow switching to
      these?
*/

:- http_handler(swish(typeahead), typeahead, [id(swish_typeahead)]).
:- http_handler(swish(search),    search,    [id(swish_search)]).

%%	search_box(+Options)//
%
%	Render a Bootstrap search box.

search_box(_Options) -->
	html(form([class('navbar-form'), role(search)],
		  div(class('input-group'),
		      [ input([ type(text),
				class('form-control'),
				placeholder('Search'),
				title('Searches code, documentation and files'),
				id('search')
			      ]),
			div(class('input-group-btn'),
			    button([ class([btn, 'btn-default']),
				     type(submit)],
				   i(class([glyphicon, 'glyphicon-search']),
				     [])))
		      ]))).


%%	typeahead(+Request)
%
%	Support the search typeahead widget. The  handler returns a JSON
%	array of matches. Each match is an object that contains at least
%	a label.

typeahead(Request) :-
	http_parameters(Request,
			[ q(Query, [default('')]),
			  set(Set, [default(predicates)])
			]),
	findall(Match, typeahead(Set, Query, Match), Matches),
	reply_json_dict(Matches).

typeahead(predicates, Query, Template) :-
	swish_config(templates, Templates),
	member(Template, Templates),
	_{name:Name, arity:_} :< Template,
	sub_atom(Name, 0, _, _, Query).

%%	search(+Request)
%
%	Handle an actual search  request  from   the  SWISH  search box.
%	Returns an HTML  document  with  the   actual  results  that  is
%	displayed in a modal dialog.

search(_Request) :-
	reply_html_page(search,
			[],
			h1('Search results')).
