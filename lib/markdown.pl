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

:- module(swish_markdown, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(pldoc/doc_html),
	      except([ file//2
		     ])).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(option)).
:- use_module(library(filesex)).

:- use_module(storage).
:- use_module(config).

/** <module> SWISH Notebook markdown support

This module translates markdown cells for teh SWISH Notebook into HTML
*/

:- http_handler(swish(markdown), markdown, [id(markdown)]).

%%	markdown(+Request)
%
%	Translate a Markdown text for the   SWISH  Notebook into an HTML
%	document.

markdown(Request) :-
        http_parameters(Request,
                        [ text(Data, [optional(true), default('')])
                        ]),
        atom_codes(Data, Codes),
        wiki_file_codes_to_dom(Codes, '/', DOM), % FIXME: What file to pass?
        phrase(html(DOM), Tokens),
        format('Content-type: text/html; charset=UTF-8\n\n'),
        print_html(Tokens).

%%      wiki_codes_to_dom(+Codes, +File, -DOM)
%
%       DOM is the HTML dom representation for Codes that originate from
%       File.

wiki_file_codes_to_dom(String, File, DOM) :-
        (   nb_current(pldoc_file, OrgFile)
        ->  setup_call_cleanup(
                b_setval(pldoc_file, File),
                wiki_codes_to_dom(String, [], DOM),
                b_setval(pldoc_file, OrgFile))
        ;   setup_call_cleanup(
                b_setval(pldoc_file, File),
                wiki_codes_to_dom(String, [], DOM),
                nb_delete(pldoc_file))
        ).


		 /*******************************
		 *	     HOOK WIKI		*
		 *******************************/

:- multifile
	prolog:doc_autolink_extension/2.

prolog:doc_autolink_extension(swinb, notebook).

:- public
	file//2.

%%	file(+File, +Options)//
%
%	Hook that deals with linking other notebooks using the following
%	markdown syntax:
%
%	  ```
%	  - [My first book](mybook.swinb)
%	  - [Label](store.pl)
%	  - [Label](library/lists.pl)
%	  ```

:- multifile
	swish_config:source_alias/2.

file(File, Options) -->
	{ once(sub_atom(File, Pre, _, _Post, /)),
	  sub_atom(File, 0, Pre, _, Alias),
	  swish_config:source_alias(Alias, _Options),
	  option(label(Label), Options),
	  http_location_by_id(swish, Swish),
	  directory_file_path(Swish, File, HREF)
	}, !,
	html(a([class([alias,file]), href(HREF)], Label)).
file(File, Options) -->
	{ storage_file(File),
	  option(label(Label), Options),
	  http_location_by_id(swish, Swish),
	  directory_file_path(Swish, p, StoreDir),
	  directory_file_path(StoreDir, File, HREF)
	}, !,
	html(a([class(store), href(HREF)], Label)).
file(File, Options) -->
	pldoc_html:file(File, Options).

