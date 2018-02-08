/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(swish_markdown, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
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
	option(method(get), Request), !,
        http_parameters(Request,
                        [ text(Data, [optional(true), default('')])
                        ]),
        atom_codes(Data, Codes),
        wiki_file_codes_to_dom(Codes, '/', DOM), % FIXME: What file to pass?
        phrase(html(DOM), Tokens),
        format('Content-type: text/html; charset=UTF-8\n\n'),
        print_html(Tokens).
markdown(Request) :-
	option(method(post), Request), !,
	http_read_data(Request, Codes, [to(codes)]),
	wiki_file_codes_to_dom(Codes, '/', DOM),
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
prolog:doc_autolink_extension(lnk,   permalink).

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
	  option(label(Label), Options, File),
	  http_location_by_id(swish, Swish),
	  directory_file_path(Swish, p, StoreDir),
	  directory_file_path(StoreDir, File, HREF)
	}, !,
	html(a([class(store), href(HREF)], Label)).
file(File, Options) -->
	pldoc_html:file(File, Options).

