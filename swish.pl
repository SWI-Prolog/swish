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

:- module(swish_app,
	  [
	  ]).
:- use_module(library(pengines)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(lib/config, []).
:- use_module(lib/page, []).
:- use_module(lib/storage).
:- use_module(lib/include).
:- use_module(lib/csv).
:- use_module(lib/examples).
:- use_module(lib/highlight).
:- use_module(lib/markdown).
:- use_module(lib/template_hint, []).


		 /*******************************
		 *	       PATHS		*
		 *******************************/

user:file_search_path(swish_web, swish(web)).
user:file_search_path(js,        swish_web(js)).
user:file_search_path(css,       swish_web(css)).
user:file_search_path(icons,     swish_web(icons)).

set_swish_path :-
	absolute_file_name(swish('swish.pl'), _,
			   [file_errors(fail), access(read)]), !.
set_swish_path :-
	prolog_load_context(directory, Dir),
	asserta(user:file_search_path(swish, Dir)).

:- set_swish_path.

http:location(swish, root(.), [priority(-100)]).


		 /*******************************
		 *	       CORS		*
		 *******************************/

% By default, enable CORS

:- set_setting_default(http:cors, [*]).


		 /*******************************
		 *	      CONFIG		*
		 *******************************/

:- multifile
	swish_config:config/2,
	swish_config:source_alias/2.

%%	swish_config:config(?Config, ?Value) is nondet.
%
%	All solutions of this predicate are  available in the JavaScript
%	object config.swish.config. Config must be an  atom that is also
%	a valid JavaScript identifier. Value  must   be  a value that is
%	valid for json_write_dict/2. Most configurations  are also saved
%	in the application preferences. These   are  marked [P]. Defined
%	config parameters:
%
%	  - show_beware
%	  [P] If `true`, show the *Beware* modal dialog on startup
%	  - tabled_results
%	  [P] If `true`, check the _table results_ checkbox by default.
%	  - application
%	  Name of the Pengine application.
%	  - csv_formats
%	  [P] CSV output formats offered. For example, ClioPatria
%	  defines this as [rdf,prolog]. The first element is default.
%	  - community_examples
%	  Allow marking saved programs as example.  If marked, the
%	  programs are added to the Examples menu.
%	  - public_access
%	  If lib/authenticate.pl is loaded and this flag is `true`,
%	  _all_ access to SWISH demands authentication.  If false,
%	  only running queries and saving files is restricted. Note
%	  that this flag has no effect if no authentication module is
%	  loaded.

swish_config:config(show_beware,        true).
swish_config:config(tabled_results,     false).
swish_config:config(application,        swish).
swish_config:config(csv_formats,        [prolog]).
swish_config:config(community_examples, false).
swish_config:config(public_access,      false).

%%	swish_config:source_alias(Alias, Options) is nondet.
%
%	Specify access for files below a given _alias_. Options define
%
%	  - access(Access)
%	  One of `read` or `both`.  Default is `read`.
%	  - if(Condition)
%	  Provide additional conditions.  Defined conditions are:
%	    - loaded
%	    Only provide access to the file if it is loaded.


                 /*******************************
                 *   CREATE SWISH APPLICATION   *
                 *******************************/

:- multifile
	pengines:prepare_module/3.

:- pengine_application(swish).
:- use_module(swish:lib/render).
:- use_module(swish:lib/trace).
:- use_module(swish:lib/jquery).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).

% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.

:- use_module(library(clpfd), []).
:- use_module(library(clpb), []).

% load rendering modules

:- use_module(swish(lib/render/sudoku),	  []).
:- use_module(swish(lib/render/chess),	  []).
:- use_module(swish(lib/render/table),	  []).
:- use_module(swish(lib/render/codes),	  []).
:- use_module(swish(lib/render/svgtree),  []).
:- use_module(swish(lib/render/graphviz), []).
:- use_module(swish(lib/render/c3),	  []).
