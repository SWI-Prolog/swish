/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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
:- use_module(lib/swish_csv).
:- use_module(lib/examples).
:- use_module(lib/profiles).
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
%	  - ping
%	  Ping pengine status every N seconds.  Updates sparkline
%	  chart with stack usage.
%	  - nb_eval_script
%	  Evaluate scripts in HTML cells of notebooks?

% Allow other code to overrule the defaults from this file.
term_expansion(swish_config:config(Config, _Value), []) :-
	clause(swish_config:config(Config, _), _).

swish_config:config(show_beware,        true).
swish_config:config(tabled_results,     false).
swish_config:config(application,        swish).
swish_config:config(csv_formats,        [prolog]).
swish_config:config(community_examples, false).
swish_config:config(public_access,      false).
swish_config:config(ping,		10).
swish_config:config(notebook,		_{eval_script: true}).

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
:- use_module(swish:lib/swish_debug).
:- use_module(swish:library(pengines_io)).
:- use_module(swish:library(solution_sequences)).
:- use_module(swish:library(aggregate)).
:- if(exists_source(library(tabling))).
:- use_module(swish:library(tabling)).
:- endif.

pengines:prepare_module(Module, swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).

% Additional sandboxing rules.
:- use_module(lib/flags).

% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.

:- use_module(library(clpfd), []).
:- use_module(library(clpb), []).
:- use_module(lib/swish_chr, []).

% load rendering modules

:- use_module(swish(lib/render/sudoku),	  []).
:- use_module(swish(lib/render/chess),	  []).
:- use_module(swish(lib/render/table),	  []).
:- use_module(swish(lib/render/codes),	  []).
:- use_module(swish(lib/render/svgtree),  []).
:- use_module(swish(lib/render/graphviz), []).
:- use_module(swish(lib/render/c3),	  []).
:- use_module(swish(lib/render/url),	  []).
:- use_module(swish(lib/render/bdd),	  []).
