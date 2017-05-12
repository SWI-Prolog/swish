/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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

:- module(swish_ide,
	  [ swish/0,
	    swish/1			% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(www_browser)).
:- if(exists_source(library(uid))).
:- use_module(library(uid)).
:- endif.

/** <module>

Open SWISH as an IDE for developing a local application.
*/

		 /*******************************
		 *	       CONFIG		*
		 *******************************/

:- multifile
	swish_config:config/2,			% Name, Value
	swish_config:source_alias/2,		% Alias, Options
	swish_config:verify_write_access/3,	% Request, File, Options
	pengines:authentication_hook/3,		% Request, Application, User
	pengines:not_sandboxed/2,		% User, Application
	user:file_search_path/2.		% Alias, Path

user:file_search_path(project, '.').

swish_config:config(show_beware,        false).
swish_config:config(community_examples, true).

swish_config:source_alias(project, [access(both), search('*.pl')]).
swish_config:source_alias(library, []).

swish_config:verify_write_access(_Request, _File, _Options).

pengines:authentication_hook(_Request, swish, User) :-
	current_user(User).
pengines:not_sandboxed(_User, _Application).

:- if(current_predicate(getuid/1)).
current_user(User) :- !,
	getuid(UID),
	user_info(UID, Info),
	user_data(name, Info, User).
:- endif.
current_user(default).


:- use_module(swish).

%%	swish
%
%	Start the SWISH server and open the main page in your browser.

swish :-
	swish(localhost:_Port).

swish(Port) :-
	http_server_property(Port, goal(swish_ide:http_dispatch)), !,
	open_browser(Port).
swish(_:Port) :-
	integer(Port),
	http_server_property(Port, goal(swish_ide:http_dispatch)), !,
	open_browser(Port).
swish(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]),
	open_browser(Port).

open_browser(Address) :-
	host_port(Address, Host, Port),
	http_server_property(Port, scheme(Scheme)),
	http_absolute_location(root(.), Path, []),
	format(atom(URL), '~w://~w:~w~w', [Scheme, Host, Port, Path]),
	www_open_url(URL).

host_port(Host:Port, Host, Port) :- !.
host_port(Port, localhost, Port).


