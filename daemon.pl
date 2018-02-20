#!/usr/bin/env swipl

:- module(swish_daemon, []).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Run

    ./daemon.pl --help for help
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
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

:- use_module(library(http/http_unix_daemon)).
:- use_module(library(option)).
:- use_module(library(main)).

:- initialization(swish_daemon, main).

swish_daemon :-
	current_prolog_flag(argv, Argv),
	argv_options(Argv, _RestArgv, Options0),
	swish_options(Options0, Options),
	(   option(https(_), Options)
	->  use_module(swish(lib/ssl_certificate))
	;   true
	),
	http_daemon(Options).

swish_options(Options0, Options) :-
	option(https(_), Options0), !,
	add_default_option(certfile, Options0, 'https/server.crt', Options1),
	add_default_option(keyfile,  Options1, 'https/server.key', Options).
swish_options(Options, Options).

add_default_option(Name, Options0, Default, Options) :-
	Term =.. [Name,Value],
	(   option(Term, Options0)
	->  Options = Options0
	;   Value = Default,
	    Options = [Term|Options0]
	).

user:file_search_path(swish, SwishDir) :-
	getenv('SWISH_HOME', SwishDir), !.
user:file_search_path(swish, SwishDir) :-
	source_file(swish_daemon, ThisFile),
	file_directory_name(ThisFile, SwishDir).

:- [swish(swish)].
