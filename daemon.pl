#!/usr/bin/env swipl

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Run

    ./daemon.pl --help for help
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(http/http_unix_daemon)).

swish_daemon.

user:file_search_path(swish, SwishDir) :-
	source_file(swish_daemon, ThisFile),
	file_directory_name(ThisFile, SwishDir).

:- initialization http_daemon.

:- use_module(swish(lib/ssl_certificate)).
:- [swish(swish)].
:- use_module(swish:swish(lib/swish_debug)).
