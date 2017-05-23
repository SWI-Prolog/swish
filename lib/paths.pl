/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
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

:- module(swish_paths, []).
:- use_module(library(http/http_path), []).

/** <module> Setup SWISH search paths
*/

:- initialization initialize_paths.

:- multifile
    user:file_search_path/2,
    http:location/3.

user:file_search_path(data,           data).
user:file_search_path(config_enabled, 'config-enabled').
user:file_search_path(config_enabled, swish('config-enabled')).
user:file_search_path(config,         config_enabled(.)).
user:file_search_path(config,         swish('config-available')).
user:file_search_path(swish_web,      swish(web)).
user:file_search_path(swish_pack,     swish(pack)).
user:file_search_path(js,             swish_web(js)).
user:file_search_path(css,            swish_web(css)).
user:file_search_path(icons,          swish_web(icons)).

%!  set_swish_path
%
%   Setup the swish search path.

set_swish_path :-
    absolute_file_name(swish('swish.pl'), _,
                       [file_errors(fail), access(read)]), !.
set_swish_path :-
    prolog_load_context(directory, Dir),
    asserta(user:file_search_path(swish, Dir)).

%!  attach_local_packs
%
%   Attach pack submodules from swish(pack)

attach_local_packs :-
    attach_packs(swish_pack(.), [duplicate(replace), search(first)]).

%!  set_data_path
%
%   Setup and possibly create a directory for storing dynamic data.

set_data_path :-
    absolute_file_name(data(.), _,
                       [ file_type(directory),
                         access(write),
                         file_errors(fail)
                       ]), !.
set_data_path :-
    absolute_file_name(data(.), Dir,
                       [ solutions(all)
                       ]),
    \+ exists_directory(Dir),
    catch(make_directory(Dir),
          error(permission_error(create,directory,Dir), _),
          fail), !,
    print_message(informational, swish(created_data_dir(Dir))).
set_data_path :-
    print_message(error, swish(no_data_dir)),
    halt(1).

initialize_paths :-
    set_swish_path,
    attach_local_packs,
    set_data_path.

% HTTP paths

http:location(swish, root(.), [priority(-100)]).



