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

:- module(swish_config_rdf, []).

/** <module> Configure RDF libraries

Make the RDF libraries  available.  Note   that  by  default the sandbox
allows for querying the database. Modifying  the RDF database requires a
login.
*/

:- if(true).     % use false to make RDF predicates available by default.
:- use_module(library(semweb/rdf_db), []).
:- use_module(library(semweb/rdf11), []).
:- else.
%:- use_module(swish:library(semweb/rdf_db)).
:- use_module(swish:library(semweb/rdf11)).
:- endif.

% Allow loading data in various formats and from HTTP
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/turtle)).

:- use_module(library(semweb/rdf_sandbox)).
:- use_module(library(broadcast)).


		 /*******************************
		 *         PERSISTENCY		*
		 *******************************/

:- use_module(library(semweb/rdf_persistency)).

attach_rdf_data(Spec) :-
    absolute_file_name(Spec, Dir,
                       [ file_type(directory),
                         access(write),
                         file_errors(fail)
                       ]),
    !,
    rdf_attach_db(Dir, []).
attach_rdf_data(Spec) :-
    absolute_file_name(Spec, Dir,
			   [ solutions(all)
			   ]),
    \+ exists_directory(Dir),
    catch(make_directory(Dir),
          error(permission_error(create, directory, Dir), _),
          fail),
    !,
    rdf_attach_db(Dir, []).

% Comment if you do not want persistent storage.  Use post_server_start
% to load the RDF data after making the server available.
:- listen(http(pre_server_start), attach_rdf_data(data('RDF'))).
