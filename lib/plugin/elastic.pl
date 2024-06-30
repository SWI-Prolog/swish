/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2024, SWI-Prolog Solutions b.v.
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

:- module(elastic,
          [ es_get/2,                   % +Path, -JSON
            es_put/2,
            es_post/2,                  % +Path, +Data
            es_post/3,                  % +Path, +Data, -Result
            es_create_index/2,
            es_delete_index/1,
            es_add/4,                   % +Index, +Doc, -Reply, +Options
            es_search/3
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(uri)).

:- multifile
    es_server/2.                        % URL, Options

/** <module> Core Elastic search interface

This module is a thin layer over  the Elastic REST interface. Eventually
this should go into a pack.
*/

%!  es_get(+Path, -Result) is det.
%!  es_put(+Path, +Data) is det.
%!  es_post(+Path, +Data) is det.
%!  es_post(+Path, +Data, -Result) is det.
%
%   Low level access to the Elastic HTTP service. Path is either an atom
%   or a /-separated segment list, e.g., `Index/'_doc'`.
%
%   @arg Result is a JSON term represented as a dict.

es_get(Path, Result) :-
    es_url(Path, URL, Options),
    http_open(URL, In,
              [ status_code(Status)
              | Options
              ]),
    call_cleanup(reply(Status, In, Result), close(In)).

es_put(Path, Data) :-
    es_url(Path, URL, Options),
    http_open(URL, In, [ method(put),
                         post(json(Data)),
                         status_code(Status)
                       | Options
                       ]),
    call_cleanup(ok_reply(Status, In), close(In)).

es_post(Path, Data) :-
    es_url(Path, URL, Options),
    http_open(URL, In, [ post(json(Data)),
                         status_code(Status)
                       | Options
                       ]),
    call_cleanup(ok_reply(Status, In), close(In)).

es_post(Path, Data, Result) :-
    es_url(Path, URL, Options),
    http_open(URL, In, [ post(json(Data)),
                         status_code(Status)
                       | Options
                       ]),
    call_cleanup(reply(Status, In, Result), close(In)).

%!  es_add(+Index, +Data, -Reply, +Options) is det.
%
%   Add a document to  the  index  Index.   Data  is  a  Prolog  dict to
%   represent the document. This must  satisfy   the  type as defined by
%   es_create_index/2.

es_add(Index, Data, Reply, Options) :-
    option(id(Id), Options),
    !,
    es_post(Index/'_doc'/Id, Data, Reply).
es_add(Index, Data, Reply, _Options) :-
    es_post(Index/'_doc', Data, Reply).

%!  es_search(+Index, +Query, -Result) is det.
%
%   Search using Query, returning a list   of  Prolog dicts that express
%   the result.

es_search(Index, For, Result) :-
    es_post(Index/'_search', For, Result).

%!  es_create_index(+Index, +Mapping) is det.
%!  es_delete_index(+Index) is det.
%
%   Create or delete an index. Note that  deleting an index also deletes
%   all its data.

es_create_index(Index, Mapping) :-
    es_put(Index,
           #{mappings:
             #{properties:Mapping}
            }).

es_delete_index(Index) :-
    es_url(Index, URL, Options),
    http_open(URL, In, [ method(delete),
                         status_code(Status)
                       | Options
                       ]),
    call_cleanup(ok_reply(Status, In), close(In)).

%!  es_url(+Path, -URL, -Options) is det.
%
%   Etablish the Elastic URL from Path and the options for connecting as
%   required  by  http_open/3.  The  latter    typically   contains  TLS
%   authentication if this is used.

es_url(Path, URL, [connection('Keep-alive')|Options]) :-
    es_server(URL0, Options),
    path_segments(Path1, Path),
    uri_edit(path(Path1), URL0, URL).

:- public tls_verify/5.
tls_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, verified) :-
    !.
tls_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, hostname_mismatch) :-
    !.
tls_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
    fail.

%!  ok_reply(+Status, +Stream) is det.
%!  reply(+Status, +Stream, -Reply) is det.
%
%   Handle the HTTP reply. Either succeed (2xx for ok_reply/2), return a
%   Prolog dict with the JSON  reply  (2xx,   for  reply/3)  or raise an
%   exception holding with a Prolog dict holding the details.

ok_reply(Status, _In) :-
    between(200, 299, Status),
    !.
ok_reply(Status, In) :-
    json_read_dict(In, Result, []),
    print_message(error, es_error(Status, Result)).

reply(Status, In, Reply) :-
    between(200, 299, Status),
    !,
    json_read_dict(In, Reply, []).
reply(Status, In, null) :-
    json_read_dict(In, Result, []),
    throw(error(es_error(Status, Result), _)).

%!  path_segments(?Path, ?Segments) is det.
%
%   Convert between `a/b/c/...` and `'a/b/c/...'`

path_segments(Path, Segments) :-
    nonvar(Path),
    !,
    atomic_list_concat(Segments, /, Path).
path_segments(Path, Path) :-
    atomic(Path),
    !.
path_segments(Path, Segments) :-
    phrase(segments(Segments), List),
    atomic_list_concat(List, /, Path).

segments(A/B) -->
    !,
    segments(A),
    segments(B).
segments(A) -->
    [A].

		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1,
    prolog:error_message//1.

prolog:error_message(es_error(Code, Message)) -->
    [ 'Elastic search: got reply ~w:'-[Code], nl,
      '   ~@'-[print_term(Message, [output(current_output)])],
      nl
    ].

