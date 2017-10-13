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

:- module(swish_data_scrape, []).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(option)).
:- use_module(library(hash_stream)).
:- use_module(library(apply)).
:- use_module(library(xpath)).
:- use_module(library(sandbox)).
:- use_module(library(pairs)).
:- use_module(library(error)).

:- use_module('../data_source').

/** <module> Scrape data from XML and HTML pages
*/

:- multifile
    swish_data_source:source/2.

swish_data_source:source(
    scrape(URL, Proj, DOM, Convert, Options),
    swish_data_scrape:scrape(URL, Proj, DOM, Convert, Options)).

:- public scrape/6.

scrape(URL, Proj, DOM, {Convert}, Options0, Hash) :-
    exclude(safe_option, Options0, UnSafe),
    (   UnSafe == []
    ->  true
    ;   permission_error(use, scrape_options, UnSafe)
    ),
    default_options(URL, DefOptions),
    merge_options(Options0, DefOptions, Options),
    load_page(URL, DOM, Version, Options), !,
    safe_goal(Convert),
    dict_pairs(Proj, _, Pairs),
    pairs_keys_values(Pairs, Keys, Values),
    Signature =.. [Hash|Keys],
    Head =.. [Hash|Values],
    forall(call(Convert),
           'data assert'(Head)),
    'data materialized'(Hash, Signature, Version).
scrape(URL, Proj, DOM, Convert, Options, _Hash) :-
    throw(error(failure_error(scrape(URL, Proj, DOM, Convert, Options)))).

load_page(URL, DOM, Version, Options) :-
    setup_call_catcher_cleanup(
        ( http_open(URL, Pair,
                    [ header(last_modified, LastModified),
                      header(etag, Etag)
                    | Options
                    ]),
          stream_pair(Pair, In0, Out),
          close(Out),
          open_hash_stream(In0, In, [algorithm(sha1)])
        ),
        load_structure(In, DOM, Options),
        Catcher,
        finalize(Catcher, In, Etag, LastModified, Version)).

finalize(exit, In, Etag, LastModified, Version) :-
    stream_hash(In, SHA1),
    close(In),
    (   parse_time(LastModified, LastModStamp)
    ->  true
    ;   LastModStamp = (-)
    ),
    source_tag(Etag, LastModStamp, SHA1, Version).

source_tag('',   -,    SHA1, version{sha1:SHA1}) :- !.
source_tag(Etag, -,    SHA1, version{sha1:SHA1, etag:Etag}).
source_tag('',   Time, SHA1, version{sha1:SHA1, last_modified:Time}).
source_tag(Etag, Time, SHA1, version{sha1:SHA1, etag:Etag, last_modified:Time}).

safe_option(dialect(_)).
safe_option(space(_)).
safe_option(dtd(Dialect)) :-
    dialect(Dialect).
safe_option(max_errors(_)).
safe_option(syntax_errors(_)).

dialect(sgml).
dialect(html4).
dialect(html5).
dialect(html).
dialect(xhtml).
dialect(xml).
dialect(xmlns).

default_options(URL, Options) :-
    file_name_extension(_, Ext, URL),
    ext_options(Ext, Options),
    !.
default_options(_URL, Options) :-
    ext_options(html, Options).

ext_options(xml,  [dialect(xml)]).
ext_options(htm,  Options) :-
    ext_options(html, Options).
ext_options(html, [ dtd(DTD),
                    dialect(Dialect),
                    max_errors(-1),
                    syntax_errors(quiet)
                  ]) :-
    Dialect = html,
    dtd(Dialect, DTD).
