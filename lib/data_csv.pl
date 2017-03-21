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

:- module(data_csv,
          [
          ]).
:- use_module(library(csv)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lazy_lists)).
:- use_module(library(apply)).
:- use_module(library(http/http_open)).

/** <module> Cached CSV data access

This module provides access to CSV  data   by  caching  it into a Prolog
module. The data itself is kept in  a   separate  data  module, so it is
maintained over a SWISH Pengine invocation. It  still needs to be unique
and thus is is  stored  under  a   hash  constructed  from  the  URL and
processing options.
*/

%!  csv_data(+Id, +URL, +Options)
%
%   Make the CSV data in URL available   using  Id. Given this id, dicts
%   with a tag Id are expanded to goals   on  the CSV data. In addition,
%   record(Id, Record) gives a full data record   as a dict. Options are
%   as defined by csv//2.  In addition, these options are processed:
%
%     - encoding(+Encoding)
%       Set the encoding for processing the data.  Default is guessed
%       from the URL header or `utf8` if there is no clue.
%     - columns(+ColumnNames)
%       Names for the columns. If not provided, the first row is assumed
%       to hold the column names.


		 /*******************************
		 *            STORE		*
		 *******************************/

:- dynamic
    data/3,             % Hash, URL, Options
    data_columns/2,	% Hash, Columns
    data_retrieved/3,	% Hash, Retrieved, LastModified
    data_last_access/2. % Hash, Time

load_csv(URL, Options) :-
    variant_sha1(URL+Options, Hash),
    setup_call_catcher_cleanup(
        http_open(URL, In,
                  [ header(content_type, ContentType),
                    last_modified(LastModified)
                  ]),
        load_csv_stream(In, Hash, ContentType, Signature, Options),
        Catcher,
        finalize(Catcher, In, LastModified, Signature)).

load_csv_stream(In, Hash, ContentType, Signature, Options) :-
    set_encoding(In, ContentType, Options),
    lazy_list(lazy_read_rows(In, [functor(Hash)|Options]), Rows0),
    signature(Rows0, Rows, Hash, Signature, Options),
    maplist(assertz, Rows),
    !.

set_encoding(In, _ContentType, Options) :-
    option(encoding(Enc), Options, utf8),
    set_stream(In, encoding(Enc)).

signature(Rows, Rows, Hash, Signature, Options) :-
    option(columns(Names), Options),
    !,
    must_be(list(atom), Names),
    Signature =.. [Hash|Names].
signature([Head|Rows], Rows, _Hash, Signature, _Options) :-
    Head =.. [_|Names],
    must_be(list(atom), Names),
    Signature = Head.

finalize(exit, In, LastModified, Signature) :-
    !,
    close(In),
    functor(Signature, Hash, _),
    (   parse_time(LastModified, LastModStamp)
    ->  true
    ;   LastModStamp = (-)
    ),
    get_time(Now),
    assertz(data_retrieved(Hash, Now, LastModStamp)).
finalize(Reason, In, _LastModified, Signature) :-
    close(In),
    retractall(Signature).


%!  lazy_read_rows(+Stream, +Options, -List, -Tail)
%
%   Lazy list iterator for rows in a CSV file

lazy_read_rows(Stream, Options, List, Tail) :-
    select_option(chunk(ChunkSize), Options, CSVOptions, 10),
    csv_options(Record, CSVOptions),
    lazy_read_rows(Stream, Record, ChunkSize, List, Tail).

:- lazy_list_iterator(lazy_read_rows(Stream, RecordOptions), Row,
                      csv_read_row(Stream, Row, RecordOptions),
                      Row == end_of_file).
