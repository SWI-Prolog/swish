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

:- module(swish_data_csv, []).
:- use_module(library(csv)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lazy_lists)).
:- use_module(library(apply)).
:- use_module(library(date)).
:- use_module(library(zlib)).
:- use_module(library(http/http_open)).
:- use_module(library(hash_stream)).

:- use_module('../data_source').

/** <module> Data source handler for CSV data

This handler deals  with  downloading  CSV   and  TSV  files  from  HTTP
resources. The defined data source is   csv(URL, Options). The following
options are processed:

 - encoding(+Encoding)
   Set the encoding for processing the data.  Default is guessed
   from the URL header or `utf8` if there is no clue.
 - column_names(+ColumnNames)
   Names for the columns. If not provided, the first row is assumed
   to hold the column names.
*/

:- multifile
    swish_data_source:source/2.

swish_data_source:source(csv(URL, Options),
                         swish_data_csv:import_csv(URL, Options)).


		 /*******************************
		 *           CSV IMPORT		*
		 *******************************/

:- public import_csv/3.

import_csv(URL, Options, Hash) :-
    input_format(URL, Options, Filters, Options1),
    setup_call_catcher_cleanup(
        (   http_open(URL, HTTPIO,
                      [ header(content_type, ContentType),
                        header(last_modified, LastModified),
                        header(etags, Etag)
                      ]),
            stream_pair(HTTPIO, In0, Out),
            close(Out),
            open_hash_stream(In0, In, [algorithm(sha1)]),
            input_filters(In, ContentType, Filters, In1, Options1)
        ),
        load_csv_stream(In1, Hash, Signature, Options1),
        Catcher,
        finalize(Catcher, In, In1, Hash, LastModified, Etag,
                 import_csv(URL,Options1),
                 Signature)).

load_csv_stream(In, Hash, Signature, Options) :-
    lazy_list(lazy_read_rows(In, [functor(Hash)|Options]), Rows0),
    signature(Rows0, Rows, Hash, Signature, Options),
    maplist('data assert', Rows),
    !.

signature(Rows, Rows, Hash, Signature, Options) :-
    option(column_names(Names0), Options),
    !,
    column_key_list(Names0, Names),
    Signature =.. [Hash|Names].
signature([Head|Rows], Rows, Hash, Signature, _Options) :-
    Head =.. [_|Names0],
    column_key_list(Names0, Names),
    Signature =.. [Hash|Names].

column_key_list(Names0, Names) :-
    must_be(list, Names0),
    maplist(column_key, Names0, Names).

column_key(Key, Key) :-
    atom(Key),
    !.
column_key(Int, Int) :-
    integer(Int),
    current_prolog_flag(min_tagged_integer, Min),
    Int >= Min,
    current_prolog_flag(max_tagged_integer, Max),
    Int =< Max, !.
column_key(Any, Key) :-
    format(atom(Key), '~w', [Any]).


finalize(exit, HTTPIn, In, Hash, LastModified, Etag, _Action, Signature) :-
    !,
    stream_hash(HTTPIn, SHA1),
    close(In),
    (   parse_time(LastModified, LastModStamp)
    ->  true
    ;   LastModStamp = (-)
    ),
    source_tag(Etag, LastModStamp, SHA1, SourceID),
    'data materialized'(Hash, Signature, SourceID).
finalize(Reason, _, In, Hash, _LastModified, _Etag, Action, Signature) :-
    close(In),
    'data failed'(Hash, Signature),
    failed(Reason, Action).

source_tag('',   -,    SHA1, version{sha1:SHA1}) :- !.
source_tag(Etag, -,    SHA1, version{sha1:SHA1, etag:Etag}).
source_tag('',   Time, SHA1, version{sha1:SHA1, last_modified:Time}).
source_tag(Etag, Time, SHA1, version{sha1:SHA1, etag:Etag, last_modified:Time}).

failed(exception(Ex), _) :-
    throw(Ex).
failed(fail, Action) :-
    throw(error(failure_error(Action))).


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

%!  input_format(+URL, +OptionsIn, -Filters, -OptionsOut) is det.

input_format(URL, Options0, [gzip|Filters], Options) :-
    file_name_extension(URL1, gz, URL),
    !,
    input_format(URL1, Options0, Filters, Options).
input_format(URL, Options0, [], Options) :-
    (   option(separator(_), Options0)
    ->  Options = Options0
    ;   file_name_extension(_, Ext0, URL),
        downcase_atom(Ext0, Ext),
        ext_separator(Ext, Sep)
    ->  Options = [separator(Sep)|Options0]
    ;   Options = Options0
    ).

ext_separator(csv, 0',).
ext_separator(tsv, 0'\t).

%!  input_filters(+In0, +ContentType, +Filters, -In, +Options) is det.
%
%   Establish input filters and encoding from   the content type and the
%   file name.

input_filters(In, _ContentType, [], In, Options) :-
    !,
    (   option(encoding(Enc), Options)
    ->  set_stream(In, encoding(Enc))
    ;   set_stream(In, encoding(bom))
    ->  true
    ;   set_stream(In, encoding(utf8))
    ).
input_filters(In, ContentType, [gzip|T], In2, Options) :-
    !,
    zopen(In, In1, [format(gzip)]),
    input_filters(In1, ContentType, T, In2, Options).
