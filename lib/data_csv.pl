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
          [ data_source/2,              % :Id, +Source
            record/2,                   % :Id, -Record

            data_flush/1                % +Hash
          ]).
:- use_module(library(csv)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lazy_lists)).
:- use_module(library(apply)).
:- use_module(library(date)).
:- use_module(library(zlib)).
:- use_module(library(http/http_open)).

/** <module> Cached CSV data access

This module provides access to CSV  data   by  caching  it into a Prolog
module. The data itself is kept in  a   separate  data  module, so it is
maintained over a SWISH Pengine invocation. It  still needs to be unique
and thus is is  stored  under  a   hash  constructed  from  the  URL and
processing options.
*/

:- meta_predicate
    csv_data(:, +, +),
    record(:, -).


%!  data_source(:Id, +Source) is det.
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

data_source(M:Id, Source) :-
    variant_sha1(Source, Hash),
    data_source_db(Hash, Source),
    !,
    (   clause(M:'$data'(Id, Hash), true)
    ->  true
    ;   assertz(M:'$data'(Id, Hash))
    ).
data_source(M:Id, Source) :-
    valid_source(Source),
    variant_sha1(Source, Hash),
    assertz(data_source_db(Hash, Source)),
    assertz(M:'$data'(Id, Hash)).

%!  record(:Id, -Record) is nondet.
%
%   True when Record is a record in the dataset identified by Id.

record(M:Id, Record) :-
    clause(M:'$data'(Id, Hash), true),
    !,
    materialize(Hash),
    data_signature(Hash, Signature),
    data_record(Signature, Id, Record, Head),
    call(Head).

record(_:Id, _Record) :-
    existence_error(dataset, Id).

data_record(Signature, Tag, Record, Head) :-
    Signature =.. [Name|Keys],
    pairs_keys_values(Pairs, Keys, Values),
    dict_pairs(Record, Tag, Pairs),
    Head =.. [Name|Values].

:- multifile
    swish:goal_expansion/2.

swish:goal_expansion(Dict, data_csv:Head) :-
    is_dict(Dict, Id),
    prolog_load_context(module, M),
    clause(M:'$data'(Id, Hash), true),
    materialize(Hash),
    data_signature(Hash, Signature),
    data_record(Signature, Id, Record, Head),
    Dict :< Record.


		 /*******************************
		 *       DATA MANAGEMENT	*
		 *******************************/

valid_source(Source) :-
    must_be(ground, Source),
    valid_source(Source, _Goal).

valid_source(csv(URL, Options), import_csv(URL, Options)).

%!  materialize(+Hash)
%
%   Materialise the data identified by Hash

materialize(Hash) :-
    must_be(atom, Hash),
    data_materialized(Hash, _When, _From),
    !.
materialize(Hash) :-
    data_source_db(Hash, Goal),
    call(Goal),
    data_signature(Hash, Head),
    functor(Head, Name, Arity),
    public(Name/Arity).


%!  data_flush(+Hash)
%
%   Drop the data associated with hash

data_flush(Hash) :-
    data_signature(Hash, Signature),
    data_record(Signature, _Id, _Record, Head),
    retractall(Head),
    retractall(data_signature(Hash, Head)),
    retractall(data_materialized(Hash, _When1, _From)),
    retractall(data_last_access(Hash, _When2)).


		 /*******************************
		 *           CSV IMPORT		*
		 *******************************/

:- dynamic
    data_source_db/2,                      % Hash, Goal
    data_signature/2,                   % Hash, Signature
    data_materialized/3,                % Hash, Materialized, SourceID
    data_last_access/2.                 % Hash, Time

import_csv(URL, Options) :-
    variant_sha1(URL+Options, Hash),
    input_format(URL, Options, Filters, Options1),
    setup_call_catcher_cleanup(
        http_open(URL, In,
                  [ header(content_type, ContentType),
                    header(last_modified, LastModified)
                  ]),
        ( input_filters(In, ContentType, Filters, In1, Options1),
          load_csv_stream(In1, Hash, Signature, Options1)
        ),
        Catcher,
        finalize(Catcher, In1, LastModified,
                 import_csv(URL,Options1),
                 Signature)).

load_csv_stream(In, Hash, Signature, Options) :-
    lazy_list(lazy_read_rows(In, [functor(Hash)|Options]), Rows0),
    signature(Rows0, Rows, Hash, Signature, Options),
    maplist(assertz, Rows),
    !.

signature(Rows, Rows, Hash, Signature, Options) :-
    option(columns(Names), Options),
    !,
    must_be(list(atom), Names),
    Signature =.. [Hash|Names].
signature([Head|Rows], Rows, _Hash, Signature, _Options) :-
    Head =.. [_|Names],
    must_be(list(atom), Names),
    Signature = Head.

finalize(exit, In, LastModified, _Action, Signature) :-
    !,
    close(In),
    functor(Signature, Hash, _),
    (   parse_time(LastModified, LastModStamp)
    ->  true
    ;   LastModStamp = (-)
    ),
    get_time(Now),
    assertz(data_materialized(Hash, Now, LastModStamp)),
    assertz(data_signature(Hash, Signature)).
finalize(Reason, In, _LastModified, Action, Signature) :-
    close(In),
    retractall(Signature),
    failed(Reason, Action).

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
    option(encoding(Enc), Options, utf8),
    set_stream(In, encoding(Enc)).
input_filters(In, ContentType, [gzip|T], In2, Options) :-
    !,
    zopen(In, In1, [format(gzip)]),
    input_filters(In1, ContentType, T, In2, Options).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_meta/2.

sandbox:safe_meta(data_csv:csv_data(_:_,_,_), []) :- !, fail.
sandbox:safe_meta(data_csv:record(_:_, _), []) :- !, fail.
sandbox:safe_meta(data_csv:csv_data(_,_,_), []).
sandbox:safe_meta(data_csv:record(_, _), []).
