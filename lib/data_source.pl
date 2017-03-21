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

:- module(swish_data_source,
          [ data_source/2,              % :Id, +Source
            record/2,                   % :Id, -Record

            data_flush/1,               % +Hash
            'data assert'/1,            % +Term
            'data materialized'/3,	% +Hash, +Signature, +SourceID
            'data failed'/2		% +Hash, +Signature
          ]).
:- use_module(library(error)).

/** <module> Cached data access

This module provides access to external data   by caching it as a Prolog
predicate. The data itself is kept in  a   global  data module, so it is
maintained over a SWISH Pengine invocation.
*/

:- meta_predicate
    data_source(:, +),
    record(:, -).

:- multifile
    source/2.                           % +Term, -Goal


		 /*******************************
		 *          ADMIN DATA		*
		 *******************************/

:- dynamic
    data_source_db/2,                   % Hash, Goal
    data_signature/2,                   % Hash, Signature
    data_materialized/3,                % Hash, Materialized, SourceID
    data_last_access/2.                 % Hash, Time

'data assert'(Term) :-
    assertz(Term).

'data materialized'(Hash, Signature, SourceID) :-
    get_time(Now),
    assertz(data_signature(Hash, Signature)),
    assertz(data_materialized(Hash, Now, SourceID)).

'data failed'(_Hash, Signature) :-
    functor(Signature, Name, Arity),
    functor(Generic, Name, Arity),
    retractall(Generic).


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

swish:goal_expansion(Dict, swish_data_source:Head) :-
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
    source(Source, _Goal),
    !.
valid_source(Source) :-
    existence_error(data_source, Source).

%!  materialize(+Hash)
%
%   Materialise the data identified by Hash

materialize(Hash) :-
    must_be(atom, Hash),
    data_materialized(Hash, _When, _From),
    !.
materialize(Hash) :-
    data_source_db(Hash, Source),
    source(Source, Goal),
    call(Goal, Hash),
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
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_meta/2.

sandbox:safe_meta(swish_data_source:data_source(_:_,_), []) :- !, fail.
sandbox:safe_meta(swish_data_source:record(_:_, _), []) :- !, fail.
sandbox:safe_meta(swish_data_source:data_source(_,_), []).
sandbox:safe_meta(swish_data_source:record(_, _), []).
