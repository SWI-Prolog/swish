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
            data_record/2,              % :Id, -Record
            record/2,                   % :Id, -Record
            data_property/2,            % :Id, ?Property
            data_row/2,                 % :Id, -Row
            data_row/4,                 % :Id, +Range, +Header, -Row
            data_dump/3,                % :Id, +Range, -Row

            data_flush/1,               % +Hash
            'data assert'/1,            % +Term
            'data materialized'/3,	% +Hash, +Signature, +SourceID
            'data failed'/2		% +Hash, +Signature
          ]).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(pengines)).

:- setting(max_memory, integer, 8000,
           "Max memory used for cached data store (Mb)").


/** <module> Cached data access

This module provides access to external data   by caching it as a Prolog
predicate. The data itself is kept in  a   global  data module, so it is
maintained over a SWISH Pengine invocation.
*/

:- meta_predicate
    data_source(:, +),
    data_record(:, -),
    record(:, -),
    data_row(:, -),
    data_row(:, +, +, -),
    data_dump(:, +, -),
    data_property(:, -).

:- multifile
    source/2.                           % +Term, -Goal


		 /*******************************
		 *          ADMIN DATA		*
		 *******************************/

:- dynamic
    data_source_db/3,                   % Hash, Goal, Lock
    data_signature_db/2,                % Hash, Signature
    data_materialized/5,                % Hash, Materialized, SourceID, CPU, Wall
    data_last_access/3.                 % Hash, Time, Updates

'data assert'(Term) :-
    assertz(Term).

%!  'data materialized'(+Hash, +Signature, +SourceVersion) is det.
%
%   Called by a data plugin  to  indicate   that  loading  the  data has
%   finished.
%
%   @arg Hash is the has of the original data source
%   @arg Signature is a term Hash(Arg1, Arg2, ...), where `Arg1`, ...
%   are atoms or small integers that indicate the field names.
%   @arg SourceVersion is a term that indicates the identity of the source.
%   this is typically a dict containing e.g., a time stamp, content
%   hash, HTTP =Etag= value, etc.

'data materialized'(Hash, Signature, SourceVersion) :-
    statistics(cputime, CPU1),
    get_time(Now),
    nb_current('$data_source_materalize', stats(Time0, CPU0)),
    CPU  is CPU1 - CPU0,
    Wall is Now - Time0,
    assertz(data_signature_db(Hash, Signature)),
    assertz(data_materialized(Hash, Now, SourceVersion, CPU, Wall)).

'data failed'(_Hash, Signature) :-
    functor(Signature, Name, Arity),
    functor(Generic, Name, Arity),
    retractall(Generic).

%!  data_source(:Id, +Source) is det.
%
%   Create a data source Id from   the  source definition Source. Source
%   definitions are plugin files loaded from swish(data).

data_source(M:Id, Source) :-
    variant_sha1(Source, Hash),
    data_source_db(Hash, Source, _),
    !,
    (   clause(M:'$data'(Id, Hash), true)
    ->  true
    ;   assertz(M:'$data'(Id, Hash))
    ).
data_source(M:Id, Source) :-
    valid_source(Source),
    variant_sha1(Source, Hash),
    mutex_create(Lock),
    assertz(data_source_db(Hash, Source, Lock)),
    assertz(M:'$data'(Id, Hash)).

%!  record(:Id, -Record) is nondet.
%!  data_record(:Id, -Record) is nondet.
%
%   True when Record is  a  dict  representing   a  row  in  the dataset
%   identified by Id.
%
%   @deprecated  record/2  is   deprecated.   New    code   should   use
%   data_record/2.

record(Id, Record) :-
    data_record(Id, Record).

data_record(M:Id, Record) :-
    data_hash(M:Id, Hash),
    materialize(Hash),
    data_signature_db(Hash, Signature),
    data_record(Signature, Id, Record, Head),
    call(Head).

data_record(Signature, Tag, Record, Head) :-
    Signature =.. [Name|Keys],
    pairs_keys_values(Pairs, Keys, Values),
    dict_pairs(Record, Tag, Pairs),
    Head =.. [Name|Values].

data_hash(M:Id, Hash) :-
    clause(M:'$data'(Id, Hash), true),
    !.
data_hash(_:Id, _) :-
    existence_error(dataset, Id).

%!  data_row(:Id, -Row) is nondet.
%!  data_row(:Id, +Range, +Header, -Row) is nondet.
%
%   True when Row is a term Id(Arg,   ...), where the first row contains
%   the column names.
%
%   @arg Header If `true`, include a header row.
%   @see data_dump/3 to return a table and for a description of Range.

data_row(Id, Row) :-
    data_row(Id, all, true, Row).

data_row(M:Id, Range, Header, Row) :-
    must_be(boolean, Header),
    data_hash(M:Id, Hash),
    materialize(Hash),
    data_signature_db(Hash, Signature),
    Signature =.. [_|ColNames],
    same_length(ColNames, Vars),
    Goal =.. [Hash|Vars],
    Row  =.. [Id|Vars],
    (   Header == true,
        Vars = ColNames
    ;   range(Range, M:Id, Goal)
    ).

range(all, _Id, Goal) :-
    !,
    call(Goal).
range(From-To, _Id, Goal) :-
    !,
    Skip is From - 1,
    Size is To-Skip,
    limit(Size, offset(Skip, call(Goal))).
range(Limit, _Id, Goal) :-
    Limit >= 0,
    !,
    limit(Limit, call(Goal)).
range(Limit, Id, Goal) :-
    Limit < 0,
    data_property(Id, rows(Rows)),
    Skip is Rows+Limit,
    offset(Skip, call(Goal)).

%!  data_dump(:Id, +Range, -Table) is det.
%
%   Table is a list of rows in the indicated range. This cooperates with
%   the table rendering to produce a data table.  Range is one of:
%
%     - all
%       All rows from the data are included.  Be careful if these
%       are many as it is likely to make your browser very slow.
%     - From-To
%       List the (1-based) rows From to To
%     - Count
%       If Count >= 0, list the _first_, else list the _last_
%       Count rows.

data_dump(Id, Range, Table) :-
    findall(Row, data_row(Id, Range, true, Row), Table).


%!  data_property(:Id, ?Property) is nondet.
%
%   True when Property is a known property about the data source Id.
%   Defined properties are:
%
%     - columns(-Count)
%       Number of columns in the table.
%     - column_names(-Names)
%       Names is a list of the column names as they appear in the
%       data.
%     - rows(-Rows)
%       Number of rows in the table
%     - hash(-Hash)
%       Get the internal (hashed) identifier for the data source
%     - source_version(-SourceVersion)
%       A term (often a dict) that provides version information
%       about the source.  Details depend on the source.
%     - materialized(-TimeStamp)
%       The data source was materialized at TimeStamp.
%     - source(-Term)
%       Description of the original source term used to declare
%       the data source

data_property(M:Id, Property) :-
    data_hash(M:Id, Hash),
    materialize(Hash),
    property(Property),
    property(Property, Hash).

property(columns(_)).
property(column_names(_)).
property(rows(_)).
property(hash(_)).
property(source_version(_)).
property(materialized(_)).
property(source(_)).

property(columns(Count), Hash) :-
    data_signature_db(Hash, Signature),
    functor(Signature, _, Count).
property(column_names(Names), Hash) :-
    data_signature_db(Hash, Signature),
    Signature =.. [_|Names].
property(rows(Count), Hash) :-
    data_signature_db(Hash, Signature),
    predicate_property(Signature, number_of_clauses(Count)).
property(hash(Hash), Hash).
property(source_version(SourceVersion), Hash) :-
    data_materialized(Hash, _, SourceVersion, _, _).
property(materialized(TimeStamp), Hash) :-
    data_materialized(Hash, TimeStamp, _, _, _).
property(source(SourceTerm), Hash) :-
    data_source_db(Hash, SourceTerm, _Lock).

%!  swish:goal_expansion(+Dict, -DataGoal)
%
%   Translate a Dict where the tag is   the  identifier of a data source
%   and the keys are columns pf this  source   into  a goal on the data.
%   Note that the data itself  is   represented  as  a Prolog predicate,
%   representing each row as a fact and each column as an argument.

:- multifile
    swish:goal_expansion/2.

swish:goal_expansion(Dict, swish_data_source:Head) :-
    is_dict(Dict, Id),
    prolog_load_context(module, M),
    clause(M:'$data'(Id, Hash), true),
    materialize(Hash),
    data_signature_db(Hash, Signature),
    data_record(Signature, Id, Record, Head),
    Dict :< Record.


		 /*******************************
		 *       DATA MANAGEMENT	*
		 *******************************/

valid_source(Source) :-
    must_be(nonvar, Source),
    source(Source, _Goal),
    !.
valid_source(Source) :-
    existence_error(data_source, Source).

%!  materialize(+Hash)
%
%   Materialise the data identified by   Hash.  The materialization goal
%   should
%
%     - Call 'data assert'/1 using a term Hash(Arg, ...) for each term
%       to add to the database.
%     - Call 'data materialized'(Hash, Signature, SourceVersion) on
%       completion, where `Signature` is a term Hash(ArgName, ...) and
%       `SourceVersion` indicates the version info provided by the
%       source.  Use `-` if this information is not available.
%     - OR call `data failed`(+Hash, +Signature) if materialization
%       fails after some data has been asserted.

materialize(Hash) :-
    must_be(atom, Hash),
    data_materialized(Hash, _When, _From, _CPU, _Wall),
    !,
    update_last_access(Hash).
materialize(Hash) :-
    data_source_db(Hash, Source, Lock),
    update_last_access(Hash),
    gc_data,
    with_mutex(Lock, materialize_sync(Hash, Source)).

materialize_sync(Hash, _Source) :-
    data_materialized(Hash, _When, _From, _CPU, _Wall),
    !.
materialize_sync(Hash, Source) :-
    source(Source, Goal),
    get_time(Time0),
    statistics(cputime, CPU0),
    setup_call_cleanup(
        b_setval('$data_source_materalize', stats(Time0, CPU0)),
        call(Goal, Hash),
        nb_delete('$data_source_materalize')),
    data_signature_db(Hash, Head),
    functor(Head, Name, Arity),
    public(Name/Arity).


		 /*******************************
		 *              GC		*
		 *******************************/

%!  update_last_access(+Hash) is det.
%
%   Update the last known access time. The   value  is rounded down to 1
%   minute to reduce database updates.

update_last_access(Hash) :-
    get_time(Now),
    Rounded is floor(Now/60)*60,
    (   data_last_access(Hash, Rounded, _)
    ->  true
    ;   clause(data_last_access(Hash, _, C0), true, Old)
    ->  C is C0+1,
        asserta(data_last_access(Hash, Rounded, C)),
        erase(Old)
    ;   asserta(data_last_access(Hash, Rounded, 1))
    ).

gc_stats(Hash, _{ hash:Hash,
                  materialized:When, cpu:CPU, wall:Wall,
                  bytes:Size,
                  last_accessed_ago:Ago,
                  access_frequency:AccessCount
                }) :-
    data_materialized(Hash, When, _From, CPU, Wall),
    data_signature_db(Hash, Signature),
    data_last_access(Hash, Last, AccessCount),
    get_time(Now),
    Ago is floor(Now/60)*60-Last,
    predicate_property(Signature, number_of_clauses(Count)),
    functor(Signature, _, Arity),
    Size is (88+(16*Arity))*Count.


%!  gc_data is det.
%!  gc_data(+MaxSize) is det.
%
%   Remove the last unused data set until   memory  of this module drops
%   below  MaxSize.  The   predicate   gc_data/0    is   called   before
%   materializing a data source.

gc_data :-
    setting(max_memory, MB),
    Bytes is MB*1024*1024,
    gc_data(Bytes),
    set_module(program_space(Bytes)).

gc_data(MaxSize) :-
    module_property(swish_data_source, program_size(Size)),
    Size < MaxSize,
    !.
gc_data(MaxSize) :-
    findall(Stat, gc_stats(_, Stat), Stats),
    sort(last_accessed_ago, >=, Stats, ByTime),
    member(Stat, ByTime),
       data_flush(ByTime.hash),
       module_property(swish_data_source, program_size(Size)),
       Size < MaxSize,
    !.
gc_data(_).


%!  data_flush(+Hash)
%
%   Drop the data associated with hash

data_flush(Hash) :-
    data_signature_db(Hash, Signature),
    data_record(Signature, _Id, _Record, Head),
    retractall(Head),
    retractall(data_signature_db(Hash, Head)),
    retractall(data_materialized(Hash, _When1, _From, _CPU, _Wall)),
    retractall(data_last_access(Hash, _When2, _Count)).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_meta/2.

sandbox:safe_meta(swish_data_source:data_source(Id,_), [])     :- safe_id(Id).
sandbox:safe_meta(swish_data_source:data_record(Id,_), [])     :- safe_id(Id).
sandbox:safe_meta(swish_data_source:record(Id,_), [])          :- safe_id(Id).
sandbox:safe_meta(swish_data_source:data_row(Id,_), [])        :- safe_id(Id).
sandbox:safe_meta(swish_data_source:data_row(Id,_,_,_), [])    :- safe_id(Id).
sandbox:safe_meta(swish_data_source:data_dump(Id,_,_), [])     :- safe_id(Id).
sandbox:safe_meta(swish_data_source:data_property(Id,_), [])   :- safe_id(Id).

safe_id(M:_) :- !, pengine_self(M).
safe_id(_).
