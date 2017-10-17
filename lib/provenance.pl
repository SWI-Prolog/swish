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

:- module(swish_provenance,
          [ swish_provenance/2
          ]).
:- use_module(library(apply)).
:- use_module(library(pengines)).
:- use_module(library(lists)).
:- use_module(library(signature)).              % from pcache pack
:- use_module(storage).

:- meta_predicate
    swish_provenance(:, -).

/** <module> SWISH provenance collection
*/

%!  swish_provenance(:Goal, -Provenance) is det.
%
%   Provide provenance information for running Goal.

swish_provenance(Goal, ['<local>'-Source|Used]) :-
    goal_provenance(Goal, Prov0),
    select(SourceID-Preds, Prov0, Prov1),
    split_string(SourceID, "/", "/", ["pengine:", IdS, "src"]),
    !,
    local_source(SourceID, Preds, Source),
    atom_string(Module, IdS),
    maplist(file_prov(Module), Prov1, Used).

file_prov(Module, FileURL-Preds0, Hash-Preds) :-
    atom_concat('swish://', File, FileURL),
    !,
    storage_meta_data(File, Meta),
    Hash = Meta.commit,
    maplist(unqualify(Module), Preds0, Preds).
file_prov(_, Prov, Prov).

unqualify(M, Pred0, Pred) :-
    Pred0.head = M:Plain,
    Pred = Pred0.put(head, Plain).

local_source(SourceID, _Preds, [source{text:Source}]) :-
    pengine_self(Me),
    pengine_property(Me, source(SourceID, Source)),
    !.
local_source(_, Preds, Source) :-
    maplist(local_def, Preds, Source).

local_def(Pred0, predicate{head:Head, clauses:Clauses}) :-
    M:Head = Pred0.head,
    findall(Clause, clause_of(M:Head, Clause), Clauses).

clause_of(M:Pred, Clause) :-
    clause(M:Pred, Body),
    (   Body == true
    ->  Clause = Pred
    ;   Clause = (Pred :- Body)
    ).
