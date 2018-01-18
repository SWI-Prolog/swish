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
          [ swish_provenance/2,                 % :Goal, -Provenance
            permahash/2                         % :Goal, -Hash
          ]).
:- use_module(library(apply)).
:- use_module(library(pengines)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(signature)).              % from pcache pack
:- use_module(config).
:- use_module(page).
:- use_module(storage).
:- use_module(gitty).
:- use_module(authenticate).
:- use_module(pep).
:- use_module(library(http/http_dispatch)).

:- meta_predicate
    swish_provenance(:, -),
    permahash(:, -).

/** <module> SWISH provenance collection

This module provides persistent hashes for a goal and its dependencies.
*/

:- http_handler(swish('q/'), permalink,   [ id(permalink), prefix ]).

%!  permahash(:Goal, -Hash) is det.
%
%   Create a hash for Goal and its dependencies.

permahash(M:Goal, Hash) :-
    goal_string(Goal, String),
    swish_provenance(M:Goal, Provenance),
    storage_store_term(Provenance, ProvHash),
    storage_store_term(goal{goal:String,
                            prov:ProvHash}, Hash).

goal_string(Goal, String) :-
    State = state(''),
    (   nb_current('$variable_names', Bindings),
        maplist(bind, Bindings),
        with_output_to(string(String), portray_clause(Goal)),
        nb_setarg(1, State, String),
        fail
    ;   arg(1, State, String)
    ).

bind(Name=Var) :-
    Var = '$VAR'(Name).

%!  swish_provenance(:Goal, -Provenance:dict) is det.
%
%   Provide provenance information for running Goal.

swish_provenance(Goal, Provenance) :-
    goal_provenance(Goal, Prov0),
    (   select(SourceID-Preds, Prov0, Prov1),
        split_string(SourceID, "/", "/", ["pengine:", IdS, "src"]),
        atom_string(Module, IdS),
        Preds \== []
    ->  local_source(Module, SourceID, Preds, Source),
        convlist(file_prov(Module), Prov1, Used),
        (   Used \== []
        ->  Provenance = prov{ local: Source,
                               import: Used }
        ;   Provenance = prov{ local: Source }
        )
    ;   Goal = M:_,
        convlist(file_prov(M), Prov0, Used),
        Used \== []
    ->  Provenance = prov{ import: Used }
    ;   Provenance = prov{}
    ).

file_prov(Module, URI-Preds0, Hash-Preds) :-
    current_predicate(Module:'swish included'/1),
    Module:'swish included'(gitty(CommitHash, _DataHash, URI)),
    !,
    Hash = CommitHash,
    maplist(unqualify(Module), Preds0, Preds).

unqualify(M, Pred0, Pred) :-
    Pred0.head = M:Plain,
    Pred = Pred0.put(head, Plain).

local_source(Module, SourceID, Preds0,
             [source{gitty:Hash, predicates:Preds}]) :-
    pengine_self(Me),
    pengine_property(Me, source(SourceID, Source)),
    !,
    storage_store_term(source{text:Source}, Hash),
    maplist(unqualify(Module), Preds0, Preds).
local_source(_Module, _SourceID, Preds, Source) :-
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

		 /*******************************
		 *      RESTORE A PERMALINK	*
		 *******************************/

%!  permalink(+Request)
%
%   Open a query and  source  from   a  permalink.  Normally  mounted on
%   `/q/hash`.

permalink(Request) :-
    authenticate(Request, Auth),
    permalink(Request, [identity(Auth)]).

permalink(Request, _Options) :-
    swish_reply_resource(Request), !.
permalink(Request, Options) :-
    swish_reply_config(Request, Options), !.
permalink(Request, Options) :-
    option(path_info(Hash), Request),
    is_gitty_hash(Hash),
    authorized(gitty(download(Hash, permalink)), Options),
    storage_load_term(Hash, PermaData),
    _{goal:Goal, prov:Prov} :< PermaData,
    storage_load_term(Prov, ProvData),
    (   _{local:Local} :< ProvData
    ->  maplist(source, Local, Sources),
        atomics_to_string(Sources, "\n", Code)
    ;   Code = ""
    ),
    swish_reply([ code(Code),
                  q(Goal),
                  show_beware(false)
                | Options
                ],
                Request).

source(Prov, Source) :-
    storage_load_term(Prov.get(gitty), LocalData),
    Source = LocalData.get(text).


		 /*******************************
		 *             HOOK		*
		 *******************************/

:- multifile
    swish_config:config/2,
    swish_trace:pre_context/3.

swish_config:config(permahash_var, '_swish__permahash').

swish_trace:pre_context('_swish__permahash', Goal, Hash) :-
    permahash(Goal, Hash).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(swish_provenance:permahash/2).
