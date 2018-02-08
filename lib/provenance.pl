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
            permahash/2,                        % :Goal, -Hash
            current_permahash/3                 % ?Name, -Meta, -Hash
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
		 *        HOOK FOR .LNK		*
		 *******************************/

:- multifile
    web_storage:open_hook/3.

web_storage:open_hook(swish, Options0, Options) :-
    option(meta(Meta), Options0),
    file_name_extension(_, lnk, Meta.get(name)),
    option(code(HashCode), Options0),
    atom_string(Hash, HashCode),
    is_gitty_hash(Hash),
    !,
    permalink_code_query(Hash, Code, Query),
    merge_options([ code(Code),
                    q(Query),
                    type(pl),
                    show_beware(false)
                  ],
                  Options0, Options).
web_storage:open_hook(json, JSON0, JSON) :-
    file_name_extension(_, lnk, JSON0.get(meta).get(name)),
    atom_string(Hash, JSON0.get(data)),
    is_gitty_hash(Hash),
    !,
    permalink_code_query(Hash, Code, Query),
    JSON = JSON0.put(_{data:Code, query:Query}).


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
    permalink_code_query(Hash, Code, Query),
    swish_reply([ code(Code),
                  q(Query),
                  show_beware(false)
                | Options
                ],
                Request).

permalink_code_query(Hash, Code, Query) :-
    storage_load_term(Hash, PermaData),
    _{goal:Query, prov:Prov} :< PermaData,
    storage_load_term(Prov, ProvData),
    (   _{local:Local} :< ProvData
    ->  maplist(source, Local, Sources),
        atomics_to_string(Sources, "\n", Code0),
        import_versions(ProvData, Code0, Code)
    ;   import_versions(ProvData, "", Code)
    ).

source(Prov, Source) :-
    storage_load_term(Prov.get(gitty), LocalData),
    Source = LocalData.get(text).

%!  import_versions(+ProvData, +Code0, -Code) is det.
%
%   If there are imported files, check  that their versions are current.
%   If not, prepend an include statement to import the right version.

import_versions(ProvData, Code0, Code) :-
    _{import:Import} :< ProvData,
    convlist(import_version(Code0), Import, Strings),
    Strings \== [],
    !,
    append(Strings, [Code0], AllStrings),
    atomics_to_string(AllStrings, "\n", Code).
import_versions(_, Code, Code).

%!  import_version(+Code0, +Import, -String) is semidet.
%
%   If Import is not the HEAD of the  imported file, unify String with a
%   header that includes the specific version.
%
%   @tbd We also have the hashes  of   the  predicates from the imported
%   files that are used. We could use   this to verify that the imported
%   predicates have not changed and therefore  we can (still) import the
%   HEAD rather than the specific version.
%
%   @tbd Currently assumes there is either  no local source (2nd clause)
%   or the local source contains  all   required  `:-  include`. Is that
%   true?

import_version(Code0, Hash-_Predicates, String) :-
    storage_meta_data(Hash, Meta),
    import_version(Code0, Hash, Meta, String).

import_version(_Code0, Hash, Meta, String) :-
    \+ Meta.get(symbolic) == "HEAD", !,
    format_time(string(Date), '%+', Meta.time),
    file_name_extension(Base, _, Meta.name),
    format(string(String),
           '% Permalink: using "~w" from ~s\n\c
            :- include(~q, [version(~q)]).',
           [ Base, Date, Base, Hash ]).
import_version("", _Hash, Meta, String) :-
    file_name_extension(Base, _, Meta.name),
    format(string(String),
           '% Permalink: using current version of "~w"\n\c
            :- include(~q).',
           [ Base, Base ]).


		 /*******************************
		 *            ENUMERATE		*
		 *******************************/

%!  current_permahash(?Name, -Meta, -Hash) is nondet.
%
%   Enumerate saved permahashes.
%
%   @arg Name is the name of the permahash file
%   @arg Meta is the meta data of this file (author, time, tags, etc.)
%   @arg Hash is the permahash

current_permahash(Name, Hash, Meta) :-
    storage_file_extension(Name, lnk),
    storage_file(Name, HashString, Meta),
    atom_string(Hash, HashString),
    is_gitty_hash(Hash).


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
