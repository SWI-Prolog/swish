/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2018, VU University Amsterdam
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

:- module(swish_version_service, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(git)).
:- use_module(library(apply)).

:- use_module(version).
:- use_module(markdown).

/** <module> Serve version details over HTTP

This module serves SWISH and Prolog version   details over HTTP. This is
file is normally loaded though the   config file `version_info.pl`. This
file is not loaded by default for security reasons.
*/

:- http_handler(swish(versions),  versions,  [id(versions)]).
:- http_handler(swish(changes),   changes,   [id(changes)]).
:- http_handler(swish(changelog), changelog, [id(changelog)]).

versions(_Request) :-
    prolog_version_atom(SWIVersion),
    module_version_data(swish, SWISHVersion),
    reply_json_dict(json{ prolog:
                          json{ brand:  "SWI-Prolog",
                                version: SWIVersion
                              },
                          swish:SWISHVersion
                        }).

module_version_data(Module, Dict) :-
    findall(Name-Value, module_version_data(Module, Name, Value), Pairs),
    dict_pairs(Dict, json, Pairs).

module_version_data(Module, Name, Value) :-
    git_module_property(Module, Term),
    Term =.. [Name,Value].

%!  changes(+Request)
%
%   Get quick statistics on changes since a   commit  to inform the user
%   about new functionality. If no commit is  passed we reply no changes
%   and the last commit we have seen.

:- dynamic  change_cache/3.
:- volatile change_cache/3.

:- multifile
    user:message_hook/3.

user:message_hook(make(done(_)), _, _) :-
    retractall(change_cache(_,_,_)),
    fail.

changes(Request) :-
    http_parameters(Request,
                    [ commit(Commit, [optional(true)]),
                      show(Show, [oneof([tagged, all]), default(tagged)])
                    ]),
    changes(Commit, Show, Changes),
    reply_json_dict(Changes).

changes(Commit, Show, Changes) :-
    change_cache(Commit, Show, Changes),
    !.
changes(Commit, Show, Changes) :-
    git_module_property(swish, directory(Dir)),
    (   nonvar(Commit)
    ->  atom_concat(Commit, '..', Revisions),
        Options = [ revisions(Revisions) ]
    ;   Options = [ limit(1) ]
    ),
    git_shortlog(Dir, ShortLog0, Options),
    (   Show == tagged
    ->  include(is_tagged_change, ShortLog0, ShortLog)
    ;   ShortLog = ShortLog0
    ),
    (   last_change(ShortLog, LastCommit, LastModified)
    ->  (   nonvar(Commit)
        ->  length(ShortLog, Count)
        ;   Count = 0
        ),
        Changes = json{commit:  LastCommit,
                       date:    LastModified,
                       changes: Count
                      }
    ;   last_change(ShortLog0, LastCommit, LastModified)
    ->  Changes = json{commit:  LastCommit,
                       date:    LastModified,
                       changes: 0
                      }
    ;   Changes = json{ changes: 0
                      }
    ),
    asserta(change_cache(Commit, Show, Changes)).

last_change([LastEntry|_], LastCommit, LastModified) :-
    git_log_data(commit_hash,         LastEntry, LastCommit),
    git_log_data(committer_date_unix, LastEntry, LastModified).

is_tagged_change(Change) :-
    git_log_data(subject, Change, Message0),
    sub_string(Message0, Pre, _, _, ":"),
    Pre > 0,
    !,
    sub_string(Message0, 0, Pre, _, Tag),
    string_upper(Tag, Tag).

%!  changelog(+Request)
%
%   Sends the changelog since a  given  version,   as  well  as the last
%   commit and its timestamp.

changelog(Request) :-
    http_parameters(Request,
                    [ commit(Since, [optional(true)]),
                      last(Count, [default(10)]),
                      show(Show, [oneof([tagged, all]), default(tagged)])
                    ]),
    git_module_property(swish, directory(Dir)),
    (   nonvar(Since)
    ->  atom_concat(Since, '..', Revisions),
        Options = [ revisions(Revisions) ]
    ;   Options = [ limit(Count) ]
    ),
    git_shortlog(Dir, ShortLog, Options),
    (   ShortLog = [LastEntry|_]
    ->  git_log_data(commit_hash,         LastEntry, LastCommit),
        git_log_data(committer_date_unix, LastEntry, LastModified),
        convlist(changelog(Show), ShortLog, ChangeLog),
        reply_json_dict(json{ commit:    LastCommit,
                              date:	 LastModified,
                              changelog: ChangeLog
                            })
    ;   reply_json_dict(json{ message: "No changes"
                            })
    ).

changelog(Show, Entry,
          json{commit:Commit,
               author: Author,
               committer_date_relative: When,
               message: Message}) :-
    git_log_data(subject, Entry, Message0),
    format_commit_message(Show, Message0, Message),
    git_log_data(commit_hash, Entry, Commit),
    git_log_data(author_name, Entry, Author),
    git_log_data(committer_date_relative, Entry, When).


format_commit_message(tagged, Message0, Message) :-
    sub_string(Message0, Pre, _, Post, ":"),
    Pre > 0,
    !,
    sub_string(Message0, 0, Pre, _, Tag),
    string_upper(Tag, Tag),
    sub_string(Message0, _, Post, 0, Msg),
    string_codes(Msg, Codes),
    wiki_file_codes_to_dom(Codes, '/', DOM),
    phrase(wiki_html(div(class('v-changelog-entry'),
                         [ span(class('v-changelog-tag'), Tag)
                         | DOM
                         ])),
           Tokens),
    with_output_to(string(Message), print_html(Tokens)).
format_commit_message(all, Message0, Message) :-
    format_commit_message(tagged, Message0, Message),
    !.
format_commit_message(all, Message0, Message) :-
    string_codes(Message0, Codes),
    wiki_file_codes_to_dom(Codes, '/', DOM),
    phrase(wiki_html(div(class('v-changelog-entry'),
                         DOM)),
           Tokens),
    with_output_to(string(Message), print_html(Tokens)).

		 /*******************************
		 *          COMPATIBILITY	*
		 *******************************/

% support SWI-Prolog < 7.7.19

:- if(\+catch(check_predicate_option(git:git_shortlog/3, 3, revisions(a)),
              error(_,_), fail)).
:- abolish(git:git_shortlog/3).
git:(
git_shortlog(Dir, ShortLog, Options) :-
    (   option(revisions(Range), Options)
    ->  RangeSpec = [Range]
    ;   option(limit(Limit), Options, 10),
        RangeSpec = ['-n', Limit]
    ),
    (   option(git_path(Path), Options)
    ->  Extra = ['--', Path]
    ;   option(path(Path), Options)
    ->  relative_file_name(Path, Dir, RelPath),
        Extra = ['--', RelPath]
    ;   Extra = []
    ),
    git_format_string(git_log, Fields, Format),
    append([[log, Format], RangeSpec, Extra], GitArgv),
    git_process_output(GitArgv,
                       read_git_formatted(git_log, Fields, ShortLog),
                       [directory(Dir)])).
:- endif.

