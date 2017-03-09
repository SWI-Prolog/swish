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

:- module(swish_notify,
          [ follow/3,                           % +DocID, +ProfileID, +Options
            notify/2                            % +DocID, +Action
          ]).
:- use_module(library(settings)).
:- use_module(library(persistency)).
:- use_module(library(broadcast)).

:- use_module(library(user_profile)).
:- use_module(email).


/** <module> SWISH notifications

This module keeps track of which users wish to track which notifications
and sending the notifications to the user.  If the target user is online
we will notify using an avatar.  Otherwise we send an email.
*/

:- setting(database, callable, swish('data/notify.db'),
           "Database holding notifications").


		 /*******************************
		 *            DATABASE		*
		 *******************************/

:- persistent
        follower(docid:string,
                 profile:string,
                 options:list(atom)).

notify_open_db :-
    db_attached(_),
    !.
notify_open_db :-
    setting(database, Spec),
    absolute_file_name(Spec, Path, [access(write)]),
    db_attach(Path, [sync(close)]).

%!  follow(+DocID, +ProfileID, +Options) is det.
%
%   Assert that DocID is being followed by ProfileID using Options.

follow(DocID, ProfileID, Options) :-
    to_atom(DocID, DocIDA),
    to_atom(ProfileID, ProfileIDA),
    notify_open_db,
    (   follower(DocIDA, ProfileIDA, OldOptions)
    ->  (   OldOptions == Options
        ->  true
        ;   retractall_follower(DocIDA, ProfileIDA, _),
            assert_follower(DocIDA, ProfileIDA, Options)
        )
    ;   assert_follower(DocIDA, ProfileIDA, Options)
    ).

%!  notify(+DocID, +Action) is det.
%
%   Action has been executed on DocID.  Notify all interested users.

notify(DocID, Action) :-
    to_atom(DocID, DocIDA),
    notify_open_db,
    forall(follower(DocIDA, Profile, Options),
           notify_user(Profile, Action, Options)).

to_atom(Text, Atom) :-
    atom_string(Atom, Text).

%!  notify_user(+Profile, +Action, +Options)
%
%   Notify the user belonging to Profile about Action.

notify_user(Profile, Action, Options) :-
    ignore(notify_chat(Profile, Action, Options)),
    ignore(notify_by_mail(Profile, Action, Options)).


		 /*******************************
		 *         BROADCAST API	*
		 *******************************/

:- unlisten(swish(_)),
   listen(swish(Event), notify_event(Event)).

notify_event(follow(DocID, ProfileID, Options)) :-
    follow(DocID, ProfileID, Options).
notify_event(updated(File, _PrevCommitID, Commit)) :-
    atom_concat('gitty:', File, DocID),
    notify(DocID, updated(Commit)).


		 /*******************************
		 *            CHAT		*
		 *******************************/

notify_chat(ProfileID, Action, _Options) :-
    chat_to_profile(ProfileID, \chat(Action)).

chat(updated(Commit)) -->
    html([\committer(Commit), ' updated ', \file_name(Commit)]).


		 /*******************************
		 *            EMAIL		*
		 *******************************/

notify_by_mail(Profile, Action, _Options) :-
    profile_property(Profile, email(Email)),
    phrase(subject(Action), Codes),
    string_codes(Subject, Codes),
    smtp_send_html(Email, \message(Profile, Action), [subject(Subject)]).

subject(Action) -->
    subject_prefix,
    subject_action(Action).

subject_prefix -->
    "[SWISH] ".

subject_action(updated(Commit)) -->
    txt_commit_file(Commit), "updated by ", txt_committer(Commit).


		 /*******************************
		 *            HTML BODY		*
		 *******************************/

message(ProfileID, Action) -->
    dear(ProfileID),
    notification(Action),
    signature.

dear(ProfileID) -->
    html(h4(["Dear ", \profile_name(ProfileID), ","])).

profile_name(ProfileID) -->
    html(ProfileID.get(name)).

signature -->
    { public_url(swish, [], HREF, []) },
    html(address(['SWISH at ', a(href(HREF),HREF)])).

notification(updated(Commit)) -->
    html(p(['The file ', \file_name(Commit),
            ' has been updated by ', \committer(Commit), '.'])).

file_name(Commit) -->
    { public_url(web_storage, path_postfix(Commit.name), HREF, []) },
    html(a(href(HREF), Commit.name)).

committer(Commit) -->
    { ProfileID = Commit.get(profile_id) }, !,
    profile_name(ProfileID).
committer(Commit) -->
    html(Commit.get(owner)).


		 /*******************************
		 *  TEXT RULES ON GITTY COMMITS	*
		 *******************************/

txt_commit_file(Commit) -->
    write(Commit.name).

txt_committer(Commit) -->
    { ProfileID = Commit.get(profile_id) }, !,
    txt_profile_name(ProfileID).
txt_committer(Commit) -->
    write(Commit.get(owner)), !.



		 /*******************************
		 *    RULES ON GITTY COMMITS	*
		 *******************************/

txt_profile_name(ProfileID) -->
    write(ProfileID.get(name)).


		 /*******************************
		 *            BASICS		*
		 *******************************/

write(Term, Head, Tail) :-
    format(codes(Head, Tail), '~w', [Term]).
