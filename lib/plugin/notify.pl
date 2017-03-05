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
    notify_open_db,
    assert_follower(DocID, ProfileID, Options).

%!  notify(+DocID, +Action) is det.
%
%   Action has been executed on DocID.  Notify all interested users.

notify(DocID, Action) :-
    notify_open_db,
    forall(follower(DocID, Profile, Options),
           notify_user(Profile, Action, Options)).

%!  notify_user(+Profile, +Action, +Options)
%
%   Notify the user belonging to Profile about Action.

notify_user(Profile, Action, Options) :-
    notify_chat(Profile, Action, Options),
    notify_by_mail(Profile, Action, Options).

		 /*******************************
		 *            CHAT		*
		 *******************************/




		 /*******************************
		 *            EMAIL		*
		 *******************************/

notify_by_mail(Profile, Action, _Options) :-
    profile_property(Profile, email(Email)),
    phrase(subject(Action), Codes),
    string_codes(Subject, Codes),
    smtp_send_html(Email, \message(Profile, Action), [subject(Subject)]).

message(Profile, Action) -->
    dear(Profile),
    body(Action),
    signature.
