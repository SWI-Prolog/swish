/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.
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

:- module(redis_transfer,
          [ redis_transfer_chat/0,
            redis_transfer_profiles/0,
            redis_transfer_notifications/0,
            redis_transfer/0
          ]).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(redis)).

:- use_module(library(user_profile)).
:- use_module(chatstore).
:- use_module(plugin/notify).

/** <module> Transfer file based data to Redis
*/

%!  redis_transfer
%
%   Transfer file based data to Redis.

redis_transfer :-
    redis(swish, get(swish:transferred:files), true),
    !.
redis_transfer :-
    redis_transfer_chat,
    redis_transfer_profiles,
    redis_transfer_notifications,
    redis(swish, set(swish:transferred:files, true)).


		 /*******************************
		 *             CHAT		*
		 *******************************/

%!  redis_transfer_chat
%
%   Transfer the chat messages from files to Redis.

redis_transfer_chat :-
    forall(chat_message(Msg),
           chat_store(Msg)).

chat_message(Msg) :-
    chat_doc_id(DocID),
    chat_store:chat_messages_from_files(DocID, Messages,
                                        [ max(1000000)
                                        ]),
    member(Msg, Messages).

chat_doc_id(Id) :-
    (   chat_store:storage_dir(Dir)
    ->  true
    ;   with_mutex(chat_store, chat_store:open_chatstore_guarded),
        chat_store:storage_dir(Dir)
    ),
    directory_member(Dir, File, [ recursive(true) ]),
    exists_file(File),
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_term(In, Msg1, []),
        close(In)),
    is_dict(Msg1),
    Id = Msg1.docid.


		 /*******************************
		 *            PROFILE		*
		 *******************************/

%!  redis_transfer_profiles
%
%   Transfer all user profiles from files to the Redis DB.

redis_transfer_profiles :-
    file_profiles(Pairs),
    forall(member(ProfileID-Attributes, Pairs),
           profile_create(ProfileID, Attributes)).

file_profiles(Pairs) :-
    set_setting(user_profile:backend, impl_profile_prolog),
    profile_open_db([]),
    findall(ProfileID-Attributes,
            current_profile(ProfileID, Attributes),
            Pairs),
    set_setting(user_profile:backend, impl_profile_redis).


		 /*******************************
		 *         NOTIFICATIONS	*
		 *******************************/

%!  redis_transfer_notifications
%
%   Transfer notifications from files to redis.

redis_transfer_notifications :-
    swish_notify:notify_open_db,
    forall(swish_notify:follower(DocID, ProfileID, Options),
           follow(DocID, ProfileID, Options)).
