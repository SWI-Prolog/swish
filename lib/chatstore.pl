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

:- module(chat_store,
          [ chat_store/1,               % +Message
            chat_messages/2             % +DocID, -Messages
          ]).
:- use_module(library(settings)).
:- use_module(library(filesex)).
:- use_module(library(readutil)).
:- use_module(library(sha)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- http_handler(swish(chat/messages), chat_messages, [ id(chat_messages) ]).

:- setting(directory, callable, data(chat),
	   'The directory for storing chat messages.').

/** <module> Store chat messages
*/

:- multifile
    swish_config:chat_count_about/2.	% +DocID, -Count

:- initialization open_chatstore.

:- dynamic  storage_dir/1.
:- volatile storage_dir/1.

open_chatstore :-
    setting(directory, Spec),
    absolute_file_name(Spec, Dir,
		       [ file_type(directory),
			 access(write),
			 file_errors(fail)
		       ]), !,
    asserta(storage_dir(Dir)).
open_chatstore :-
    setting(directory, Spec),
    absolute_file_name(Spec, Dir,
		       [ solutions(all)
		       ]),
    \+ exists_directory(Dir),
    catch(make_directory(Dir),
	  error(permission_error(create, directory, Dir), _),
	  fail), !,
    asserta(storage_dir(Dir)).

chat_dir_file(DocID, Path, File) :-
    sha_hash(DocID, Bin, []),
    hash_atom(Bin, Hash),
    sub_atom(Hash, 0, 2, _, D1),
    sub_atom(Hash, 2, 2, _, D2),
    sub_atom(Hash, 4, _, 0, Name),
    storage_dir(Dir),
    atomic_list_concat([Dir, D1, D2], /, Path),
    atomic_list_concat([Path, Name], /, File).

chat_file(DocID, File) :-
    chat_dir_file(DocID, Dir, File),
    make_directory_path(Dir).

%!  chat_store(+Message:dict) is det.
%
%   Add a chat message to the chat  store. If `Message.create == false`,
%   the message is only stored if the   chat  is already active. This is
%   used to only insert messages about changes   to the file if there is
%   an ongoing chat so we know to which version chat messages refer.

chat_store(Message) :-
    chat{docid:DocID} :< Message,
    chat_file(DocID, File),
    (	del_dict(create, Message, false, Message1)
    ->	exists_file(File)
    ;	Message1 = Message
    ),
    !,
    strip_chat(Message1, Message2),
    with_mutex(chat_store,
               setup_call_cleanup(
                   open(File, append, Out, [encoding(utf8)]),
                   format(Out, '~q.~n', [Message2]),
                   close(Out))).
chat_store(_).

%!  strip_chat(_Message0, -Message) is det.
%
%   Remove  stuff  from  a  chat  message   that  is  useless  to  store
%   permanently, such as the wsid (WebSocket id).

strip_chat(Message0, Message) :-
    strip_chat_user(Message0.get(user), User),
    !,
    Message = Message0.put(user, User).
strip_chat(Message, Message).

strip_chat_user(User0, User) :-
    del_dict(wsid, User0, _, User),
    !.
strip_chat_user(User, User).


%!  chat_messages(+DocID, -Messages:list) is det.
%
%   Get all messages associated with DocID.

chat_messages(DocID, Messages) :-
    chat_dir_file(DocID, _, File),
    (   exists_file(File)
    ->  read_file_to_terms(File, Messages, [encoding(utf8)])
    ;   Messages = []
    ).

%!  swish_config:chat_count_about(+DocID, -Count)
%
%   True when Count is the number of messages about DocID

swish_config:chat_count_about(DocID, Count) :-
    chat_messages(DocID, Messages),
    length(Messages, Count).


		 /*******************************
		 *              HTTP		*
		 *******************************/

%!  chat_messages(+Request)
%
%   HTTP handler that returns chat messages for a document

chat_messages(Request) :-
    http_parameters(Request,
                    [ docid(DocID, [])
                    ]),
    chat_messages(DocID, Messages),
    reply_json_dict(Messages).
