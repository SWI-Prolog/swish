/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam
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

:- module(swish_chat,
	  [ chat_broadcast/1,		% +Message
	    chat_broadcast/2,		% +Message, +Channel

	    notifications//1		% +Options
	  ]).
:- use_module(library(http/hub)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/websocket)).
:- use_module(library(http/json)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).
:- use_module(library(http/html_write)).

:- use_module(storage).
:- use_module(gitty).
:- use_module(config).
:- use_module(avatar).

/** <module> The SWISH collaboration backbone

We have three levels of identity as   enumerated  below. Note that these
form a hierarchy: a particular user  may   be  logged  on using multiple
browsers which in turn may have multiple SWISH windows opened.

  1. Any open SWISH window has an associated websocket, represented
     by the identifier returned by hub_add/3.
  2. Any browser, possibly having multiple open SWISH windows, is
     identified by a session cookie.
  3. The user may be logged in, either based on the cookie or on
     HTTP authentication.
*/

:- multifile
	swish_config:config/2.

swish_config:config(chat, true).


		 /*******************************
		 *	ESTABLISH WEBSOCKET	*
		 *******************************/

:- http_handler(swish(chat), start_chat, [ id(swish_chat) ]).

start_chat(Request) :-
	swish_config:authenticate(Request, User), !, % must throw to deny access
	start_chat(Request, [user(User)]).
start_chat(Request) :-
	start_chat(Request, []).

start_chat(Request, Options) :-
	http_session_id(Session),
	http_upgrade_to_websocket(
	    accept_chat(Session, Options),
	    [ guarded(false),
	      subprotocols([chat])
	    ],
	    Request).

accept_chat(Session, Options, WebSocket) :-
	create_chat_room,
	hub_add(swish_chat, WebSocket, Id),
	create_visitor(Id, Session, TmpUser, UserData, Options),
	hub_send(Id, json(UserData.put(_{type:welcome, uid:TmpUser}))).


		 /*******************************
		 *	        DATA		*
		 *******************************/

%%	visitor_session(?WSId, ?Session).
%%	session_user(?Session, ?TmpUser).
%%	visitor_data(?TmpUser, ?UserData:dict).
%%	subscription(?Session, ?Channel, ?SubChannel).
%
%	These predicates represent our notion of visitors.
%
%	@arg WSID is the identifier of the web socket. As we may have to
%	reconnect lost connections, this is may be replaced.
%	@arg Session is the session identifier.  This is used to connect
%	SWISH actions to WSIDs.
%	@arg TmpUser is the ID with which we identify the user for this
%	run. The value is a UUID and thus doesn't reveal the real
%	identity of the user.
%	@arg UserDict is a dict that holds information about the real
%	user identity.  This can be empty if no information is known
%	about this user.

:- dynamic
	visitor_session/2,		% WSID, Session
	session_user/2,			% Session, TmpUser
	visitor_data/2,			% TmpUser, Data
	subscription/3.			% Session, Channel, SubChannel

%%	create_visitor(+WSID, +Session, -TmpUser, -UserData, +Options)
%
%	Create a new visitor. The first   clause  deals with two windows
%	opened from the same  browser  or   re-establishing  a  lost web
%	socket.

create_visitor(WSID, Session, TmpUser, UserData, Options) :-
	(   visitor_session(_, Session)
	->  true
	;   OneDay is 24*60*60,
	    http_set_session(Session, timeout(OneDay))
	),
	assertz(visitor_session(WSID, Session)),
	create_session_user(Session, TmpUser, UserData, Options).

%%	destroy_visitor(+WSID)
%
%	The web socket WSID has been   closed. We should not immediately
%	destroy the temporary user as the browser may soon reconnect due
%	to a page reload  or  re-establishing   the  web  socket after a
%	temporary network failure. We leave   the destruction thereof to
%	the session, but set the session timeout to a fairly short time.
%
%	@tbd	We should only inform clients that we have informed
%		about this user.

destroy_visitor(WSID) :-
	must_be(atom, WSID),
	retract(visitor_session(WSID, Session)),
	(   visitor_session(_, Session)
	->  true
	;   http_set_session(Session, timeout(300)),
	    session_user(Session, UID),
	    Message = _{ type:left,
			 uid:UID
		       },
	    chat_broadcast(Message)
	).

%%	create_session_user(+Session, -User, -UserData, +Options)
%
%	Associate a user with the session. The user id is a UUID that is
%	not associated with  any  persistent  notion   of  a  user.  The
%	destruction is left to the destruction of the session.

:- unlisten(http_session(end(_, _))),
   listen(http_session(end(SessionID, _Peer)),
	  destroy_session_user(SessionID)).

create_session_user(Session, TmpUser, UserData, _Options) :-
	session_user(Session, TmpUser),
	visitor_data(TmpUser, UserData), !.
create_session_user(Session, TmpUser, UserData, Options) :-
	uuid(TmpUser),
	get_visitor_data(UserData, Options),
	assertz(session_user(Session, TmpUser)),
	assertz(visitor_data(TmpUser, UserData)).

destroy_session_user(Session) :-
	retract(session_user(Session, TmpUser)),
	destroy_visitor_data(TmpUser).

destroy_visitor_data(TmpUser) :-
	(   retract(visitor_data(TmpUser, Data)),
	    release_avatar(Data.get(avatar)),
	    fail
	;   true
	).

%%	subscribe(+WSID, +Channel) is det.

subscribe(WSID, Channel) :-
	subscribe(WSID, Channel, _SubChannel).
subscribe(WSID, Channel, SubChannel) :-
	visitor_session(WSID, Session),
	assertz(subscription(Session, Channel, SubChannel)).

unsubscribe(WSID, Channel) :-
	unsubscribe(WSID, Channel, _SubChannel).
unsubscribe(WSID, Channel, SubChannel) :-
	visitor_session(WSID, Session),
	retractall(subscription(Session, Channel, SubChannel)).

%%	subscribe_session_to_gitty_file(+Session, +File)

subscribe_session_to_gitty_file(Session, File) :-
	(   subscription(Session, gitty, File)
	->  true
	;   assertz(subscription(Session, gitty, File))
	).

%%	add_user_details(+Message, -Enriched) is det.
%
%	Add additional information to a message.  Message must
%	contain a `uid` field.

add_user_details(Message, Enriched) :-
	visitor_data(Message.uid, Data),
	(   _{realname:Name, avatar:Avatar} :< Data
	->  Enriched = Message.put(_{name:Name, avatar:Avatar})
	;   _{avatar:Avatar} :< Data
	->  Enriched = Message.put(_{avatar:Avatar})
	).

%%	get_visitor_data(-Data:dict, +Options) is det.
%
%	Optain data for a new visitor.
%
%	@bug	This may check for avatar validity, which may take
%		long.  Possibly we should do this in a thread.

get_visitor_data(Data, Options) :-
	swish_config:config(user, UserData, Options), !,
	(   _{realname:Name, email:Email} :< UserData
	->  email_avatar(Email, Avatar),
	    dict_create(Data, u,
			[ realname(Name),
			  email(email),
			  avatar(Avatar)
			| Options
			])
	;   _{realname:Name} :< UserData
	->  random_avatar(Avatar),
	    dict_create(Data, u,
			[ realname(Name),
			  avatar(Avatar)
			| Options
			])
	;   _{user:Name} :< UserData
	->  dict_create(Data, u,
			[ realname(Name)
			| Options
			])
	).
get_visitor_data(u{avatar:Avatar}, _Options) :-
	random_avatar(Avatar).


email_avatar(Email, Avatar) :-
	email_gravatar(Email, Avatar),
	valid_gravatar(Avatar), !.
email_avatar(_, Avatar) :-
	random_avatar(Avatar).


		 /*******************************
		 *	   BROADCASTING		*
		 *******************************/

%%	chat_broadcast(+Message)
%%	chat_broadcast(+Message, +Channel)
%
%	Send Message to all known SWISH clients. Message is a valid JSON
%	object, i.e., a dict or option list.
%
%	@arg Channel is either an atom or a term Channel/SubChannel,
%	where both Channel and SubChannel are atoms.

chat_broadcast(Message) :-
	debug(chat(broadcast), 'Broadcast: ~p', [Message]),
	hub_broadcast(swish_chat, json(Message)).

chat_broadcast(Message, Channel/SubChannel) :- !,
	must_be(atom, Channel),
	must_be(atom, SubChannel),
	debug(chat(broadcast), 'Broadcast on ~p: ~p',
	      [Channel/SubChannel, Message]),
	hub_broadcast(swish_chat, json(Message),
		      subscribed(Channel, SubChannel)).
chat_broadcast(Message, Channel) :-
	must_be(atom, Channel),
	debug(chat(broadcast), 'Broadcast on ~p: ~p', [Channel, Message]),
	hub_broadcast(swish_chat, json(Message),
		      subscribed(Channel)).

subscribed(Channel, WSID) :-
	visitor_session(WSID, Session),
	subscription(Session, Channel, _).
subscribed(Channel, SubChannel, WSID) :-
	visitor_session(WSID, Session),
	subscription(Session, Channel, SubChannel).


		 /*******************************
		 *	     CHAT ROOM		*
		 *******************************/

create_chat_room :-
	current_hub(swish_chat, _), !.
create_chat_room :-
	with_mutex(swish_chat, create_chat_room_sync).

create_chat_room_sync :-
	current_hub(swish_chat, _), !.
create_chat_room_sync :-
	hub_create(swish_chat, Room, _{}),
	thread_create(swish_chat(Room), _, [alias(swish_chat)]).

swish_chat(Room) :-
	(   catch(swish_chat_event(Room), E, chat_exception(E))
	->  true
	;   print_message(warning, goal_failed(swish_chat_event(Room)))
	),
	swish_chat(Room).

chat_exception('$aborted') :- !.
chat_exception(E) :-
	print_message(warning, E).

swish_chat_event(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room).

%%	handle_message(+Message, +Room)
%
%	Handle incomming messages

handle_message(Message, _Room) :-
	websocket{opcode:text} :< Message, !,
	atom_json_dict(Message.data, JSON, []),
	json_message(JSON, Message.client).
handle_message(Message, _Room) :-
	hub{joined:WSID} :< Message, !,
	debug(chat(visitor), 'Joined: ~p', [WSID]).
handle_message(Message, _Room) :-
	hub{left:WSID} :< Message, !,
	(   destroy_visitor(WSID)
	->  debug(chat(visitor), 'Left: ~p', [WSID])
	;   true
	).
handle_message(Message, _Room) :-
	websocket{opcode:close, client:WSID} :< Message, !,
	debug(chat(visitor), 'Left: ~p', [WSID]),
	destroy_visitor(WSID).
handle_message(Message, _Room) :-
	debug(chat(ignored), 'Ignoring chat message ~p', [Message]).


json_message(Dict, Client) :-
	_{ type: "subscribe",
	   channel:ChannelS, sub_channel:SubChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	atom_string(SubChannel, SubChannelS),
	subscribe(Client, Channel, SubChannel).
json_message(Dict, Client) :-
	_{type: "subscribe", channel:ChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	subscribe(Client, Channel).
json_message(Dict, Client) :-
	_{ type: "unsubscribe",
	   channel:ChannelS, sub_channel:SubChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	atom_string(SubChannel, SubChannelS),
	unsubscribe(Client, Channel, SubChannel).
json_message(Dict, Client) :-
	_{type: "unsubscribe", channel:ChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	unsubscribe(Client, Channel).


		 /*******************************
		 *	      EVENTS		*
		 *******************************/

:- unlisten(swish(_, _)),
   listen(swish(Request, Event), swish_event(Event, Request)).

swish_event(Event, _Request) :-
	http_session_id(Session),
	debug(event, 'Event: ~p, session ~q', [Event, Session]),
	event_file(Event, File),
	subscribe_session_to_gitty_file(Session, File),
	broadcast_event(Event, File, Session).

broadcast_event(Event, File, Session) :-
	session_user(Session, UID),
	event_html(Event, HTML),
	Message0 = _{ type:notify,
		      uid:UID,
		      html:HTML
		    },
	add_user_details(Message0, Message),
	chat_broadcast(Message, gitty/File).

event_html(Event, HTML) :-
	phrase(event_message(Event), Tokens),
	delete(Tokens, nl(_), SingleLine),
	with_output_to(string(HTML), print_html(SingleLine)).

event_message(created(File)) -->
	html([ 'Created ', \file(File) ]).
event_message(updated(File, _From, _To)) -->
	html([ 'Saved ', \file(File) ]).
event_message(deleted(File, _From, _To)) -->
	html([ 'Deleted ', \file(File) ]).
event_message(download(Store, FileOrHash)) -->
	{ event_file(download(Store, FileOrHash), File)
	},
	html([ 'Downloaded ', \file(File) ]).

file(File) -->
	html(a(href('/p/'+File), File)).

event_file(created(File), File).
event_file(updated(File, _From, _To), File).
event_file(deleted(File, _From, _To), File).
event_file(download(Store, FileOrHash), File) :-
	(   is_gitty_hash(FileOrHash)
	->  gitty_commit(Store, FileOrHash, Meta),
	    File = Meta.name
	;   File = FileOrHash
	).

		 /*******************************
		 *	       UI		*
		 *******************************/

%%	notifications(+Options)//
%
%	The  chat  element  is  added  to  the  navbar  and  managed  by
%	web/js/chat.js

notifications(_Options) -->
	html(ul([ class([nav, 'navbar-nav', 'pull-right']),
		  id(chat)
		], [])).
