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
:- use_module(library(debug)).
:- use_module(library(http/html_write)).

:- use_module(config).

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

:- initialization
	Time is 24*60*60,
	http_set_session_options([timeout(Time)]).


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
	create_visitor(Id, Session, Options).


		 /*******************************
		 *	        DATA		*
		 *******************************/

:- dynamic
	visitor/3,			% Id, Session, OptionDict
	subscription/3.			% Session, Channel, SubChannel

create_visitor(WSID, Session, Options) :-
	dict_create(Dict, options, Options),
	assertz(visitor(WSID, Session, Dict)).

destroy_visitor(WSID) :-
	retract(visitor(WSID, Session, _)),
	retractall(subscription(Session, _, _)).

subscribe(WSID, Channel) :-
	subscribe(WSID, Channel, _SubChannel).
subscribe(WSID, Channel, SubChannel) :-
	visitor(WSID, Session),
	assertz(subscription(Session, Channel, SubChannel)).

unsubscribe(WSID, Channel) :-
	unsubscribe(WSID, Channel, _SubChannel).
unsubscribe(WSID, Channel, SubChannel) :-
	visitor(WSID, Session),
	retractall(subscription(Session, Channel, SubChannel)).


		 /*******************************
		 *	   BROADCASTING		*
		 *******************************/

%%	chat_broadcast(+Message)
%%	chat_broadcast(+Message, +Channel)
%
%	Send Message to all known SWISH clients. Message is a message as
%	accepted by ws_send/2.

chat_broadcast(Message) :-
	hub_broadcast(swish_chat, Message).

chat_broadcast(Message, Channel/SubChannel) :- !,
	must_be(atom, Channel),
	must_be(atom, SubChannel),
	hub_broadcast(swish_chat, Message, subscribed(Channel, SubChannel)).
chat_broadcast(Message, Channel) :-
	must_be(atom, Channel),
	hub_broadcast(swish_chat, Message, subscribed(Channel)).

subscribed(Channel, WSID) :-
	subscription(WSID, Channel, _).
subscribed(Channel, SubChannel, WSID) :-
	subscription(WSID, Channel, SubChannel).


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
	hub{joined:Id} :< Message, !,
	debug(chat(visitor), 'Joined: ~p', [Id]).
handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !,
	(   destroy_visitor(Id)
	->  debug(chat(visitor), 'Left: ~p', [Id])
	;   true
	).
handle_message(Message, _Room) :-
	websocket{opcode:close, client:Id} :< Message, !,
	debug(chat(visitor), 'Left: ~p', [Id]),
	destroy_visitor(Id).
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
		 *	       UI		*
		 *******************************/

%%	notifications(+Options)//

notifications(_Options) -->
	html(ul(class([nav, 'navbar-nav', 'pull-right']),
		[ li(class(dropdown),
		     [ a([ class('dropdown-toggle'),
			   'data-toggle'(dropdown)
			 ],
			 [ span(class([glyphicon, 'glyphicon-user']), []),
			   b(class(caret), [])
			 ]),
		       ul([ class(['dropdown-menu', 'pull-right']),
			    name('Notifications')
			  ],
			  [ li(a('Hello nice world!'))
			  ])
		     ])
		])).
