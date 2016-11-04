/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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
	    chat_broadcast/2		% +Message, +Channel
	  ]).
:- use_module(library(http/hub)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/json)).
:- use_module(library(debug)).

:- use_module(config).

/** <module> The SWISH collaboration backbone


*/

:- multifile
	swish_config:config/2.

swish_config:config(chat, true).

:- http_handler(swish(chat),
		http_upgrade_to_websocket(
		    accept_chat,
		    [ guarded(false),
		      subprotocols([chat])
		    ]),
		[ id(swish_chat)
		]).

accept_chat(WebSocket) :-
	create_chat_room,
	hub_add(swish_chat, WebSocket, _Id).


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

subscribed(Channel, Id) :-
	subscription(Id, Channel, _).
subscribed(Channel, SubChannel, Id) :-
	subscription(Id, Channel, SubChannel).

		 /*******************************
		 *	        DATA		*
		 *******************************/

:- dynamic
	visitor/2,			% Id, Joined
	subscription/3.			% Id, Channel, SubChannel

create_visitor(Id) :-
	get_time(Time),
	assertz(visitor(Id, Time)).

destroy_visitor(Id) :-
	retract(visitor(Id, _)),
	retractall(subscription(Id, _, _)).


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
	(   catch(swish_chat_event(Room), E,
		  print_message(warning, E))
	->  true
	;   true			% warning?
	),
	swish_chat(Room).

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
	debug(chat(visitor), 'Joined: ~p', [Id]),
	create_visitor(Id).
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
	assertz(subscription(Client, Channel, SubChannel)).
json_message(Dict, Client) :-
	_{type: "subscribe", channel:ChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	assertz(subscription(Client, Channel, _)).
json_message(Dict, Client) :-
	_{ type: "unsubscribe",
	   channel:ChannelS, sub_channel:SubChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	atom_string(SubChannel, SubChannelS),
	retractall(subscription(Client, Channel, SubChannel)).
json_message(Dict, Client) :-
	_{type: "unsubscribe", channel:ChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	retractall(subscription(Client, Channel, _)).
