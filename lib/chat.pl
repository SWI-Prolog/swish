/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016-2017, VU University Amsterdam
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
:- use_module(library(http/http_parameters)).
:- use_module(library(http/websocket)).
:- use_module(library(http/json)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(uuid)).
:- use_module(library(random)).
:- use_module(library(base64)).
:- use_module(library(apply)).
:- use_module(library(broadcast)).
:- use_module(library(ordsets)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(user_profile)).
:- use_module(library(aggregate)).

:- use_module(storage).
:- use_module(gitty).
:- use_module(config).
:- use_module(login).
:- use_module(avatar).
:- use_module(noble_avatar).

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

:- meta_predicate must_succeed(0).

%!	start_chat(+Request)
%
%	HTTP handler that establishes  a   websocket  connection where a
%	user gets an avatar and optionally a name.

start_chat(Request) :-
	swish_config:authenticate(Request, _User), !, % must throw to deny access
	start_chat(Request, []).
start_chat(Request) :-
	start_chat(Request, []).

start_chat(Request, Options) :-
	(   current_user_info(Request, Info)
	->  ChatOptions = [current_user_info(Info)|Options1]
	;   ChatOptions = Options1
	),
	http_open_session(Session, []),
	http_parameters(Request,
			[ avatar(Avatar, [optional(true)]),
			  nickname(NickName, [optional(true)]),
			  reconnect(Token, [optional(true)])
			]),
	extend_options([ avatar(Avatar),
			 nick_name(NickName),
			 reconnect(Token)
		       ], Options, Options1),
	http_upgrade_to_websocket(
	    accept_chat(Session, ChatOptions),
	    [ guarded(false),
	      subprotocols([chat])
	    ],
	    Request).

extend_options([], Options, Options).
extend_options([H|T0], Options, [H|T]) :-
	ground(H), !,
	extend_options(T0, Options, T).
extend_options([_|T0], Options, T) :-
	extend_options(T0, Options, T).


%!	accept_chat(+Session, +Options, +WebSocket)

accept_chat(Session, Options, WebSocket) :-
	must_succeed(accept_chat_(Session, Options, WebSocket)).

accept_chat_(Session, Options, WebSocket) :-
	create_chat_room,
	(   reconnect_token(WSID, Token, Options),
	    existing_visitor(WSID, Session, Token, TmpUser, UserData)
	->  Reason = rejoined
	;   must_succeed(create_visitor(WSID, Session, Token,
					TmpUser, UserData, Options)),
	    Reason = joined
	),
	hub_add(swish_chat, WebSocket, WSID),
	visitor_count(Visitors),
	Msg = _{ type:welcome,
		 uid:TmpUser,
		 wsid:WSID,
		 reconnect:Token,
		 visitors:Visitors
	       },
	hub_send(WSID, json(UserData.put(Msg))),
	must_succeed(chat_broadcast(UserData.put(_{type:Reason,
						   visitors:Visitors,
						   wsid:WSID}))).

reconnect_token(WSID, Token, Options) :-
	option(reconnect(Token), Options),
	visitor_session(WSID, _, Token), !.

must_succeed(Goal) :-
	catch(Goal, E, print_message(warning, E)), !.
must_succeed(Goal) :-
	print_message(warning, goal_failed(Goal)).


		 /*******************************
		 *	        DATA		*
		 *******************************/

%%	visitor_session(?WSId, ?Session, ?Token).
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
	visitor_status/2,		% WSID, Status
	visitor_session/3,		% WSID, Session, Token
	session_user/2,			% Session, TmpUser
	visitor_data/2,			% TmpUser, Data
	subscription/3.			% WSID, Channel, SubChannel

%!	visitor(?WSID) is nondet
%
%	True when WSID should be considered an active visitor.

visitor(WSID) :-
	visitor_session(WSID, _Session, _Token),
	\+ inactive(WSID).

visitor_count(Count) :-
	aggregate_all(count, visitor(_), Count).

%!	inactive(+WSID) is semidet.
%
%	True if WSID is inactive. This means   we lost the connection at
%	least 5 minutes ago.

inactive(WSID) :-
	visitor_status(WSID, lost(Lost)),
	get_time(Now),
	Now - Lost > 5*60.

%!	visitor_session(?WSID, ?Session) is nondet.
%
%	True if websocket WSID is associated with Session.

visitor_session(WSID, Session) :-
	visitor_session(WSID, Session, _Token).

%!	wsid_visitor(?WSID, ?Visitor)
%
%	True when WSID is associated with Visitor

wsid_visitor(WSID, Visitor) :-
	nonvar(WSID), !,
	visitor_session(WSID, Session),
	session_user(Session, Visitor).
wsid_visitor(WSID, Visitor) :-
	session_user(Session, Visitor),
	visitor_session(WSID, Session).


%!	existing_visitor(+WSID, +Session, +Token, -TmpUser, -UserData) is semidet.
%
%	True if we are dealing with  an   existing  visitor for which we
%	lost the connection.

existing_visitor(WSID, Session, Token, TmpUser, UserData) :-
	visitor_session(WSID, Session, Token),
	session_user(Session, TmpUser),
	visitor_data(TmpUser, UserData), !.

%%	create_visitor(+WSID, +Session, ?Token, -TmpUser, -UserData, +Options)
%
%	Create a new visitor  when  a   new  websocket  is  established.
%	Options provides information we have about the user:
%
%	  - current_user_info(+Info)
%	  Already logged in user with given information
%	  - avatar(Avatar)
%	  Avatar remembered in the browser for this user.
%	  - nick_name(NickName)
%	  Nick name remembered in the browser for this user.

create_visitor(WSID, Session, Token, TmpUser, UserData, Options) :-
	generate_key(Token),
	assertz(visitor_session(WSID, Session, Token)),
	create_session_user(Session, TmpUser, UserData, Options).

%!  generate_key(-Key) is det.
%
%   Generate a random confirmation key

generate_key(Key) :-
	length(Codes, 16),
	maplist(random_between(0,255), Codes),
	phrase(base64url(Codes), Encoded),
	atom_codes(Key, Encoded).

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
	destroy_reason(WSID, Reason),
	(   Reason == unload
	->  retract(visitor_session(WSID, _Session, _Token))
	;   get_time(Now),
	    assertz(visitor_status(WSID, lost(Now)))
	),
	visitor_count(Count),
	chat_broadcast(_{ type:removeUser,
			  wsid:WSID,
			  reason:Reason,
			  visitors:Count
			}).

destroy_reason(WSID, Reason) :-
	retract(visitor_status(WSID, unload)), !,
	Reason = unload.
destroy_reason(_, close).

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

%!	update_visitor_data(+TmpUser, +Data, +Reason) is det.
%
%	Update the user data for the visitor TmpUser to Data.

update_visitor_data(TmpUser, Data, Reason) :-
	retract(visitor_data(TmpUser, Old)), !,
	(   _ = Old.get(anon_avatar)
	->  Old1 = Old
	;   OldAvarat = Old.get(avatar)
	->  Old1 = Old.put(anon_avatar, OldAvarat)
	;   Old1 = Old
	),
	set_visitor_data(TmpUser, Old1.put(Data), Reason).
update_visitor_data(TmpUser, Data, Reason) :-
	set_visitor_data(TmpUser, Data, Reason).

%!	update_visitor_data(+TmpUser) is det.
%
%	Update visitor data after a logout

update_visitor_data(TmpUser) :-
	retract(visitor_data(TmpUser, Old)), !,
	(   OldAvarat = Old.anon_avatar
	->  true
	;   noble_avatar_url(OldAvarat, [])
	),
	set_visitor_data(TmpUser, v{avatar:OldAvarat}, 'logout').
update_visitor_data(TmpUser) :-
	noble_avatar_url(OldAvarat, []),
	set_visitor_data(TmpUser, v{avatar:OldAvarat}, 'logout').

set_visitor_data(TmpUser, Data, Reason) :-
	assertz(visitor_data(TmpUser, Data)),
	inform_visitor_change(TmpUser, Reason).

%!	inform_visitor_change(+TmpUser, +Reason) is det.
%
%	Inform browsers showing  TmpUser  that   the  visitor  data  has
%	changed. The first  clause  deals   with  forwarding  from  HTTP
%	requests,  where  we  have  the  session  and  the  second  from
%	websocket requests where we have the WSID.

inform_visitor_change(TmpUser, Reason) :-
	http_in_session(Session), !,
	public_user_data(TmpUser, Data),
	forall(visitor_session(WSID, Session),
	       inform_friend_change(WSID, Data, Reason)).
inform_visitor_change(TmpUser, Reason) :-
	b_getval(wsid, WSID),
	public_user_data(TmpUser, Data),
	inform_friend_change(WSID, Data, Reason).

inform_friend_change(WSID, Data, Reason) :-
	Message = json(_{ type:"profile",
			  wsid:WSID,
			  reason:Reason
			}.put(Data)),
	hub_send(WSID, Message),
	forall(viewing_same_file(WSID, Friend),
	       ignore(hub_send(Friend, Message))).

viewing_same_file(WSID, Friend) :-
	subscription(WSID, gitty, File),
	subscription(Friend, gitty, File),
	Friend \== WSID.

%%	subscribe(+WSID, +Channel) is det.

subscribe(WSID, Channel) :-
	subscribe(WSID, Channel, _SubChannel).
subscribe(WSID, Channel, SubChannel) :-
	(   subscription(WSID, Channel, SubChannel)
	->  true
	;   assertz(subscription(WSID, Channel, SubChannel))
	).

unsubscribe(WSID, Channel) :-
	unsubscribe(WSID, Channel, _SubChannel).
unsubscribe(WSID, Channel, SubChannel) :-
	retractall(subscription(WSID, Channel, SubChannel)).

%%	sync_gazers(+WSID, +Files:list(atom)) is det.
%
%	A browser signals it has Files open.   This happens when a SWISH
%	instance is created as well  as   when  a SWISH instance changes
%	state, such as closing a tab, adding   a  tab, bringing a tab to
%	the foreground, etc.

sync_gazers(WSID, Files0) :-
	findall(F, subscription(WSID, gitty, F), Viewing0),
	sort(Files0, Files),
	sort(Viewing0, Viewing),
	(   Files == Viewing
	->  true
	;   ord_subtract(Files, Viewing, New),
	    add_gazing(WSID, New),
	    ord_subtract(Viewing, Files, Left),
	    del_gazing(WSID, Left)
	).

add_gazing(_, []) :- !.
add_gazing(WSID, Files) :-
	inform_me_about_existing_gazers(WSID, Files),
	inform_existing_gazers_about_newby(WSID, Files).

inform_me_about_existing_gazers(WSID, Files) :-
	findall(Gazer, files_gazer(Files, Gazer), Gazers),
	hub_send(WSID, json(_{type:"gazers", gazers:Gazers})).

files_gazer(Files, Gazer) :-
	member(File, Files),
	subscription(WSID, gitty, File),
	visitor_session(WSID, Session),
	session_user(Session, UID),
	public_user_data(UID, Data),
	Gazer = _{file:File, uid:UID, wsid:WSID}.put(Data).

inform_existing_gazers_about_newby(WSID, Files) :-
	forall(member(File, Files),
	       signal_gazer(WSID, File)).

signal_gazer(WSID, File) :-
	subscribe(WSID, gitty, File),
	broadcast_event(opened(File), File, WSID).

del_gazing(_, []) :- !.
del_gazing(WSID, Files) :-
	forall(member(File, Files),
	       del_gazing1(WSID, File)).

del_gazing1(WSID, File) :-
	broadcast_event(closed(File), File, WSID),
	unsubscribe(WSID, gitty, File).

%%	add_user_details(+Message, -Enriched) is det.
%
%	Add additional information to a message.  Message must
%	contain a `uid` field.

add_user_details(Message, Enriched) :-
	public_user_data(Message.uid, Data),
	Enriched = Message.put(Data).

%%	public_user_data(+UID, -Public:dict) is det.
%
%	True when Public provides the   information  we publically share
%	about UID. This is currently the name and avatar.

public_user_data(UID, Public) :-
	visitor_data(UID, Data),
	(   _{name:Name, avatar:Avatar} :< Data
	->  Public = _{name:Name, avatar:Avatar}
	;   _{avatar:Avatar} :< Data
	->  Public = _{avatar:Avatar}
	;   Public = _{}
	).

%%	get_visitor_data(-Data:dict, +Options) is det.
%
%	Optain data for a new visitor.  Options include:
%
%	  - current_user_info(+InfoDict)
%	  Info as provided by current_user_info/2.
%	  - avatar(+URL)
%	  Possibly saved avatar
%
%	Data always contains an `avatar` key   and optionally contains a
%	`name` and `email` key. If the avatar is generated there is also
%	a key `avatar_generated` with the value `true`.
%
%	@bug	This may check for avatar validity, which may take
%		long.  Possibly we should do this in a thread.

get_visitor_data(Data, Options) :-
	option(current_user_info(UserData), Options, _{}),
	findall(N-V, visitor_property(UserData, Options, N, V), Pairs),
	dict_pairs(Data, v, Pairs).

visitor_property(UserData, Options, name, Name) :-
	(   Name = UserData.get(name)
	->  true
	;   Name = UserData.get(user)
	->  true
	;   option(nick_name(Name), Options)
	).
visitor_property(UserData, _, email, Email) :-
	Email = UserData.get(email).
visitor_property(UserData, Options, Name, Value) :-
	(   Avatar = UserData.get(avatar)
	->  Name = avatar, Value = Avatar
	;   Email = UserData.get(email),
	    email_gravatar(Email, Avatar),
	    valid_gravatar(Avatar)
	->  Name = avatar, Value = Avatar
	;   (   option(avatar(Avatar), Options)
	    ->  true
	    ;   noble_avatar_url(Avatar, Options)
	    )
	->  (   Name = avatar, Value = Avatar
	    ;	Name = avatar_generated, Value = true
	    )
	).


		 /*******************************
		 *	   NOBLE AVATAR		*
		 *******************************/

:- http_handler(swish(avatar), reply_avatar, [prefix]).

%%	reply_avatar(+Request)
%
%	HTTP handler for Noble Avatar images.

reply_avatar(Request) :-
	option(path_info(Local), Request),
	http_reply_file(noble_avatar(Local), [], Request).

noble_avatar_url(HREF, Options) :-
	option(avatar(HREF), Options), !.
noble_avatar_url(HREF, _Options) :-
	noble_avatar(_Gender, Path, true),
	file_base_name(Path, File),
	http_absolute_location(swish(avatar/File), HREF, []).


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
	(   handle_message(Message, Room)
	->  true
	;   print_message(warning, goal_failed(handle_message(Message, Room)))
	).

%%	handle_message(+Message, +Room)
%
%	Handle incoming messages

handle_message(Message, _Room) :-
	websocket{opcode:text} :< Message, !,
	atom_json_dict(Message.data, JSON, []),
	debug(chat(received), 'Received from ~p: ~p', [Message.client, JSON]),
	WSID = Message.client,
	setup_call_cleanup(
	    b_setval(wsid, WSID),
	    json_message(JSON, WSID),
	    nb_delete(wsid)).
handle_message(Message, _Room) :-
	hub{joined:WSID} :< Message, !,
	debug(chat(visitor), 'Joined: ~p', [WSID]).
handle_message(Message, _Room) :-
	hub{left:WSID, reason:write(Lost)} :< Message, !,
	(   destroy_visitor(WSID)
	->  debug(chat(visitor), 'Left ~p due to write error for ~p',
		  [WSID, Lost])
	;   true
	).
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


%%	json_message(+Message, +WSID) is det.
%
%	Process a JSON message  translated  to   a  dict.  The following
%	messages are understood:
%
%	  - subscribe channel [subchannel]
%	  - unsubscribe channel [subchannel]
%	  Actively (un)subscribe for specific message channels.
%	  - unload
%	  A SWISH instance is cleanly being unloaded.
%	  - has-open-files files
%	  Executed after initiating the websocket to indicate loaded
%	  files.
%	  - set-nick-name name
%	  User set nick name for anonymous identoty

json_message(Dict, WSID) :-
	_{ type: "subscribe",
	   channel:ChannelS, sub_channel:SubChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	atom_string(SubChannel, SubChannelS),
	subscribe(WSID, Channel, SubChannel).
json_message(Dict, WSID) :-
	_{type: "subscribe", channel:ChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	subscribe(WSID, Channel).
json_message(Dict, WSID) :-
	_{ type: "unsubscribe",
	   channel:ChannelS, sub_channel:SubChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	atom_string(SubChannel, SubChannelS),
	unsubscribe(WSID, Channel, SubChannel).
json_message(Dict, WSID) :-
	_{type: "unsubscribe", channel:ChannelS} :< Dict, !,
	atom_string(Channel, ChannelS),
	unsubscribe(WSID, Channel).
json_message(Dict, WSID) :-
	_{type: "unload"} :< Dict, !,	% clean close/reload
	sync_gazers(WSID, []),
	assertz(visitor_status(WSID, unload)).
json_message(Dict, WSID) :-
	_{type: "has-open-files", files:FileDicts} :< Dict, !,
	maplist(dict_file_name, FileDicts, Files),
	sync_gazers(WSID, Files).
json_message(Dict, WSID) :-
	_{type: "reloaded", file:FileS, commit:Hash} :< Dict, !,
	atom_string(File, FileS),
	event_html(reloaded(File), HTML),
	Message = _{ type:notify,
		     wsid:WSID,
		     html:HTML,
		     event:reloaded,
		     argv:[File,Hash]
		   },
	chat_broadcast(Message, gitty/File).
json_message(Dict, WSID) :-
	_{type: "set-nick-name", name:Name} :< Dict, !,
	wsid_visitor(WSID, Visitor),
	update_visitor_data(Visitor, _{name:Name}, 'set-nick-name').
json_message(Dict, _WSID) :-
	_{type: "chat-message", users:Users} :< Dict, !,
	del_dict(users, Dict, _, Message),
	forall(member(User, Users),
	       send_chat(User, Message)).
json_message(Dict, _WSID) :-
	debug(chat(ignored), 'Ignoring JSON message ~p', [Dict]).

dict_file_name(Dict, File) :-
	atom_string(File, Dict.get(file)).


		 /*******************************
		 *	   CHAT MESSAGES	*
		 *******************************/

%!	send_chat(+Self, +User, +Message)
%
%	Relay a chat message to a user.

send_chat(User, Message) :-
	atom_string(WSID, User.get(id)),
	debug(chat(chat), 'Forwarding to ~p: ~p', [WSID, Message]),
	ignore(hub_send(WSID, json(Message))).



		 /*******************************
		 *	      EVENTS		*
		 *******************************/

:- unlisten(swish(_)),
   listen(swish(Event), swish_event(Event)).

%%	swish_event(+Event) is semidet.
%
%	Event happened inside SWISH.  Currently triggered events:
%
%	  - updated(+File, +From, +To)
%	  File was updated from hash From to hash To.
%	  - profile(+ProfileID)
%	  Session was associated with user with profile ProfileID
%	  - logout(+ProfileID)
%	  User logged out. If the login was based on HTTP authentication
%	  ProfileID equals `http`.

swish_event(Event) :-
	broadcast_event(Event),
	http_session_id(Session),
	debug(event, 'Event: ~p, session ~q', [Event, Session]),
	event_file(Event, File), !,
	(   visitor_session(WSID, Session),
	    subscription(WSID, gitty, File)
	->  true
	;   visitor_session(WSID, Session)
	->  true
	;   WSID = undefined
	),
	session_broadcast_event(Event, File, Session, WSID).
swish_event(profile(ProfileID)) :- !,
	current_profile(ProfileID, Profile),
	http_session_id(Session),
	session_user(Session, User),
	update_visitor_data(User, Profile, 'login').
swish_event(logout(_ProfileID)) :- !,
	http_session_id(Session),
	session_user(Session, User),
	update_visitor_data(User).

%!	propagate_profile_change(+ProfileID, +Attribute, +Value)
%
%	Trap external changes to the profile.

:- listen(user_profile(modified(ProfileID, Name, _Old, New)),
          propagate_profile_change(ProfileID, Name, New)).

propagate_profile_change(ProfileID, _, _) :-
	http_current_session(Session, profile_id(ProfileID)),
	session_user(Session, User),
	current_profile(ProfileID, Profile),
	update_visitor_data(User, Profile, 'profile-edit').


%%	broadcast_event(+Event) is semidet.
%
%	If true, broadcast this event.

broadcast_event(updated(_File, _From, _To)).


%%	broadcast_event(+Event, +File, +WSID)
%
%	Event happened that is related to  File in WSID. Broadcast it
%	to subscribed users as a notification.
%
%	@tbd	Extend the structure to allow other browsers to act.

broadcast_event(Event, File, WSID) :-
	visitor_session(WSID, Session),
	session_broadcast_event(Event, File, Session, WSID).

session_broadcast_event(Event, File, Session, WSID) :-
	session_user(Session, UID),
	event_html(Event, HTML),
	Event =.. [EventName|Argv],
	Message0 = _{ type:notify,
		      uid:UID,
		      html:HTML,
		      event:EventName,
		      event_argv:Argv,
		      wsid:WSID
		    },
	add_user_details(Message0, Message),
	chat_broadcast(Message, gitty/File).

%%	event_html(+Event, -HTML:string) mis det.
%
%	Describe an event as an HTML  message   to  be  displayed in the
%	client's notification area.

event_html(Event, HTML) :-
	(   phrase(event_message(Event), Tokens)
	->  true
	;   phrase(html('Unknown-event: ~p'-[Event]), Tokens)
	),
	delete(Tokens, nl(_), SingleLine),
	with_output_to(string(HTML), print_html(SingleLine)).

event_message(created(File)) -->
	html([ 'Created ', \file(File) ]).
event_message(reloaded(File)) -->
	html([ 'Reloaded ', \file(File) ]).
event_message(updated(File, _From, _To)) -->
	html([ 'Saved ', \file(File) ]).
event_message(deleted(File, _From, _To)) -->
	html([ 'Deleted ', \file(File) ]).
event_message(closed(File)) -->
	html([ 'Closed ', \file(File) ]).
event_message(opened(File)) -->
	html([ 'Opened ', \file(File) ]).
event_message(download(File)) -->
	html([ 'Opened ', \file(File) ]).
event_message(download(Store, FileOrHash, _Format)) -->
	{ event_file(download(Store, FileOrHash), File)
	},
	html([ 'Opened ', \file(File) ]).

file(File) -->
	html(a(href('/p/'+File), File)).

%%	event_file(+Event, -File) is semidet.
%
%	True when Event is associated with File.

event_file(created(File), File).
event_file(updated(File, _From, _To), File).
event_file(deleted(File, _From, _To), File).
event_file(download(Store, FileOrHash, _Format), File) :-
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
	html(div(class(chat),
		 [ ul([ class([nav, 'navbar-nav', 'pull-right']),
			id(chat)
		      ], []),
		   div(class('user-count'),
		       [ span(id('user-count'), '?'),
			 ' users'
		       ])
		 ])).
