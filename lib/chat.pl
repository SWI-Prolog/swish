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
	    chat_to_profile/2,		% +ProfileID, :HTML
	    chat_about/2,		% +DocID, +Message

	    notifications//1,		% +Options
	    broadcast_bell//1		% +Options
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
:- if(exists_source(library(user_profile))).
:- use_module(library(user_profile)).
:- endif.
:- use_module(library(aggregate)).

:- use_module(storage).
:- use_module(gitty).
:- use_module(config).
:- use_module(avatar).
:- use_module(noble_avatar).
:- use_module(chatstore).
:- use_module(authenticate).
:- use_module(pep).
:- use_module(content_filter).

:- html_meta(chat_to_profile(+, html)).

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

:- multifile swish_config:config/2.

swish_config:config(hangout, 'Hangout.swinb').


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
	authenticate(Request, Identity),
	start_chat(Request, [identity(Identity)]).

start_chat(Request, Options) :-
	authorized(chat(open), Options),
	(   http_in_session(Session)
	->  CheckLogin = false
	;   http_open_session(Session, []),
	    CheckLogin = true
	),
	check_flooding(Session),
	http_parameters(Request,
			[ avatar(Avatar, [optional(true)]),
			  nickname(NickName, [optional(true)]),
			  reconnect(Token, [optional(true)])
			]),
	extend_options([ avatar(Avatar),
			 nick_name(NickName),
			 reconnect(Token),
			 check_login(CheckLogin)
		       ], Options, ChatOptions),
	debug(chat(websocket), 'Accepting (session ~p)', [Session]),
	http_upgrade_to_websocket(
	    accept_chat(Session, ChatOptions),
	    [ guarded(false),
	      subprotocols(['v1.chat.swish.swi-prolog.org', chat])
	    ],
	    Request).

extend_options([], Options, Options).
extend_options([H|T0], Options, [H|T]) :-
	ground(H), !,
	extend_options(T0, Options, T).
extend_options([_|T0], Options, T) :-
	extend_options(T0, Options, T).


%!	check_flooding(+Session)
%
%	See whether the client associated with  a session is flooding us
%	and if so, return a resource error.

check_flooding(_0Session) :-
	get_time(Now),
	(   http_session_retract(websocket(Score, Last))
	->  Passed is Now-Last,
	    NewScore is Score*(2**(-Passed/60)) + 10
	;   NewScore = 10,
	    Passed = 0
	),
	debug(chat(flooding), 'Flooding score: ~2f (session ~p)',
	      [NewScore, _0Session]),
	http_session_assert(websocket(NewScore, Now)),
	(   NewScore > 50
	->  throw(http_reply(resource_error(
				 websocket(reconnect(Passed, NewScore)))))
	;   true
	).

%!	accept_chat(+Session, +Options, +WebSocket)

accept_chat(Session, Options, WebSocket) :-
	must_succeed(accept_chat_(Session, Options, WebSocket)).

accept_chat_(Session, Options, WebSocket) :-
	create_chat_room,
	(   reconnect_token(WSID, Token, Options),
	    retractall(visitor_status(WSID, lost(_))),
	    existing_visitor(WSID, Session, Token, TmpUser, UserData),
	    hub_add(swish_chat, WebSocket, WSID)
	->  Reason = rejoined
	;   hub_add(swish_chat, WebSocket, WSID),
	    must_succeed(create_visitor(WSID, Session, Token,
					TmpUser, UserData, Options)),
	    Reason = joined
	),
	visitor_count(Visitors),
	option(check_login(CheckLogin), Options, true),
	Msg = _{ type:welcome,
		 uid:TmpUser,
		 wsid:WSID,
		 reconnect:Token,
		 visitors:Visitors,
		 check_login:CheckLogin
	       },
	hub_send(WSID, json(UserData.put(Msg))),
	must_succeed(chat_broadcast(UserData.put(_{type:Reason,
						   visitors:Visitors,
						   wsid:WSID}))),
	gc_visitors,
	debug(chat(websocket), '~w (session ~p, wsid ~p)',
	      [Reason, Session, WSID]).


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
	\+ inactive(WSID, 30).

visitor_count(Count) :-
	aggregate_all(count, visitor(_), Count).

%!	inactive(+WSID, +Timeout) is semidet.
%
%	True if WSID is inactive. This means   we lost the connection at
%	least Timeout seconds ago.

inactive(WSID, Timeout) :-
	visitor_status(WSID, lost(Lost)),
	get_time(Now),
	Now - Lost > Timeout.

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
existing_visitor(WSID, Session, Token, _, _) :-
	retractall(visitor_session(WSID, Session, Token)),
	fail.

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
	->  reclaim_visitor(WSID)
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

%!	gc_visitors
%
%	Reclaim all visitors with whom we   have lost the connection and
%	the browser did not reclaim the selection within 5 minutes.

:- dynamic last_gc/1.

gc_visitors :-
	last_gc(Last),
	get_time(Now),
	Now-Last < 300, !.
gc_visitors :-
	with_mutex(gc_visitors, gc_visitors_sync).

gc_visitors_sync :-
	get_time(Now),
	(   last_gc(Last),
	    Now-Last < 300
	->  true
	;   retractall(last_gc(_)),
	    asserta(last_gc(Now)),
	    do_gc_visitors
	).

do_gc_visitors :-
	forall(( visitor_session(WSID, _Session, _Token),
		 inactive(WSID, 5*60)
	       ),
	       reclaim_visitor(WSID)).

reclaim_visitor(WSID) :-
	debug(chat(gc), 'Reclaiming idle ~p', [WSID]),
	retractall(visitor_session(WSID, _Session, _Token)),
	retractall(visitor_status(WSID, _Status)),
	unsubscribe(WSID, _).


%%	create_session_user(+Session, -User, -UserData, +Options)
%
%	Associate a user with the session. The user id is a UUID that is
%	not associated with  any  persistent  notion   of  a  user.  The
%	destruction is left to the destruction of the session.

:- listen(http_session(end(SessionID, _Peer)),
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
	forall(visitor_session(WSID, Session, _Token),
	       inform_session_closed(WSID, Session)),
	retractall(visitor_session(_, Session, _)),
	forall(retract(session_user(Session, TmpUser)),
	       destroy_visitor_data(TmpUser)).

destroy_visitor_data(TmpUser) :-
	(   retract(visitor_data(TmpUser, Data)),
	    release_avatar(Data.get(avatar)),
	    fail
	;   true
	).

inform_session_closed(WSID, Session) :-
	ignore(hub_send(WSID, json(_{type:session_closed}))),
	session_user(Session, TmpUser),
	update_visitor_data(TmpUser, _Data, logout).


%!	update_visitor_data(+TmpUser, +Data, +Reason) is det.
%
%	Update the user data for the visitor   TmpUser  to Data. This is
%	rather complicates due to all the   defaulting  rules. Reason is
%	one of:
%
%	  - login
%	  - logout
%	  - 'set-nick-name'
%	  - 'profile-edit'
%
%	@tbd Create a more declarative description  on where the various
%	attributes must come from.

update_visitor_data(TmpUser, _Data, logout) :- !,
	anonymise_user_data(TmpUser, NewData),
	set_visitor_data(TmpUser, NewData, logout).
update_visitor_data(TmpUser, Data, Reason) :-
	profile_reason(Reason), !,
	(   visitor_data(TmpUser, Old)
	;   Old = v{}
	),
	copy_profile([name,avatar,email], Data, Old, New),
	set_visitor_data(TmpUser, New, Reason).
update_visitor_data(TmpUser, _{name:Name}, 'set-nick-name') :- !,
	visitor_data(TmpUser, Old),
	set_nick_name(Old, Name, New),
	set_visitor_data(TmpUser, New, 'set-nick-name').
update_visitor_data(TmpUser, Data, Reason) :-
	set_visitor_data(TmpUser, Data, Reason).

profile_reason('profile-edit').
profile_reason('login').

copy_profile([], _, Data, Data).
copy_profile([H|T], New, Data0, Data) :-
	copy_profile_field(H, New, Data0, Data1),
	copy_profile(T, New, Data1, Data).

copy_profile_field(avatar, New, Data0, Data) :-	!,
	(   Data1 = Data0.put(avatar,New.get(avatar))
	->  Data  = Data1.put(avatar_source, profile)
	;   email_gravatar(New.get(email), Avatar),
	    valid_gravatar(Avatar)
	->  Data = Data0.put(_{avatar:Avatar,avatar_source:email})
	;   Avatar = Data0.get(anonymous_avatar)
	->  Data = Data0.put(_{avatar:Avatar,avatar_source:client})
	;   noble_avatar_url(Avatar, []),
	    Data = Data0.put(_{avatar:Avatar,avatar_source:generated,
			       anonymous_avatar:Avatar
			      })
	).
copy_profile_field(email, New, Data0, Data) :- !,
	(   NewMail = New.get(email)
	->  update_avatar_from_email(NewMail, Data0, Data1),
	    Data = Data1.put(email, NewMail)
	;   update_avatar_from_email('', Data0, Data1),
	    (	del_dict(email, Data1, _, Data)
	    ->	true
	    ;	Data = Data1
	    )
	).
copy_profile_field(F, New, Data0, Data) :-
	(   Data = Data0.put(F, New.get(F))
	->  true
	;   del_dict(F, Data0, _, Data)
	->  true
	;   Data = Data0
	).

set_nick_name(Data0, Name, Data) :-
	Data = Data0.put(_{name:Name, anonymous_name:Name}).

%!	update_avatar_from_email(+Email, +DataIn, -Data)
%
%	Update the avatar after a change  of   the  known  email. If the
%	avatar comes from the profile, no action is needed. If Email has
%	a gravatar, use that. Else  use  the   know  or  a new generated
%	avatar.

update_avatar_from_email(_, Data, Data) :-
	Data.get(avatar_source) == profile, !.
update_avatar_from_email('', Data0, Data) :-
	Data0.get(avatar_source) == email, !,
	noble_avatar_url(Avatar, []),
	Data = Data0.put(_{avatar:Avatar, anonymous_avatar:Avatar,
			   avatar_source:generated}).
update_avatar_from_email(Email, Data0, Data) :-
	email_gravatar(Email, Avatar),
	valid_gravatar(Avatar), !,
	Data = Data0.put(avatar, Avatar).
update_avatar_from_email(_, Data0, Data) :-
	(   Avatar = Data0.get(anonymous_avatar)
	->  Data = Data0.put(_{avatar:Avatar, avatar_source:client})
	;   noble_avatar_url(Avatar, []),
	    Data = Data0.put(_{avatar:Avatar, anonymous_avatar:Avatar,
			       avatar_source:generated})
	).

%!	anonymise_user_data(TmpUser, Data)
%
%	Create anonymous user profile.

anonymise_user_data(TmpUser, Data) :-
	visitor_data(TmpUser, Old),
	(   _{anonymous_name:AName, anonymous_avatar:AAvatar} :< Old
	->  Data = _{anonymous_name:AName, anonymous_avatar:AAvatar,
		     name:AName, avatar:AAvatar, avatar_source:client}
	;   _{anonymous_avatar:AAvatar} :< Old
	->  Data = _{anonymous_avatar:AAvatar,
		     avatar:AAvatar, avatar_source:client}
	;   _{anonymous_name:AName} :< Old
	->  noble_avatar_url(Avatar, []),
	    Data = _{anonymous_name:AName, anonymous_avatar:Avatar,
		     name:AName, avatar:Avatar, avatar_source:generated}
	), !.
anonymise_user_data(_, Data) :-
	noble_avatar_url(Avatar, []),
	Data = _{anonymous_avatar:Avatar,
		 avatar:Avatar, avatar_source:generated}.

%!	set_visitor_data(+TmpUser, +Data, +Reason) is det.
%
%	Update the user data for the   session  user TmpUser and forward
%	the changes.

set_visitor_data(TmpUser, Data, Reason) :-
	retractall(visitor_data(TmpUser, _)),
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
	ignore(hub_send(WSID, json(_{type:"gazers", gazers:Gazers}))).

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
%	  - identity(+Identity)
%	  Identity information provided by authenticate/2.  Always
%	  present.
%	  - avatar(+URL)
%	  Avatar provided by the user
%	  - nick_name(+Name)
%	  Nick name provided by the user.
%
%	Data always contains an `avatar` key   and optionally contains a
%	`name` and `email` key. If the avatar is generated there is also
%	a key `avatar_generated` with the value `true`.
%
%	@bug	This may check for avatar validity, which may take
%		long.  Possibly we should do this in a thread.

get_visitor_data(Data, Options) :-
	option(identity(Identity), Options),
	findall(N-V, visitor_property(Identity, Options, N, V), Pairs),
	dict_pairs(Data, v, Pairs).

visitor_property(Identity, Options, name, Name) :-
	(   user_property(Identity, name(Name))
	->  true
	;   option(nick_name(Name), Options)
	).
visitor_property(Identity, _, email, Email) :-
	user_property(Identity, email(Email)).
visitor_property(Identity, Options, Name, Value) :-
	(   user_property(Identity, avatar(Avatar))
	->  avatar_property(Avatar, profile, Name, Value)
	;   user_property(Identity, email(Email)),
	    email_gravatar(Email, Avatar),
	    valid_gravatar(Avatar)
	->  avatar_property(Avatar, email, Name, Value)
	;   option(avatar(Avatar), Options)
	->  avatar_property(Avatar, client, Name, Value)
	;   noble_avatar_url(Avatar, Options),
	    avatar_property(Avatar, generated, Name, Value)
	).
visitor_property(_, Options, anonymous_name, Name) :-
	option(nick_name(Name), Options).
visitor_property(_, Options, anonymous_avatar, Avatar) :-
	option(avatar(Avatar), Options).


avatar_property(Avatar, _Source, avatar,        Avatar).
avatar_property(_Avatar, Source, avatar_source, Source).


		 /*******************************
		 *	   NOBLE AVATAR		*
		 *******************************/

:- http_handler(swish('avatar/'), reply_avatar, [id(avatar), prefix]).

%%	reply_avatar(+Request)
%
%	HTTP handler for Noble  Avatar   images.  Using  create_avatar/2
%	re-creates avatars from the file name,  so we can safely discard
%	the avatar file store.

reply_avatar(Request) :-
	option(path_info(Local), Request),
	(   absolute_file_name(noble_avatar(Local), Path,
			       [ access(read),
				 file_errors(fail)
			       ])
	->  true
	;   create_avatar(Local, Path)
	),
	http_reply_file(Path, [unsafe(true)], Request).


noble_avatar_url(HREF, Options) :-
	option(avatar(HREF), Options), !.
noble_avatar_url(HREF, _Options) :-
	noble_avatar(_Gender, Path, true),
	file_base_name(Path, File),
	http_absolute_location(swish(avatar/File), HREF, []).


		 /*******************************
		 *	   BROADCASTING		*
		 *******************************/

%%	chat_broadcast(+Message) is det.
%%	chat_broadcast(+Message, +Channel) is det.
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
subscribed(gitty, SubChannel, WSID) :-
	swish_config:config(hangout, SubChannel),
	\+ subscription(WSID, gitty, SubChannel).


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
json_message(Dict, WSID) :-
	_{type: "chat-message", docid:DocID} :< Dict, !,
	chat_add_user_id(WSID, Dict, Message),
	(   forbidden(Message, DocID, Why)
	->  hub_send(WSID, json(json{type:forbidden,
				     action:chat_post,
				     about:DocID,
				     message:Why
				    }))
	;   chat_relay(Message)
	).
json_message(Dict, _WSID) :-
	debug(chat(ignored), 'Ignoring JSON message ~p', [Dict]).

dict_file_name(Dict, File) :-
	atom_string(File, Dict.get(file)).

%!	forbidden(+Message, +DocID, -Why) is semidet.
%
%	True if the chat Message about DocID must be forbidden, in which
%	case Why is  unified  with  a   string  indicating  the  reason.
%	Currently:
%
%	  - Demands the user to be logged on
%	  - Limits the size of the message and its payloads

forbidden(Message, DocID, Why) :-
	\+ ws_authorized(chat(post(Message, DocID)), Message.user), !,
	Why = "Due to frequent spamming we were forced to limit \c
	       posting chat messages to users who are logged in.".
forbidden(Message, _DocID, Why) :-
	Text = Message.get(text),
	string_length(Text, Len),
	Len > 500,
	Why = "Chat messages are limited to 500 characters".
forbidden(Message, _DocID, Why) :-
	Payloads = Message.get(payload),
	member(Payload, Payloads),
	large_payload(Payload, Why), !.
forbidden(Message, _DocID, Why) :-
	eval_content(Message.get(text), _WC, Score),
	user_score(Message, Score, Cummulative, _Count),
	Score*2 + Cummulative < 0,
	!,
	Why = "Chat messages must be in English and avoid offensive language".

large_payload(Payload, Why) :-
	Selections = Payload.get(selection),
	member(Selection, Selections),
	(   string_length(Selection.get(string), SelLen), SelLen > 500
	;   string_length(Selection.get(context), SelLen), SelLen > 500
	), !,
	Why = "Selection too long (max. 500 characters)".
large_payload(Payload, Why) :-
	string_length(Payload.get(query), QLen), QLen > 1000, !,
	Why = "Query too long (max. 1000 characters)".

user_score(Message, MsgScore, Cummulative, Count) :-
	Profile	= Message.get(user).get(profile_id), !,
	block(Profile, MsgScore, Cummulative, Count).
user_score(_, _, 0, 1).

%!	block(+User, +Score, -Cummulative, -Count)
%
%	Keep a count and cummulative score for a user.

:- dynamic
	blocked/4.

block(User, Score, Cummulative, Count) :-
	blocked(User, Score0, Count0, Time), !,
	get_time(Now),
	Cummulative = Score0*(0.5**((Now-Time)/600)) + Score,
	Count is Count0 + 1,
	asserta(blocked(User, Cummulative, Count, Now)),
	retractall(blocked(User, Score0, Count0, Time)).
block(User, Score, Score, 1) :-
	get_time(Now),
	asserta(blocked(User, Score, 1, Now)).


		 /*******************************
		 *	   CHAT MESSAGES	*
		 *******************************/

%!	chat_add_user_id(+WSID, +Message0, -Message) is det.
%
%	Decorate a message with the user credentials.

chat_add_user_id(WSID, Dict, Message) :-
	visitor_session(WSID, Session, _Token),
	session_user(Session, Visitor),
	visitor_data(Visitor, UserData),
	User0 = u{avatar:UserData.avatar,
		  wsid:WSID
		 },
	(   Name = UserData.get(name)
	->  User1 = User0.put(name, Name)
	;   User1 = User0
	),
	(   http_current_session(Session, profile_id(ProfileID))
	->  User = User1.put(profile_id, ProfileID)
	;   User = User1
	),
	Message = Dict.put(user, User).


%!	chat_about(+DocID, +Message) is det.
%
%	Distribute a chat message about DocID.

chat_about(DocID, Message) :-
	chat_relay(Message.put(docid, DocID)).

%!	chat_relay(+Message) is det.
%
%	Store and relay a chat message.

chat_relay(Message) :-
	chat_enrich(Message, Message1),
	chat_send(Message1).

%!	chat_enrich(+Message0, -Message) is det.
%
%	Add time and identifier to the chat message.

chat_enrich(Message0, Message) :-
	get_time(Now),
	uuid(ID),
	Message = Message0.put(_{time:Now, id:ID}).

%!	chat_send(+Message)
%
%	Relay the chat message Message. If  the message has a `volatile`
%	property it is broadcasted, but not stored.

chat_send(Message) :-
	atom_concat("gitty:", File, Message.docid),
	broadcast(swish(chat(Message))),
	(   Message.get(volatile) == true
	->  true
	;   chat_store(Message)
	),
	chat_broadcast(Message, gitty/File).


		 /*******************************
		 *	      EVENTS		*
		 *******************************/

:- unlisten(swish(_)),
   listen(swish(Event), chat_event(Event)).

%%	chat_event(+Event) is semidet.
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

chat_event(Event) :-
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
chat_event(logout(_ProfileID)) :- !,
	http_session_id(Session),
	session_user(Session, User),
	update_visitor_data(User, _, logout).
chat_event(visitor_count(Count)) :-		% request
	visitor_count(Count).

:- if(current_predicate(current_profile/2)).

chat_event(profile(ProfileID)) :- !,
	current_profile(ProfileID, Profile),
	http_session_id(Session),
	session_user(Session, User),
	update_visitor_data(User, Profile, login).

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

:- endif.

%%	broadcast_event(+Event) is semidet.
%
%	If true, broadcast this event.

broadcast_event(updated(_File, _From, _To)).


%%	broadcast_event(+Event, +File, +WSID) is det.
%
%	Event happened that is related to File  in WSID. Broadcast it to
%	subscribed users as a notification. Always succeeds, also if the
%	message cannot be delivered.
%
%	@tbd	Extend the structure to allow other browsers to act.

broadcast_event(Event, File, WSID) :-
	visitor_session(WSID, Session),
	session_broadcast_event(Event, File, Session, WSID), !.
broadcast_event(_, _, _).

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

event_file(created(File, _Commit), File).
event_file(updated(File, _Commit), File).
event_file(deleted(File, _Commit), File).
event_file(download(Store, FileOrHash, _Format), File) :-
	(   is_gitty_hash(FileOrHash)
	->  gitty_commit(Store, FileOrHash, Meta),
	    File = Meta.name
	;   File = FileOrHash
	).


		 /*******************************
		 *	   NOTIFICATION		*
		 *******************************/

%!	chat_to_profile(ProfileID, :HTML) is det.
%
%	Send a HTML notification to users logged in using ProfileID.

chat_to_profile(ProfileID, HTML) :-
	(   http_current_session(Session, profile_id(ProfileID)),
	    visitor_session(WSID, Session),
	    html_string(HTML, String),
	    hub_send(WSID, json(_{ wsid:WSID,
				   type:notify,
				   html:String
				 })),
	    debug(notify(chat), 'Notify to ~p: ~p', [ProfileID, String]),
	    fail
	;   true
	).

html_string(HTML, String) :-
	phrase(html(HTML), Tokens),
	delete(Tokens, nl(_), SingleLine),
	with_output_to(string(String), print_html(SingleLine)).




		 /*******************************
		 *	       UI		*
		 *******************************/

%%	notifications(+Options)//
%
%	The  chat  element  is  added  to  the  navbar  and  managed  by
%	web/js/chat.js

notifications(_Options) -->
	{ swish_config:config(chat, true) }, !,
	html(div(class(chat),
		 [ div(class('chat-users'),
		       ul([ class([nav, 'navbar-nav', 'pull-right']),
			    id(chat)
			  ], [])),
		   div(class('user-count'),
		       [ span(id('user-count'), '?'),
			 ' users online'
		       ])
		 ])).
notifications(_Options) -->
	[].

%!	broadcast_bell(+Options)//
%
%	Adds a bell to indicate central chat messages

broadcast_bell(_Options) -->
	{ swish_config:config(chat, true),
	  swish_config:config(hangout, Hangout),
	  atom_concat('gitty:', Hangout, HangoutID)
	}, !,
	html([ a([ class(['dropdown-toggle', 'broadcast-bell']),
		   'data-toggle'(dropdown)
		 ],
		 [ span([ id('broadcast-bell'),
			  'data-document'(HangoutID)
			], []),
		   b(class(caret), [])
		 ]),
	       ul([ class(['dropdown-menu', 'pull-right']),
		    id('chat-menu')
		  ],
		  [ li(a('data-action'('chat-shared'),
			 'Open hangout')),
		    li(a('data-action'('chat-about-file'),
			 'Open chat for current file'))
		  ])
	     ]).
broadcast_bell(_Options) -->
	[].


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(websocket(reconnect(Passed, Score))) -->
	[ 'WebSocket: too frequent reconnect requests (~1f sec; score = ~1f)'-
	  [Passed, Score] ].
