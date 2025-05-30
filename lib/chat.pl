/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016-2024, VU University Amsterdam
                              CWI Amsterdam
                              SWI-Prolog Solutions b.v.
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
          [ chat_broadcast/1,           % +Message
            chat_broadcast/2,           % +Message, +Channel
            chat_to_profile/2,          % +ProfileID, :HTML
            chat_about/2,               % +DocID, +Message

            notifications//1,           % +Options
            broadcast_bell//1           % +Options
          ]).
:- use_module(library(http/hub)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_cors)).
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
:- use_module(library(redis)).
:- use_module(library(solution_sequences)).

:- use_module(storage).
:- use_module(gitty).
:- use_module(config).
:- use_module(avatar).
:- use_module(noble_avatar).
:- use_module(chatstore).
:- use_module(authenticate).
:- use_module(pep).
:- use_module(content_filter).
:- use_module(swish_redis).

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
swish_config:config(avatars, svg).              % or 'noble'
swish_config:config(session_lost_timeout, 300).


                 /*******************************
                 *      ESTABLISH WEBSOCKET     *
                 *******************************/

:- http_handler(swish(chat), start_chat, [ id(swish_chat) ]).

:- meta_predicate must_succeed(0).

%!  start_chat(+Request)
%
%   HTTP handler that establishes  a   websocket  connection where a
%   user gets an avatar and optionally a name.

start_chat(Request) :-
    memberchk(method(options), Request),
    !,
    cors_enable(Request,
                [ methods([get])
                ]),
    format('~n').
start_chat(Request) :-
    cors_enable,
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
    ground(H),
    !,
    extend_options(T0, Options, T).
extend_options([_|T0], Options, T) :-
    extend_options(T0, Options, T).


%!  check_flooding(+Session)
%
%   See whether the client associated with  a session is flooding us
%   and if so, return a resource error.

check_flooding(Session) :-
    get_time(Now),
    (   http_session_retract(websocket(Score, Last))
    ->  Passed is Now-Last,
        NewScore is Score*(2**(-Passed/60)) + 10
    ;   NewScore = 10,
        Passed = 0
    ),
    debug(chat(flooding), 'Flooding score: ~2f (session ~p)',
          [NewScore, Session]),
    http_session_assert(websocket(NewScore, Now)),
    (   NewScore > 50
    ->  throw(http_reply(resource_error(
                             error(permission_error(reconnect, websocket,
                                                    Session),
                                   websocket(reconnect(Passed, NewScore))))))
    ;   true
    ).

%!  accept_chat(+Session, +Options, +WebSocket)
%
%   Create the websocket for the chat session. If the websocket was lost
%   due to a network failure, the client   will  try to reconnect with a
%   reconnect token. If this is successful   we restore the old identity
%   and WSID. If not, we add the  websocket   to  the "hub" and send a a
%   welcome message over the established websocket.

accept_chat(Session, Options, WebSocket) :-
    must_succeed(accept_chat_(Session, Options, WebSocket)),
    Long is 100*24*3600,                        % see update_session_timeout/1
    http_set_session(Session, timeout(Long)).

accept_chat_(Session, Options, WebSocket) :-
    create_chat_room,
    (   option(reconnect(Token), Options),
        http_session_data(wsid(WSID, Token), Session),
        wsid_status_del_lost(WSID),
        existing_visitor(WSID, Session, TmpUser, UserData),
        must_succeed(hub_add(swish_chat, WebSocket, WSID))
    ->  Reason = rejoined
    ;   must_succeed(hub_add(swish_chat, WebSocket, WSID)),
        random_key(16, Token),
        http_session_asserta(wsid(WSID, Token), Session),
        must_succeed(create_visitor(WSID, Session, TmpUser, UserData, Options)),
        Reason = joined
    ),
    gc_visitors,
    visitor_count(Visitors),
    option(check_login(CheckLogin), Options, true),
    Msg0 = _{ type:welcome,
	      uid:TmpUser,
	      wsid:WSID,
	      reconnect:Token,
	      visitors:Visitors,
	      check_login:CheckLogin
	    },
    add_redis_consumer(Msg0, Msg),
    AckMsg = UserData.put(Msg),
    (   hub_send(WSID, json(AckMsg))
    ->  must_succeed(chat_broadcast(UserData.put(_{type:Reason,
                                                   visitors:Visitors,
                                                   wsid:WSID}))),
        debug(chat(websocket), '~w (session ~p, wsid ~p)',
              [Reason, Session, WSID])
    ;   Reason = joined
    ->  debug(chat(websocket), 'Failed to acknowledge join for ~p in ~p',
              [WSID, Session]),
        http_session_retractall(wsid(WSID, Token), Session),
        reclaim_visitor(WSID),
        fail
    ;   debug(chat(websocket), 'Failed to acknowledge rejoin for ~p in ~p',
              [WSID, Session]),
        fail
    ).

add_redis_consumer(Msg0, Msg) :-
    use_redis,
    redis_consumer(Consumer),
    !,
    Msg = Msg0.put(consumer, Consumer).
add_redis_consumer(Msg, Msg).

must_succeed(Goal) :-
    catch_with_backtrace(Goal, E, (print_message(warning, E), fail)),
    !.
must_succeed(Goal) :-
    print_message(warning, goal_failed(Goal)),
    fail.


                 /*******************************
                 *              DATA            *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We have three user identifications:

  - The WSID (Web Socket ID).  This uniquely identifies an open SWISH
    window.
  - The HTTP session id.   A single browser may have multiple SWISH
    windows open and thus be associated with multiple WSIDs.
  - A Visitor ID.  This captures elementary knowledge of the user
    associated with the session.  Each session has exactly on Visitor
    id.

Redis DB organization

  - swish:chat:wsid
    Redis set of known WSID
  - swish:chat:session:WSID -> at(Consumer,Session,Token)
    Expresses that WSID belongs to the SWISH server identified by
    Consumer, the given HTTP session and if if it is lost it can
    be reastablished using Token.
  - swish:chat:lost:WSID -> Time
    We lost connection to WSID at Time (e.g., websocket disconnect)
  - swish:chat:unload:WSID -> boolean
    If `true`, the page was gracefully unloaded.
  - swish:chat:visitor:Visitor -> UserData
    Visitor is a UUID reflecting a visitor with properties for
    identification.

In addition, we store data on the session:

  - websocket(Score, Time)
    Keeps a score based on attempts to establish the websocket.  Used
    to deny a connection request with a 503 error
  - swish_user(Visitor)
    Connect the session to the given visitor UUID.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%!  wsid_status(?WSID, ?Status).
%!  wsid_session(?WSID, ?SessionId).
%!  session_user(?Session, ?TmpUser).
%!  visitor_data(?TmpUser, ?UserData:dict).
%!  subscription(?WSID, ?Channel, ?SubChannel).
%
%   These predicates represent our notion of visitors. Active modes:
%
%     - wsid_status(+WSID, -Status)
%       - Indexes: 1
%       - retract(wsid_status(+WSID, -Status))
%       - assertz(wsid_status(+WSID, +Status))
%       - retractall(wsid_status(+WSID, _))
%     - wsid_session(?WSID, ?SessionId)
%       - Indexes: 1,2,3
%     - session_user(?Session, ?TmpUser)
%       - Indexes: 1
%     - visitor_data(?TmpUser, ?UserData)
%       - Indexes: 1
%     - subscription(?WSID, ?Channel, ?SubChannel)
%       - Indexes: 1,3
%       Currently all subscriptions are on the Channel `gitty`
%
%   @arg WSID is the identifier of the web socket. As we may have to
%   reconnect lost connections, this is may be replaced.
%   @arg Session is the session identifier.  This is used to connect
%   SWISH actions to WSIDs.
%   @arg TmpUser is the ID with which we identify the user for this
%   run. The value is a UUID and thus doesn't reveal the real
%   identity of the user.
%   @arg UserData is a dict that holds information about the real
%   user identity.  This can be empty if no information is known
%   about this user.
%   @arg Status is one of `unload` or `lost(Time)`
%   @arg Channel is an atom denoting a chat channel
%   @arg SubChannel is a related sub channel.

:- dynamic
    wsid_status_db/2,            % WSID, Status
    wsid_session_db/2,           % WSID, Session
    session_user_db/2,		    % Session, TmpUser
    visitor_data_db/2,		    % TmpUser, Data
    subscription_db/3.		    % WSID, Channel, SubChannel


%!  redis_key(+Which, -Server, -Key) is semidet.
%!  redis_key_ro(+Which, -Server, -Key) is semidet.
%
%   Find the Redis server  and  key   for  a  query.  The redis_key_ro/3
%   variant returns the nearby Redis replica if it exists. This can only
%   be used with read-only keys.

redis_key(Which, Server, Key) :-
    swish_config(redis, Server),
    swish_config(redis_prefix, Prefix),
    Which =.. List,
    atomic_list_concat([Prefix, chat | List], :, Key).

redis_key_ro(Which, Server, Key) :-
    swish_config(redis_ro, Server),
    !,
    swish_config(redis_prefix, Prefix),
    Which =.. List,
    atomic_list_concat([Prefix, chat | List], :, Key).
redis_key_ro(Which, Server, Key) :-
    redis_key(Which, Server, Key).

use_redis :-
    swish_config(redis, _).


%!  wsid_status(+WSID, -Status)
%
%   Status is one of lost(Time) if we   lost contact at Time or `unload`
%   if the websocket was cleanly disconnected.
%
%   The Redis version keeps two keys per   WSID as described below. Note
%   that these keys only  exist  on   temporary  lost  or  disconnecting
%   websockets.
%
%     - unload:WSID = boolean
%     - lost:WSID = time

wsid_status(WSID, Status) :-
    redis_key_ro(unload(WSID), Server, UnloadKey),
    !,
    redis_key_ro(lost(WSID), Server, LostKey),
    redis(Server,
          [ get(UnloadKey) -> Unload,
            get(LostKey) -> Lost
          ]),
    (   number(Lost),
        Status = lost(Lost)
    ;   Unload \== nil
    ->  Status = unload
    ).
wsid_status(WSID, Status) :-
    wsid_status_db(WSID, Status).

wsid_status_del(WSID) :-
    redis_key(unload(WSID), Server, UnloadKey),
    !,
    redis_key(lost(WSID), Server, LostKey),
    redis(Server,
          [ del(UnloadKey),
            del(LostKey)
          ]).
wsid_status_del(WSID) :-
    retractall(wsid_status_db(WSID, _Status)).

wsid_status_del_lost(WSID) :-
    redis_key(lost(WSID), Server, Key),
    !,
    redis(Server, del(Key)).
wsid_status_del_lost(WSID) :-
    retractall(wsid_status_db(WSID, lost(_))).

wsid_status_set_lost(WSID, Time) :-
    redis_key(lost(WSID), Server, Key),
    !,
    redis(Server, set(Key, Time)).
wsid_status_set_lost(WSID, Time) :-
    assertz(wsid_status_db(WSID, lost(Time))).

wsid_status_set_unload(WSID) :-
    redis_key(unload(WSID), Server, Key),
    !,
    redis(Server, set(Key, true)).
wsid_status_set_unload(WSID) :-
    assertz(wsid_status_db(WSID, unload)).

%!  wsid_status_del_unload(+WSID) is semidet.
%
%   True when WSID has status `unload` and this status is removed.

wsid_status_del_unload(WSID) :-
    redis_key_ro(unload(WSID), ROServer, Key),
    !,
    (   redis(ROServer, get(Key), true)
    ->  redis_key(unload(WSID), RWServer, Key),
        redis(RWServer, del(Key))
    ).
wsid_status_del_unload(WSID) :-
    retract(wsid_status_db(WSID, unload)),
    !.

%!  register_wsid_session(+WSID, +Session) is det.
%
%   Register WSID to belong  to  Session,   i.e.,  the  given  websocket
%   belongs to a Session on some browser. When using a Redis cluster, we
%   also want to know the Redis  _consumer_   on  which  this session is
%   active, such that we can relay messages to the proper SWISH server.
%
%   Redis data:
%
%     - wsid: set of WSID
%     - session:WSID to at(Consumer,Session)

register_wsid_session(WSID, Session) :-
    redis_key(wsid, Server, SetKey),
    redis_key(session(WSID), Server, SessionKey),
    !,
    redis_consumer(Consumer),
    redis(Server, sadd(SetKey, WSID)),
    redis(Server, set(SessionKey, at(Consumer,Session) as prolog)).
register_wsid_session(WSID, Session) :-
    assertz(wsid_session_db(WSID, Session)).

%!  wsid_session(?WSID, ?Session) is nondet.
%!  wsid_session(?WSID, ?Session, -Consumer) is nondet.
%
%   True when there is a known visitor  WSID that is associated with the
%   HTTP Session, uses  Token  for  reconnecting   and  runs  on  a node
%   identified by the Redis Consumer.

wsid_session(WSID, Session) :-
    wsid_session(WSID, Session, _Consumer).

wsid_session(WSID, Session, Consumer) :-
    use_redis,
    !,
    (   nonvar(Session)
    ->  http_session_data(wsid(WSID,_Token), Session)
    ;   current_wsid(WSID),
        redis_key_ro(session(WSID), Server, SessionKey),
        redis(Server, get(SessionKey), at(Consumer,Session))
    ).
wsid_session(WSID, Session, single) :-
    wsid_session_db(WSID, Session).

%!  wsid_session_reclaim(+WSID, -Session) is semidet.
%
%   True when WSID was connected to Session and now no longer is.

wsid_session_reclaim(WSID, Session) :-
    redis_key_ro(session(WSID), ROServer, SessionKey),
    redis_key(wsid, WRServer, SetKey),
    !,
    redis(ROServer, get(SessionKey), At),
    arg(2, At, Session),            % changed from at/3 to at/2.
    redis(WRServer, srem(SetKey, WSID)),
    redis(WRServer, del(SessionKey)).
wsid_session_reclaim(WSID, Session) :-
    retract(wsid_session_db(WSID, Session)).

%!  wsid_session_reclaim_all(+WSID, +Session) is det.

wsid_session_reclaim_all(WSID, _Session) :-
    redis_key(wsid, Server, SetKey),
    !,
    redis(Server, srem(SetKey, WSID)),
    redis_key(session(WSID), Server, SessionKey),
    redis(Server, del(SessionKey)).
wsid_session_reclaim_all(WSID, Session) :-
    retractall(wsid_session_db(WSID, Session)).

wsid_session_del_session(Session) :-
    use_redis,
    !,
    (   wsid_session(WSID, Session),
        wsid_session_reclaim(WSID, Session),
        fail
    ;   true
    ).
wsid_session_del_session(Session) :-
    retractall(wsid_session_db(_, Session)).

%!  current_wsid(?WSID) is nondet.
%
%   True when WSID is a (Redis) known WSID.

current_wsid(WSID) :-
    nonvar(WSID),
    !,
    redis_key_ro(wsid, Server, SetKey),
    redis(Server, sismember(SetKey, WSID), 1).
current_wsid(WSID) :-
    redis_key_ro(wsid, Server, SetKey),
    redis_sscan(Server, SetKey, List, []),
    member(WSID, List).

%!  session_user(?Session, ?TmpUser:atom).
%
%   Relate Session to a  tmp  user  id.   Info  about  the  tmp  user is
%   maintained in visitor_data/2.

session_user(Session, TmpUser) :-
    http_current_session(Session, swish_user(TmpUser)).

session_user_create(Session, User) :-
    http_session_asserta(swish_user(User), Session).

session_user_del(Session, User) :-
    http_session_retract(swish_user(User), Session).

%!  visitor_data(?Visitor, ?Data)

visitor_data(Visitor, Data) :-
    redis_key(visitor(Visitor), Server, Key),
    !,
    redis_get_hash(Server, Key, Data).
visitor_data(Visitor, Data) :-
    visitor_data_db(Visitor, Data).

visitor_data_set(Visitor, Data) :-
    redis_key(visitor(Visitor), Server, Key),
    !,
    redis_set_hash(Server, Key, Data).
visitor_data_set(Visitor, Data) :-
    retractall(visitor_data_db(Visitor, _)),
    assertz(visitor_data_db(Visitor, Data)).

visitor_data_del(Visitor, Data) :-
    redis_key(visitor(Visitor), Server, Key),
    !,
    redis_get_hash(Server, Key, Data),
    redis(Server, del(Key)).
visitor_data_del(Visitor, Data) :-
    retract(visitor_data_db(Visitor, Data)).

%!  subscription(?WSID, ?Channel, ?SubChannel)
%
%   Requires both WSID -> Channel/SubChannel and backward relation.
%   Redis:
%
%     channel:SubChannel --> set(WSID-Channel)
%     subscription:WSID  --> set(Channel-SubChannel)

subscription(WSID, Channel, SubChannel) :-
    use_redis,
    !,
    (   nonvar(WSID), nonvar(Channel), nonvar(SubChannel)
    ->  redis_key_ro(subscription(WSID), Server, WsKey),
        redis(Server, sismember(WsKey, Channel-SubChannel as prolog), 1)
    ;   nonvar(SubChannel)
    ->  redis_key_ro(channel(SubChannel), Server, ChKey),
        redis_sscan(Server, ChKey, List, []),
        member(WSID-Channel, List)
    ;   (   nonvar(WSID)
        ->  true
        ;   current_wsid(WSID)
        ),
        redis_key_ro(subscription(WSID), Server, WsKey),
        redis_sscan(Server, WsKey, List, []),
        member(Channel-SubChannel, List)
    ).
subscription(WSID, Channel, SubChannel) :-
    subscription_db(WSID, Channel, SubChannel).

%!  subscribe(+WSID, +Channel) is det.
%!  subscribe(+WSID, +Channel, +SubChannel) is det.
%
%   Subscript WSID to listen to messages on Channel/SubChannel.

subscribe(WSID, Channel) :-
    subscribe(WSID, Channel, _SubChannel).

subscribe(WSID, Channel, SubChannel) :-
    use_redis,
    !,
    redis_key(channel(SubChannel), Server, ChKey),
    redis_key(subscription(WSID), Server, WsKey),
    redis(Server, sadd(ChKey, WSID-Channel as prolog)),
    redis(Server, sadd(WsKey, Channel-SubChannel as prolog)).
subscribe(WSID, Channel, SubChannel) :-
    (   subscription(WSID, Channel, SubChannel)
    ->  true
    ;   assertz(subscription_db(WSID, Channel, SubChannel))
    ).

%!  unsubscribe(?WSID, ?Channel) is det.
%!  unsubscribe(?WSID, ?Channel, ?SubChannel) is det.
%
%   Remove all matching subscriptions.

unsubscribe(WSID, Channel) :-
    unsubscribe(WSID, Channel, _SubChannel).

unsubscribe(WSID, Channel, SubChannel) :-
    use_redis,
    !,
    (   (   nonvar(WSID), nonvar(Channel), nonvar(SubChannel)
        ->  true
        ;   subscription(WSID, Channel, SubChannel)
        ),
        redis_unsubscribe(WSID, Channel, SubChannel),
        fail
    ;   true
    ).
unsubscribe(WSID, Channel, SubChannel) :-
    retractall(subscription_db(WSID, Channel, SubChannel)).

redis_unsubscribe(WSID, Channel, SubChannel) :-
    redis_key(channel(SubChannel), Server, ChKey),
    redis_key(subscription(WSID), Server, WsKey),
    redis(Server, srem(ChKey, WSID-Channel as prolog)),
    redis(Server, srem(WsKey, Channel-SubChannel as prolog)).


		 /*******************************
		 *        HIGH LEVEL DB		*
		 *******************************/

%!  visitor(?WSID) is nondet.
%!  visitor(?WSID, -Consumer) is nondet.
%
%   True when WSID should  be  considered   an  active  visitor  that is
%   connected to the SWISH node identified   by the Redis Consumer. This
%   means
%
%      - If we lost the visitor for less than 30 sec, we consider it
%        still active.
%      - If it is connected to our websocket hub, it is active.
%      - Otherwise, if it runs on our node, we destroy the visitor.

visitor(WSID) :-
    visitor(WSID, _).

visitor(WSID, Consumer) :-
    wsid_session(WSID, _Session, Consumer),
    \+ pending_visitor(WSID, 30).

visitor_count(Count) :-
    use_redis,
    !,
    active_wsid_count(Count).
visitor_count(Count) :-
    aggregate_all(count, visitor(_), Count).

%!  pending_visitor(+WSID, +Timeout) is semidet.
%
%   True if WSID is inactive. This means   we lost the connection at
%   least Timeout seconds ago.

pending_visitor(WSID, Timeout) :-
    wsid_status(WSID, lost(Lost)),
    get_time(Now),
    Now - Lost > Timeout.

%!  wsid_visitor(?WSID, ?Visitor)
%
%   True when WSID is associated with Visitor

wsid_visitor(WSID, Visitor) :-
    nonvar(WSID),
    !,
    wsid_session(WSID, Session),
    session_user(Session, Visitor).
wsid_visitor(WSID, Visitor) :-
    session_user(Session, Visitor),
    wsid_session(WSID, Session).

%!  existing_visitor(+WSID, +Session, -TmpUser, -UserData) is semidet.
%
%   True if we are dealing with  an   existing  visitor for which we
%   lost the connection.

existing_visitor(WSID, Session, TmpUser, UserData) :-
    wsid_session(WSID, Session),
    session_user(Session, TmpUser),
    visitor_data(TmpUser, UserData),
    !.
existing_visitor(WSID, Session, _, _) :-
    wsid_session_reclaim_all(WSID, Session),
    fail.

%!  create_visitor(+WSID, +Session, -TmpUser, -UserData, +Options)
%
%   Create a new visitor  when  a   new  websocket  is  established.
%   Options provides information we have about the user:
%
%     - current_user_info(+Info)
%       Already logged in user with given information
%     - avatar(Avatar)
%       Avatar remembered in the browser for this user.
%     - anonymous_name(NickName)
%       Nick name remembered in the browser for this user.
%     - anonymous_avatar(URL)
%       Avatar remembered in the browser for this user.  Using the SVG
%       avatars, this is `/icons/avatar.svg#NNN`, which `NNN` is a
%       bitmask on the SVG to change its appearance,

create_visitor(WSID, Session, TmpUser, UserData, Options) :-
    register_wsid_session(WSID, Session),
    create_session_user(Session, TmpUser, UserData, Options).

%!  random_key(+Len, -Key) is det.
%
%   Generate a random confirmation key

random_key(Len, Key) :-
    length(Codes, Len),
    maplist(random_between(0,255), Codes),
    phrase(base64url(Codes), Encoded),
    atom_codes(Key, Encoded).

%!  destroy_visitor(+WSID) is det.
%
%   The web socket WSID has been   closed. We should not immediately
%   destroy the temporary user as the browser may soon reconnect due
%   to a page reload  or  re-establishing   the  web  socket after a
%   temporary network failure. We leave   the destruction thereof to
%   the session, but set the session timeout to a fairly short time.
%
%   @tbd    We should only inform clients that we have informed
%           about this user.

destroy_visitor(WSID) :-
    must_be(atom, WSID),
    update_session_timeout(WSID),
    destroy_reason(WSID, Reason),
    (   Reason == unload
    ->  reclaim_visitor(WSID)
    ;   get_time(Now),
        wsid_status_set_lost(WSID, Now)
    ),
    visitor_count(Count),
    debug(chat(visitor), '~p left. Broadcasting ~d visitors', [WSID,Count]),
    chat_broadcast(_{ type:removeUser,
                      wsid:WSID,
                      reason:Reason,
                      visitors:Count
                    }).

destroy_reason(WSID, Reason) :-
    wsid_status_del_unload(WSID),
    !,
    Reason = unload.
destroy_reason(_, close).

%!  update_session_timeout(+WSID) is det.
%
%   WSID was lost. If this is  the   only  websocket  on this session we
%   reduce the session timeout  to  15   minutes.  This  cooperates with
%   accept_chat/3, which sets the session  timeout   to  100 days for as
%   long as we have an active websocket connection.

update_session_timeout(WSID) :-
    wsid_session(WSID, Session),
    !,
    (   wsid_session(WSID2, Session),
        WSID2 \== WSID
    ->  true
    ;   debug(chat(websocket), 'Websocket ~p was last in session ~p',
              [ WSID, Session]),
        http_set_session(Session, timeout(900))
    ).
update_session_timeout(_).

%!  gc_visitors
%
%   Reclaim all visitors with whom we have   lost the connection and the
%   browser did not reclaim the   session  within `session_lost_timeout`
%   seconds.
%
%   This also updates active_wsid/2 to reflect the current status.

:- dynamic gc_status/1.

gc_visitors :-
    swish_config(session_lost_timeout, TMO),
    gc_status(Status),
    (   Status == running
    ->  true
    ;   Status = completed(When),
        get_time(Now),
        Now-When > TMO
    ->  fail
    ;   retractall(gc_status(completed(_)))
    ),
    !.
gc_visitors :-
    swish_config(session_lost_timeout, TMO),
    catch(thread_create(gc_visitors_sync(TMO), _Id,
                        [ alias('swish_chat_gc_visitors'),
                          detached(true)
                        ]),
          error(permission_error(create, thread, _), _),
          true).

gc_visitors_sync(TMO) :-
    setup_call_cleanup(
        asserta(gc_status(running), Ref),
        ( do_gc_visitors(TMO),
          get_time(Now),
          asserta(gc_status(completed(Now)))
        ),
        erase(Ref)).

do_gc_visitors(TMO) :-
    findall(WSID-Consumer,
            active_visitor(TMO, WSID, Consumer),
            Pairs),
    transaction(
        ( retractall(active_wsid(_,_)),
          forall(member(WSID-Consumer, Pairs),
                 assertz(active_wsid(WSID, Consumer))))).

active_visitor(TMO, WSID, Consumer) :-
    wsid_session(WSID, _Session, Consumer),
    (   valid_visitor(WSID, TMO, Consumer)
    ->  true
    ;   reclaim_visitor(WSID),
        fail
    ).

valid_visitor(WSID, _TMO, _Consumer) :-
    hub_member(swish_chat, WSID),
    !.
valid_visitor(WSID, TMO, _Consumer) :-
    wsid_status(WSID, lost(Lost)),
    !,
    get_time(Now),
    Now - Lost < TMO.
valid_visitor(_WSID, _TMO, Consumer) :-
    use_redis,
    !,
    \+ redis_consumer(Consumer).

%!  reclaim_visitor(+WSID) is det.
%
%   Reclaim a WSID connection. If  the   user  left  gracefully, this is
%   called immediately. If we lost the connection   on an error, this is
%   eventually called (indirectly) by do_gc_visitors/1.

reclaim_visitor(WSID) :-
    debug(chat(gc), 'Reclaiming idle ~p', [WSID]),
    reclaim_wsid_session(WSID),
    wsid_status_del(WSID),
    unsubscribe(WSID, _).

reclaim_wsid_session(WSID) :-
    (   wsid_session_reclaim(WSID, Session)
    ->  http_session_retractall(websocket(_, _), Session)
    ;   true
    ).

%!  create_session_user(+Session, -User, -UserData, +Options)
%
%   Associate a user with the session. The user id is a UUID that is
%   not associated with  any  persistent  notion   of  a  user.  The
%   destruction is left to the destruction of the session.

:- listen(http_session(end(SessionID, _Peer)),
          destroy_session_user(SessionID)).

create_session_user(Session, TmpUser, UserData, _Options) :-
    session_user(Session, TmpUser),
    visitor_data(TmpUser, UserData),
    !.
create_session_user(Session, TmpUser, UserData, Options) :-
    uuid(TmpUser),
    get_visitor_data(UserData, Options),
    session_user_create(Session, TmpUser),
    visitor_data_set(TmpUser, UserData).

destroy_session_user(Session) :-
    forall(wsid_session(WSID, Session, _Token),
           inform_session_closed(WSID, Session)),
    wsid_session_del_session(Session),
    forall(session_user_del(Session, TmpUser),
           destroy_visitor_data(TmpUser)).

destroy_visitor_data(TmpUser) :-
    (   visitor_data_del(TmpUser, Data),
        release_avatar(Data.get(avatar)),
        fail
    ;   true
    ).

inform_session_closed(WSID, Session) :-
    ignore(hub_send(WSID, json(_{type:session_closed}))),
    session_user(Session, TmpUser),
    update_visitor_data(TmpUser, _Data, logout).


%!  update_visitor_data(+TmpUser, +Data, +Reason) is det.
%
%   Update the user data for the visitor   TmpUser  to Data. This is
%   rather complicated due to all the   defaulting  rules. Reason is
%   one of:
%
%     - login
%     - logout
%     - 'set-nick-name'
%     - 'profile-edit'
%
%   @tbd Create a more declarative description  on where the various
%   attributes must come from.

update_visitor_data(TmpUser, _Data, logout) :-
    !,
    anonymise_user_data(TmpUser, NewData),
    set_visitor_data(TmpUser, NewData, logout).
update_visitor_data(TmpUser, Data, Reason) :-
    profile_reason(Reason),
    !,
    (   visitor_data(TmpUser, Old)
    ;   Old = v{}
    ),
    copy_profile([name,avatar,email], Data, Old, New),
    set_visitor_data(TmpUser, New, Reason).
update_visitor_data(TmpUser, _{name:Name}, 'set-nick-name') :-
    !,
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

copy_profile_field(avatar, New, Data0, Data) :-
    !,
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
copy_profile_field(email, New, Data0, Data) :-
    !,
    (   NewMail = New.get(email)
    ->  update_avatar_from_email(NewMail, Data0, Data1),
        Data = Data1.put(email, NewMail)
    ;   update_avatar_from_email('', Data0, Data1),
        (   del_dict(email, Data1, _, Data)
        ->  true
        ;   Data = Data1
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

%!  update_avatar_from_email(+Email, +DataIn, -Data)
%
%   Update the avatar after a change  of   the  known  email. If the
%   avatar comes from the profile, no action is needed. If Email has
%   a gravatar, use that. Else  use  the   know  or  a new generated
%   avatar.

update_avatar_from_email(_, Data, Data) :-
    Data.get(avatar_source) == profile,
    !.
update_avatar_from_email('', Data0, Data) :-
    Data0.get(avatar_source) == email,
    !,
    noble_avatar_url(Avatar, []),
    Data = Data0.put(_{avatar:Avatar, anonymous_avatar:Avatar,
                       avatar_source:generated}).
update_avatar_from_email(Email, Data0, Data) :-
    email_gravatar(Email, Avatar),
    valid_gravatar(Avatar),
    !,
    Data = Data0.put(avatar, Avatar).
update_avatar_from_email(_, Data0, Data) :-
    (   Avatar = Data0.get(anonymous_avatar)
    ->  Data = Data0.put(_{avatar:Avatar, avatar_source:client})
    ;   noble_avatar_url(Avatar, []),
        Data = Data0.put(_{avatar:Avatar, anonymous_avatar:Avatar,
                           avatar_source:generated})
    ).

%!  anonymise_user_data(TmpUser, Data)
%
%   Create anonymous user profile.

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
    ),
    !.
anonymise_user_data(_, Data) :-
    noble_avatar_url(Avatar, []),
    Data = _{anonymous_avatar:Avatar,
             avatar:Avatar, avatar_source:generated}.

%!  set_visitor_data(+TmpUser, +Data, +Reason) is det.
%
%   Update the user data for the   session  user TmpUser and forward
%   the changes.

set_visitor_data(TmpUser, Data, Reason) :-
    visitor_data_set(TmpUser, Data),
    inform_visitor_change(TmpUser, Reason).

%!  inform_visitor_change(+TmpUser, +Reason) is det.
%
%   Inform browsers showing  TmpUser  that   the  visitor  data  has
%   changed. The first  clause  deals   with  forwarding  from  HTTP
%   requests,  where  we  have  the  session  and  the  second  from
%   websocket requests where we have the WSID.

inform_visitor_change(TmpUser, Reason) :-
    http_in_session(Session),
    !,
    public_user_data(TmpUser, Data),
    forall(wsid_session(WSID, Session),
           inform_friend_change(WSID, Data, Reason)).
inform_visitor_change(TmpUser, Reason) :-
    nb_current(wsid, WSID),
    !,
    public_user_data(TmpUser, Data),
    inform_friend_change(WSID, Data, Reason).
inform_visitor_change(_, _).

inform_friend_change(WSID, Data, Reason) :-
    Message = json(_{ type:"profile",
                      wsid:WSID,
                      reason:Reason
                    }.put(Data)),
    send_friends(WSID, Message).

%!  sync_gazers(+WSID, +Files:list(atom)) is det.
%
%   A browser signals it has Files open.   This happens when a SWISH
%   instance is created as well  as   when  a SWISH instance changes
%   state, such as closing a tab, adding   a  tab, bringing a tab to
%   the foreground, etc.

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
    hub_member(swish_chat, WSID),
    !,
    findall(Gazer, files_gazer(Files, Gazer), Gazers),
    ignore(hub_send(WSID, json(_{type:"gazers", gazers:Gazers}))).
inform_me_about_existing_gazers(_, _).

files_gazer(Files, Gazer) :-
    member(File, Files),
    subscription(WSID, gitty, File),
    wsid_session(WSID, Session),
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

%!  add_user_details(+Message, -Enriched) is det.
%
%   Add additional information to a message.  Message must
%   contain a `uid` field.

add_user_details(Message, Enriched) :-
    public_user_data(Message.uid, Data),
    Enriched = Message.put(Data).

%!  public_user_data(+UID, -Public:dict) is det.
%
%   True when Public provides the   information  we publically share
%   about UID. This is currently the name and avatar.

public_user_data(UID, Public) :-
    visitor_data(UID, Data),
    (   _{name:Name, avatar:Avatar} :< Data
    ->  Public = _{name:Name, avatar:Avatar}
    ;   _{avatar:Avatar} :< Data
    ->  Public = _{avatar:Avatar}
    ;   Public = _{}
    ).

%!  get_visitor_data(-Data:dict, +Options) is det.
%
%   Optain data for a new visitor.  Options include:
%
%     - identity(+Identity)
%     Identity information provided by authenticate/2.  Always
%     present.
%     - avatar(+URL)
%     Avatar provided by the user
%     - nick_name(+Name)
%     Nick name provided by the user.
%
%   Data always contains an `avatar` key   and optionally contains a
%   `name` and `email` key. If the avatar is generated there is also
%   a key `avatar_generated` with the value `true`.
%
%   @bug    This may check for avatar validity, which may take
%           long.  Possibly we should do this in a thread.

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
                 *         NOBLE AVATAR         *
                 *******************************/

:- http_handler(swish('avatar/'), reply_avatar, [id(avatar), prefix]).

%!  reply_avatar(+Request)
%
%   HTTP handler for Noble  Avatar   images.  Using  create_avatar/2
%   re-creates avatars from the file name,  so we can safely discard
%   the avatar file store.
%
%   Not really. A new user gets a new   avatar  and this is based on
%   whether or not the file exists. Probably we should maintain a db
%   of handed out avatars and their last-use   time stamp. How to do
%   that? Current swish stats: 400K avatars, 3.2Gb data.

reply_avatar(Request) :-
    cors_enable,
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
    option(avatar(HREF), Options),
    !.
noble_avatar_url(HREF, _Options) :-
    swish_config:config(avatars, noble),
    !,
    noble_avatar(_Gender, Path, true),
    file_base_name(Path, File),
    http_absolute_location(swish(avatar/File), HREF, []).
noble_avatar_url(HREF, _Options) :-
    A is random(0x1FFFFF+1),
    http_absolute_location(icons('avatar.svg'), HREF0, []),
    format(atom(HREF), '~w#~d', [HREF0, A]).



                 /*******************************
                 *         BROADCASTING         *
                 *******************************/

%!  chat_broadcast(+Message) is det.
%!  chat_broadcast(+Message, +Channel) is det.
%
%   Send Message to all known SWISH  clients.   Message  is a valid JSON
%   object, i.e., a dict or option list.   When  using Redis we send the
%   message to the  ``swish:chat``  pubsub   channel  and  listening for
%   ``swish:chat`` calls chat_broadcast_local/1,2 in each instance.
%
%   @arg Channel is either an atom or a term Channel/SubChannel,
%   where both Channel and SubChannel are atoms.

chat_broadcast(Message) :-
    use_redis,
    !,
    redis(swish, publish(swish:chat, chat(Message) as prolog)).
chat_broadcast(Message) :-
    chat_broadcast_local(Message).

chat_broadcast(Message, Channel) :-
    use_redis,
    !,
    redis(swish, publish(swish:chat, chat(Message, Channel) as prolog)).
chat_broadcast(Message, Channel) :-
    chat_broadcast_local(Message, Channel).


chat_broadcast_local(Message) :-
    debug(chat(broadcast), 'Broadcast: ~p', [Message]),
    hub_broadcast(swish_chat, json(Message)).

chat_broadcast_local(Message, Channel/SubChannel) :-
    !,
    must_be(atom, Channel),
    must_be(atom, SubChannel),
    debug(chat(broadcast), 'Broadcast on ~p: ~p',
          [Channel/SubChannel, Message]),
    hub_broadcast(swish_chat, json(Message),
                  subscribed(Channel, SubChannel)).
chat_broadcast_local(Message, Channel) :-
    must_be(atom, Channel),
    debug(chat(broadcast), 'Broadcast on ~p: ~p', [Channel, Message]),
    hub_broadcast(swish_chat, json(Message),
                  subscribed(Channel)).

%!  subscribed(+Channel, +WSID) is semidet.
%!  subscribed(+Channel, +SubChannel, +WSID) is semidet.
%
%   Filter used by hub_broadcast/3. WSID is   always a locally known web
%   and active socket.

subscribed(Channel, WSID) :-
    subscription(WSID, Channel, _).
subscribed(Channel, SubChannel, WSID) :-
    subscription(WSID, Channel, SubChannel).
subscribed(gitty, SubChannel, WSID) :-
    swish_config:config(hangout, SubChannel),
    \+ subscription(WSID, gitty, SubChannel).

%!  send_friends(+WSID, +Message)
%
%   Send Message to WSID and all its friends.

send_friends(WSID, Message) :-
    use_redis,
    !,
    redis(swish, publish(swish:chat, send_friends(WSID, Message) as prolog)).
send_friends(WSID, Message) :-
    send_friends_local(WSID, Message).

send_friends_local(WSID, Message) :-
    hub_send_if_on_me(WSID, Message),
    forall(distinct(viewing_same_file(WSID, Friend)),
           ignore(hub_send_if_on_me(Friend, Message))).

hub_send_if_on_me(WSID, Message) :-
    hub_member(swish_chat, WSID),
    !,
    hub_send(WSID, Message).
hub_send_if_on_me(_, _).

viewing_same_file(WSID, Friend) :-
    subscription(WSID, gitty, File),
    subscription(Friend, gitty, File),
    Friend \== WSID.


		 /*******************************
		 *      REDIS CONNNECTION       *
		 *******************************/

:- initialization
    listen(redis(_, 'swish:chat', Message),
           chat_message(Message)).

chat_message(chat(Message)) :-
    update_visitors(Message),
    chat_broadcast_local(Message).
chat_message(chat(Message, Channel)) :-
    chat_broadcast_local(Message, Channel).
chat_message(send_friends(WSID, Message)) :-
    send_friends_local(WSID, Message).

%!  update_visitors(+Msg) is det.
%
%   Maintain notion of active users  based on broadcasted (re)join and
%   left messages.  We sync every 5 minutes to compensate for possible
%   missed users.

:- dynamic
       (   last_wsid_sync/1,
	   active_wsid/2
       ) as volatile.

update_visitors(Msg),
  _{type:removeUser, wsid:WSID} :< Msg =>
    retractall(active_wsid(WSID, _)).
update_visitors(Msg),
  _{type:joined, wsid:WSID} :< Msg,
  \+ active_wsid(WSID, _) =>
    asserta(active_wsid(WSID, Msg.get(consumer, -))).
update_visitors(Msg),
  _{type:rejoined, wsid:WSID} :< Msg,
  \+ active_wsid(WSID, _) =>
    asserta(active_wsid(WSID, Msg.get(consumer, -))).
update_visitors(_) =>
    true.

active_wsid_count(Count) :-
    predicate_property(active_wsid(_,_), number_of_clauses(Count)),
    !.
active_wsid_count(0).

active_wsid_count(Consumer, Count) :-
    aggregate(count, WSID^active_wsid(WSID, Consumer), Count).


                 /*******************************
                 *           CHAT ROOM          *
                 *******************************/

create_chat_room :-
    current_hub(swish_chat, _),
    !.
create_chat_room :-
    with_mutex(swish_chat, create_chat_room_sync).

create_chat_room_sync :-
    current_hub(swish_chat, _),
    !.
create_chat_room_sync :-
    hub_create(swish_chat, Room, _{}),
    thread_create(swish_chat(Room), _, [alias(swish_chat)]).

swish_chat(Room) :-
    (   catch_with_backtrace(swish_chat_event(Room), E, chat_exception(E))
    ->  true
    ;   print_message(warning, goal_failed(swish_chat_event(Room)))
    ),
    swish_chat(Room).

chat_exception('$aborted') :- !.
chat_exception(unwind(_)) :- !.
chat_exception(E) :-
    print_message(warning, E).

swish_chat_event(Room) :-
    thread_get_message(Room.queues.event, Message),
    (   handle_message(Message, Room)
    ->  true
    ;   print_message(warning, goal_failed(handle_message(Message, Room)))
    ).

%!  handle_message(+Message, +Room)
%
%   Handle incoming messages. This handles   messages from our websocket
%   connections, i.e., this does  not  see   messages  on  other (Redis)
%   instances.

handle_message(Message, _Room) :-
    websocket{opcode:text} :< Message,
    !,
    atom_json_dict(Message.data, JSON, []),
    debug(chat(received), 'Received from ~p: ~p', [Message.client, JSON]),
    WSID = Message.client,
    (   current_wsid(WSID)
    ->  setup_call_cleanup(
            b_setval(wsid, WSID),
            json_message(JSON, WSID),
            nb_delete(wsid))
    ;   debug(chat(visitor), 'Ignored ~p (WSID ~p unknown)', [Message, WSID])
    ).
handle_message(Message, _Room) :-
    hub{joined:WSID} :< Message,
    !,
    debug(chat(visitor), 'Joined: ~p', [WSID]).
handle_message(Message, _Room) :-
    hub{left:WSID, reason:write(Lost)} :< Message,
    !,
    (   destroy_visitor(WSID)
    ->  debug(chat(visitor), 'Left ~p due to write error for ~p',
              [WSID, Lost])
    ;   true
    ).
handle_message(Message, _Room) :-
    hub{left:WSID} :< Message,
    !,
    (   destroy_visitor(WSID)
    ->  debug(chat(visitor), 'Left: ~p', [WSID])
    ;   true
    ).
handle_message(Message, _Room) :-
    websocket{opcode:close, client:WSID} :< Message,
    !,
    debug(chat(visitor), 'Left: ~p', [WSID]),
    destroy_visitor(WSID).
handle_message(Message, _Room) :-
    debug(chat(ignored), 'Ignoring chat message ~p', [Message]).


%!  json_message(+Message, +WSID) is det.
%
%   Process a JSON message  translated  to   a  dict.  The following
%   messages are understood:
%
%     - subscribe channel [subchannel]
%     - unsubscribe channel [subchannel]
%     Actively (un)subscribe for specific message channels.
%     - unload
%     A SWISH instance is cleanly being unloaded.
%     - has-open-files files
%     Executed after initiating the websocket to indicate loaded
%     files.
%     - set-nick-name name
%     User set nick name for anonymous identoty

json_message(Dict, WSID) :-
    _{ type: "subscribe",
       channel:ChannelS, sub_channel:SubChannelS} :< Dict,
    !,
    atom_string(Channel, ChannelS),
    atom_string(SubChannel, SubChannelS),
    subscribe(WSID, Channel, SubChannel).
json_message(Dict, WSID) :-
    _{type: "subscribe", channel:ChannelS} :< Dict,
    !,
    atom_string(Channel, ChannelS),
    subscribe(WSID, Channel).
json_message(Dict, WSID) :-
    _{ type: "unsubscribe",
       channel:ChannelS, sub_channel:SubChannelS} :< Dict,
    !,
    atom_string(Channel, ChannelS),
    atom_string(SubChannel, SubChannelS),
    unsubscribe(WSID, Channel, SubChannel).
json_message(Dict, WSID) :-
    _{type: "unsubscribe", channel:ChannelS} :< Dict,
    !,
    atom_string(Channel, ChannelS),
    unsubscribe(WSID, Channel).
json_message(Dict, WSID) :-
    _{type: "unload"} :< Dict,     % clean close/reload
    !,
    sync_gazers(WSID, []),
    wsid_status_set_unload(WSID).
json_message(Dict, WSID) :-
    _{type: "has-open-files", files:FileDicts} :< Dict,
    !,
    maplist(dict_file_name, FileDicts, Files),
    sync_gazers(WSID, Files).
json_message(Dict, WSID) :-
    _{type: "reloaded", file:FileS, commit:Hash} :< Dict,
    !,
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
    _{type: "set-nick-name", name:Name} :< Dict,
    !,
    wsid_visitor(WSID, Visitor),
    update_visitor_data(Visitor, _{name:Name}, 'set-nick-name').
json_message(Dict, WSID) :-
    _{type: "chat-message", docid:DocID} :< Dict,
    !,
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

%!  forbidden(+Message, +DocID, -Why) is semidet.
%
%   True if the chat Message about DocID must be forbidden, in which
%   case Why is  unified  with  a   string  indicating  the  reason.
%   Currently:
%
%     - Demands the user to be logged on
%     - Limits the size of the message and its payloads
%
%   @tbd Call authorized/2 with all proper identity information.

forbidden(Message, DocID, Why) :-
    \+ swish_config:config(chat_spam_protection, false),
    \+ ws_authorized(chat(post(Message, DocID)), Message.user),
    !,
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
    large_payload(Payload, Why),
    !.
forbidden(Message, _DocID, Why) :-
    \+ swish_config:config(chat_spam_protection, false),
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
    ),
    !,
    Why = "Selection too long (max. 500 characters)".
large_payload(Payload, Why) :-
    string_length(Payload.get(query), QLen), QLen > 1000,
    !,
    Why = "Query too long (max. 1000 characters)".

user_score(Message, MsgScore, Cummulative, Count) :-
    Profile = Message.get(user).get(profile_id),
    !,
    block(Profile, MsgScore, Cummulative, Count).
user_score(_, _, 0, 1).

%!  block(+User, +Score, -Cummulative, -Count)
%
%   Keep a count and cummulative score for a user.

:- dynamic
    blocked/4.

block(User, Score, Cummulative, Count) :-
    blocked(User, Score0, Count0, Time),
    !,
    get_time(Now),
    Cummulative = Score0*(0.5**((Now-Time)/600)) + Score,
    Count is Count0 + 1,
    asserta(blocked(User, Cummulative, Count, Now)),
    retractall(blocked(User, Score0, Count0, Time)).
block(User, Score, Score, 1) :-
    get_time(Now),
    asserta(blocked(User, Score, 1, Now)).


                 /*******************************
                 *         CHAT MESSAGES        *
                 *******************************/

%!  chat_add_user_id(+WSID, +Message0, -Message) is det.
%
%   Decorate a message with the user credentials.

chat_add_user_id(WSID, Dict, Message) :-
    wsid_session(WSID, Session),
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


%!  chat_about(+DocID, +Message) is det.
%
%   Distribute a chat message about DocID.

chat_about(DocID, Message) :-
    chat_relay(Message.put(docid, DocID)).

%!  chat_relay(+Message) is det.
%
%   Store and relay a chat message.

chat_relay(Message) :-
    chat_enrich(Message, Message1),
    chat_send(Message1).

%!  chat_enrich(+Message0, -Message) is det.
%
%   Add time and identifier to the chat message.

chat_enrich(Message0, Message) :-
    get_time(Now),
    uuid(ID),
    Message = Message0.put(_{time:Now, id:ID}).

%!  chat_send(+Message)
%
%   Relay the chat message Message. If  the message has a `volatile`
%   property it is broadcasted, but not stored.

chat_send(Message) :-
    atom_concat("gitty:", File, Message.docid),
    broadcast(swish(chat(Message))),
    (   Message.get(volatile) == true
    ->  true
    ;   chat_store(Message)
    ),
    chat_broadcast(Message, gitty/File).


                 /*******************************
                 *            EVENTS            *
                 *******************************/

:- unlisten(swish(_)),
   listen(swish(Event), chat_event(Event)).

%!  chat_event(+Event) is semidet.
%
%   Event happened inside SWISH.  Currently triggered events:
%
%     - updated(+File, +From, +To)
%     File was updated from hash From to hash To.
%     - profile(+ProfileID)
%     Session was associated with user with profile ProfileID
%     - logout(+ProfileID)
%     User logged out. If the login was based on HTTP authentication
%     ProfileID equals `http`.

chat_event(Event) :-
    broadcast_event(Event),
    http_session_id(Session),
    debug(event, 'Event: ~p, session ~q', [Event, Session]),
    event_file(Event, File),
    !,
    (   wsid_session(WSID, Session),
        subscription(WSID, gitty, File)
    ->  true
    ;   wsid_session(WSID, Session)
    ->  true
    ;   WSID = undefined
    ),
    session_broadcast_event(Event, File, Session, WSID).
chat_event(logout(_ProfileID)) :-
    !,
    http_session_id(Session),
    session_user(Session, User),
    update_visitor_data(User, _, logout).
chat_event(visitor_count(Count)) :-             % request
    visitor_count(Count).
chat_event(visitor_count(Cluster, Local)) :-             % request
    visitor_count(Cluster),
    (   use_redis,
        redis_consumer(Consumer)
    ->  (   active_wsid_count(Consumer, Local)
        ->  true
        ;   Local = 0
        )
    ;   Local = Cluster
    ).

:- if(current_predicate(current_profile/2)).

chat_event(profile(ProfileID)) :-
    !,
    current_profile(ProfileID, Profile),
    http_session_id(Session),
    session_user(Session, User),
    update_visitor_data(User, Profile, login).

%!  propagate_profile_change(+ProfileID, +Attribute, +Value)
%
%   Trap external changes to the profile.

:- listen(user_profile(modified(ProfileID, Name, _Old, New)),
          propagate_profile_change(ProfileID, Name, New)).

propagate_profile_change(ProfileID, _, _) :-
    http_current_session(Session, profile_id(ProfileID)),
    session_user(Session, User),
    current_profile(ProfileID, Profile),
    update_visitor_data(User, Profile, 'profile-edit').

:- endif.

%!  broadcast_event(+Event) is semidet.
%
%   If true, broadcast this event.

broadcast_event(updated(_File, _Commit)).


%!  broadcast_event(+Event, +File, +WSID) is det.
%
%   Event happened that is related to File  in WSID. Broadcast it to
%   subscribed users as a notification. Always succeeds, also if the
%   message cannot be delivered.
%
%   @tbd    Extend the structure to allow other browsers to act.

broadcast_event(Event, File, WSID) :-
    wsid_session(WSID, Session),
    session_broadcast_event(Event, File, Session, WSID),
    !.
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

%!  event_html(+Event, -HTML:string) is det.
%
%   Describe an event as an HTML  message   to  be  displayed in the
%   client's notification area.

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
event_message(updated(File, _Commit)) -->
    html([ 'Saved ', \file(File) ]).
event_message(deleted(File, _From, _To)) -->
    html([ 'Deleted ', \file(File) ]).
event_message(closed(File)) -->
    html([ 'Closed ', \file(File) ]).
event_message(opened(File)) -->
    html([ 'Opened ', \file(File) ]).
event_message(download(File)) -->
    html([ 'Opened ', \file(File) ]).
event_message(download(Store, FileOrHash, Format)) -->
    { event_file(download(Store, FileOrHash, Format), File)
    },
    html([ 'Opened ', \file(File) ]).

file(File) -->
    html(a(href('/p/'+File), File)).

%!  event_file(+Event, -File) is semidet.
%
%   True when Event is associated with File.

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
                 *         NOTIFICATION         *
                 *******************************/

%!  chat_to_profile(ProfileID, :HTML) is det.
%
%   Send a HTML notification to users logged in using ProfileID.

chat_to_profile(ProfileID, HTML) :-
    (   http_current_session(Session, profile_id(ProfileID)),
        wsid_session(WSID, Session),
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
                 *             UI               *
                 *******************************/

%!  notifications(+Options)//
%
%   The  chat  element  is  added  to  the  navbar  and  managed  by
%   web/js/chat.js

notifications(_Options) -->
    { swish_config:config(chat, true) },
    !,
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

%!  broadcast_bell(+Options)//
%
%   Adds a bell to indicate central chat messages

broadcast_bell(_Options) -->
    { swish_config:config(chat, true),
      swish_config:config(hangout, Hangout),
      atom_concat('gitty:', Hangout, HangoutID)
    },
    !,
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
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message_context//1.

prolog:message_context(websocket(reconnect(Passed, Score))) -->
    [ 'WebSocket: too frequent reconnect requests (~1f sec; score = ~1f)'-
      [Passed, Score] ].
