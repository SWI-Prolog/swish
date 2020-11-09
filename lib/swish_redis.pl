/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, VU University Amsterdam
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

:- module(swish_redis,
          [ reinit_redis/0,
            redis_swish_stream/2,       % +Name, -Key
            redis_consumer/1            % -Consumer
          ]).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).
:- use_module(library(broadcast)).
:- use_module(library(option)).
:- use_module(library(socket)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- use_module(config).

/** <module> Redis stream connection

Setup to listening to redis events. We need all the _push_ facilities of
Redis:

  - redis_subscribe/4 to listen to volatile PUB/SUB messages
  - Listen on reliable redis streams using ``XREAD``
  - Listen on reliable redis streams using consumer groups

Note that config-available sets up  the   redis  server  using the alias
`swish`. Streams (redis keys) to  listen   on  are  registered using the
multifile predicate stream/2.
*/

:- multifile
    stream/2.

:- listen(http(pre_server_start(Port)),
          init_redis(Port)).

:- dynamic
    port/1,                             % Server port
    thread/1.                           % Listener thread.

init_redis(_Port) :-
    catch(thread_property(redis_listener, id(_)), error(_,_), fail),
    !.
init_redis(Port) :-
    init_pubsub,
    retractall(port(_)),
    asserta(port(Port)),
    findall(Group-S, group_stream(S, Group), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    consumer(Port, Consumer),
    maplist(create_listener(Consumer), Grouped),
    redis(swish, publish(swish:swish, joined(Consumer) as prolog), Count),
    print_message(informational, swish(redis_peers(Count))),
    at_halt(publish_halt(Consumer)).

publish_halt(Consumer) :-
    redis(swish, publish(swish:swish, left(Consumer) as prolog), _Count).

create_listener(_, (-)-Streams) :-
    !,
    thread_create(xlisten(swish, Streams, []),
                  Id, [ alias(redis_no_group)
                      ]),
    assertz(thread(Id)).
create_listener(Consumer, Group-Streams) :-
    atom_concat(redis_, Group, Alias),
    thread_create(xlisten_group(swish, Group, Consumer, Streams,
                                [ block(1)
                                ]),
                  Id, [ alias(Alias)
                      ]),
    assertz(thread(Id)).

%!  reinit_redis
%
%   Stop and start the redis thread. May   be  used to reconfigure it or
%   restart when crashed.

reinit_redis :-
    forall(retract(thread(Id)),
           catch(stop_listener(Id), error(_,_), true)),
    port(Port),
    init_redis(Port).

stop_listener(Id) :-
    thread_signal(Id, redis(stop(false))),
    thread_join(Id, _).

group_stream(Key, Group) :-
    stream(Name, Options),
    redis_swish_stream(Name, Key),
    option(max_len(MaxLen), Options, 1000),
    option(group(Group), Options, -),
    add_consumer_group(Group, Key),
    xstream_set(swish, Key, maxlen(MaxLen)).

add_consumer_group(-, _) :-
    !.
add_consumer_group(Group, Key) :-
    catch(redis(swish, xgroup(create, Key, Group, $, mkstream), _),
          error(redis_error(busygroup,_),_),
          true).

redis_swish_stream(Name, Key) :-
    swish_config(redis_prefix, Prefix),
    atomic_list_concat([Prefix, Name], :, Key).

%!  consumer(+Address, -Consumer) is det.
%
%   Find the name of this node in the  redis network. Each node needs to
%   have a name to be part of a Redis  consumer node, as well as to know
%   which sessions reside on which node.

:- dynamic consumer/1.

consumer(_, Consumer) :-
    consumer(Consumer0), !,
    Consumer = Consumer0.
consumer(Address, Consumer) :-
    address_consumer(Address, Consumer0),
    asserta(consumer(Consumer0)),
    Consumer = Consumer0.

address_consumer(_, Consumer) :-
    swish_config(redis_consumer, Consumer),
    !.
address_consumer(Host:Port, Consumer) :-
    !,
    atomic_list_concat([Host,Port], :, Consumer).
address_consumer(Port, Consumer) :-
    gethostname(Host),
    atomic_list_concat([Host,Port], :, Consumer).

%!  redis_consumer(-Consumer) is det.
%
%   True when Consumer is the name of this redis node.

redis_consumer(Consumer) :-
    consumer(Consumer).

init_pubsub :-
    redis_current_subscription(redis_pubsub, _),
    !.
init_pubsub :-
    redis_subscribe(swish,
                    [ swish:swish,    % Overall control
                      swish:chat,     % Chat broadcast messages
                      swish:gitty     % Gitty sync requests
                    ],
                    _,
                    [ alias(redis_pubsub)
                    ]).

:- initialization
    listen(redis(_, 'swish:swish', Message),
           swish_message(swish(Message))).

swish_message(Message) :-
    print_message(informational, Message).

:- multifile prolog:message//1.

prolog:message(swish(redis_peers(Count))) -->
    [ 'Redis: the are ~d peers in the cluster'-[Count] ].
prolog:message(swish(joined(Consumer))) -->
    (   { redis_consumer(Consumer) }
    ->  []
    ;   [ 'Redis: ~w joined the cluster'-[Consumer] ]
    ).
prolog:message(swish(left(Consumer))) -->
    (   { redis_consumer(Consumer) }
    ->  []
    ;   [ 'Redis: ~w left the cluster'-[Consumer] ]
    ).

