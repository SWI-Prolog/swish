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
            redis_swish_stream/2
          ]).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).
:- use_module(library(broadcast)).
:- use_module(library(option)).
:- use_module(library(socket)).

:- use_module(config).

/** <module> Redis stream connection

Setup a thread to listen to redis connections.

Note that config-available sets up  the   redis  server  using the alias
`swish`. Streams (redis keys) to  listen   on  are  registered using the
multifile predicate stream/2.
*/

:- multifile
    stream/2.

:- listen(http(pre_server_start(Port)),
          init_redis(Port)).

:- dynamic port/1.

init_redis(_Port) :-
    catch(thread_property(redis_listener, id(_)), error(_,_), fail),
    !.
init_redis(Port) :-
    retractall(port(_)),
    asserta(port(Port)),
    findall(S, stream(S), Streams),
    consumer(Port, Consumer),
    thread_create(xlisten_group(swish, swish, Consumer, Streams,
                                [ block(1)
                                ]),
                  _, [ alias(redis_listener)
                     ]).

%!  reinit_redis
%
%   Stop and start the redis thread. May   be  used to reconfigure it or
%   restart when crashed.

reinit_redis :-
    catch(stop_listener, error(_,_), true),
    port(Port),
    init_redis(Port).

stop_listener :-
    thread_signal(redis_listener, redis(stop(false))),
    thread_join(redis_listener, _).

stream(Key) :-
    stream(Name, Options),
    redis_swish_stream(Name, Key),
    option(max_len(MaxLen), Options, 1000),
    add_consumer_group(Key),
    xstream_set(swish, Key, maxlen(MaxLen)).

add_consumer_group(Key) :-
    catch(redis(swish, xgroup(create, Key, swish, $, mkstream), _),
          error(redis_error(busygroup,_),_),
          true).

redis_swish_stream(Name, Key) :-
    swish_config(redis_prefix, Prefix),
    atomic_list_concat([Prefix, Name], :, Key).

consumer(_, Consumer) :-
    swish_config(redis_consumer, Consumer),
    !.
consumer(Host:Port, Consumer) :-
    !,
    atomic_list_concat([Host,Port], :, Consumer).
consumer(Port, Consumer) :-
    gethostname(Host),
    atomic_list_concat([Host,Port], :, Consumer).

