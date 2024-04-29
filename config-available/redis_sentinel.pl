/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020-2024, SWI-Prolog Solutions b.v.
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

:- module(config_redis, []).
% Edit with Redis consumer id.  Make sure each member of the cluster
% has a unique id.
swish:swish_node('<unassigned>').

/** <module> Configure Redis

SWISH may be configured to use the Redis key-value store for the various
databases. This allows multiple SWISH instances to act as a cluster.

Typically the configuration needs to be edited in two places:

  - redis_server/3 must be called to address the Redis server
  - redis_consumer may be set to identify this instance.  The
    default is derived from the host name and port to which
    this SWISH instance listens.  Clusters are advised to
    assign a stable name to each cluster member.
*/

:- multifile swish_config:config/2.

% Do not activate if we run SWISH in _ide_ mode
:- if(\+swish_config:config(ide,true)).

:- use_module(swish(lib/config), []).
:- use_module(library(redis)).
:- use_module(library(settings)).
:- use_module(library(lists)).
:- use_module(swish('config-available/user_profile')).
:- use_module(library(profile/backend/profile_redis), []).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_redis_plugin)).
:- use_module(swish(lib/chat), []).
:- use_module(swish(lib/cron)).

:- initialization
    redis_servers.

% Edit. List the sentinels. Normally we list   all of them. If there are
% more though, they will be picked up as long   as we can get hold of at
% least one from the list  below.  The   second  argument  is  the Redis
% consumer. We use this to find a   working replicator that has the same
% Redis consumer and thus  (hopefully)  has   the  lowest  latency. When
% found, this is configured as "read only" Redis DB.
sentinel('1.2.3.4':26379, node1).
sentinel('1.2.3.5':26379, node2).
sentinel('1.2.3.6':26379, node3).

% edit: passwords.  Note that the sentinel and redis passwords
% can be different.
redis_connect_options(
    [ user(swish),
      password("********"),
      version(3),
      tls(true),
      cacert('config-enabled/etc/redis/ca.crt'),
      key('config-enabled/etc/redis/client.key'),
      cert('config-enabled/etc/redis/client.cert'),
      sentinels(Sentinels),
      sentinel_user(query),
      sentinel_password("********")
    ]) :-
    findall(Sentinel, sentinel(Sentinel, _), Sentinels).

redis_servers :-
    redis_master_server(swish),
    redis_ro_server(swish, swish_ro),
    http_schedule_maintenance(daily(02:40), clear_subscriptions_by_channel).

redis_master_server(ServerId) :-
    redis_connect_options(Options),
    redis_server(ServerId, sentinel(swipl), Options).

:- dynamic ro_server/1 as volatile.

redis_ro_server(Master, SlaveServerId) :-
    redis_connect_options(Options),
    swish:swish_node(Consumer),
    sentinel(IP:_, Consumer),
    sentinel_slave(Master, swipl, Slave, Options),
    IP == Slave.ip,
    Port = Slave.port,
    redis_server(SlaveServerId, IP:Port, Options),
    assertz(ro_server(SlaveServerId)).
redis_ro_server(_, _).

swish_config:config(redis_ro, Server) :-
    ro_server(Server).
swish_config:config(redis, swish).
swish_config:config(redis_prefix, swish).
swish_config:config(redis_consumer, Consumer) :-
    swish:swish_node(Consumer).

:- set_setting(user_profile:redis_server, swish).
:- set_setting(user_profile:redis_prefix, 'swish:profiles').
:- set_setting(user_profile:backend, impl_profile_redis).
:- set_setting(user_profile:session_persistency, true).

:- http_set_session_options([ redis_db(swish),
                              redis_prefix('swish:http:session')
                            ]).

%!  clear_subscriptions_by_channel
%
%   Maintenance command to clear dangling  subscriptions. This should be
%   executed by at least one  of   the  cluster  members, preferably the
%   currrent master. For now, we just have all cluster members doing the
%   job.

clear_subscriptions_by_channel :-
    (   dangling_subscription(WSID, Channel, SubChannel),
        swish_chat:unsubscribe(WSID, Channel, SubChannel),
        fail
    ;   true
    ).

dangling_subscription(WSID, Channel, SubChannel) :-
    (   ro_server(Server)
    ->  true
    ;   Server = swish
    ),
    redis(Server, keys('swish:chat:channel:*'), ChKeys),
    member(ChKey, ChKeys),
    atom_concat('swish:chat:channel:', SubChannel, ChKey),
    redis_sscan(Server, ChKey, List, []),
    member(WSID-Channel, List),
    \+ swish_chat:current_wsid(WSID).

:- endif. % \+swish_config:config(ide,true)
