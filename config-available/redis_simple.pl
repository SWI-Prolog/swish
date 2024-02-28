/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.
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

% Do not activate if `config(redis,false)` is present
:- if(\+swish_config:config(ide,true)).

:- use_module(swish(lib/config), []).
:- use_module(library(redis)).
:- use_module(library(settings)).
:- use_module(swish('config-available/user_profile')).
:- use_module(library(profile/backend/profile_redis), []).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_redis_plugin)).

:- redis_server(swish, localhost:6379, []).

swish_config:config(redis, swish).
swish_config:config(redis_prefix, swish).
%swish_config:config(redis_consumer, peter).

:- set_setting(user_profile:redis_server, swish).
:- set_setting(user_profile:redis_prefix, 'swish:profiles').
:- set_setting(user_profile:backend, impl_profile_redis).
:- set_setting(user_profile:session_persistency, true).

:- http_set_session_options([ redis_db(swish),
                              redis_prefix('swish:http:session')
                            ]).

:- endif. % \+swish_config:config(ide,true)
