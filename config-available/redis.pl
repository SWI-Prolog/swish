:- module(config_redis, []).
:- use_module(swish(lib/config), []).
:- use_module(library(redis)).
:- use_module(library(settings)).
:- use_module(swish('config-available/user_profile')).
:- use_module(library(profile/backend/profile_redis), []).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_redis_plugin)).

:- redis_server(swish, localhost:6379, []).

:- multifile swish_config:config/2.
swish_config:config(redis, swish).
swish_config:config(redis_prefix, swish).
%swish_config:config(redis_consumer, alice).

:- set_setting(user_profile:redis_server, swish).
:- set_setting(user_profile:redis_prefix, 'swish:profiles').
:- set_setting(user_profile:backend, impl_profile_redis).
:- set_setting(user_profile:session_persistency, true).

:- http_set_session_options([ redis_db(swish),
                              redis_prefix('swish:http:session')
                            ]).
