:- module(config_redis, []).
:- use_module(swish(lib/config), []).

:- use_module(library(redis)).

:- redis_server(swish, localhost:6379, []).

:- multifile swish_config:config/2.
swish_config:config(redis, swish).
swish_config:config(redis_prefix, swish).
%swish_config:config(redis_consumer, alice).
