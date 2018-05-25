:- module(swish_config_dim, []).
:- use_module(library(settings)).
:- multifile swish_config:config/2.

/** <module> Configure SWISH community dimensions

SWISH is both used for small  installations   were  a small community of
trusted users work on an application   and for large installations where
many users work anonymously. The  default   configuration  aims at small
communities as this is most likely the   first usage and the most common
installation.

This file (dim.pl) merely repeats the   declarations  from swish.pl. The
file `dim_large.pl` is a starting point for large installations.
*/

%	  - show_beware
%	  If `true`, show the *Beware* modal dialog on startup
%	  - tabled_results
%	  If `true`, check the _table results_ checkbox by default.
%	  - community_examples
%	  Allow marking saved programs as example.  If marked, the
%	  programs are added to the Examples menu.
%	  - ping
%	  Ping pengine status every N seconds.
%	  - chat
%	  Activate the chat interface
%	  - default_query
%	  Initial query for the source search in an empty tab

swish_config:config(show_beware,        false).
swish_config:config(tabled_results,     false).
swish_config:config(community_examples, true).
swish_config:config(ping,		2).
swish_config:config(chat,		true).
swish_config:config(default_query,	'').

% swish:slave_limit controls the max number of pengines per client
% swish:thread_pool_size controls the max number of concurrent pengines
% swish:thread_pool_stacks controls the pengine thread creation options

:- set_setting_default(swish:slave_limit,        10).
:- set_setting_default(swish:thread_pool_size,   100).
:- set_setting_default(swish:thread_pool_stacks, [/*stack_limit(1 000 000 000)*/]).
