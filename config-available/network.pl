:- module(config_swish_network, []).
:- use_module(library(settings)).
:- use_module(library(http/http_host)).

/* <module> Set HTTP access parameters

The settings below are needed  for   actions  that  require a publically
accessible URL for the SWISH server if   this address cannot be resolved
automatically.
*/

%:- set_setting_default(http:public_host,   localhost).
%:- set_setting_default(http:public_port,   3050).
%:- set_setting_default(http:public_scheme, http).
