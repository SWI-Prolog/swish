:- module(swish_config_clpBNR, []).

:- use_module(library(clpBNR)).

:- multifile user:file_search_path/2.

user:file_search_path(example, pack('clpBNR/swish')).
