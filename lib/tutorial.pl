:- module(tutorial, []).

user:file_search_path(tutorial, swish(tutorial)).
swish_config:source_alias(tutorial, [access(read), search('*.{swinb}')]).

