/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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

:- module(swish_config,
	  [ swish_reply_config/2,	% +Request, +Options
	    swish_config/2,		% ?Type, ?Config
	    swish_config_hash/2		% -HASH, +Options
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(option)).

:- multifile
	config/2,			% ?Key, ?Value
	config/3,			% ?Key, ?Value, +Options
	source_alias/2,			% ?Alias, ?Options
	authenticate/2,			% +Request, -User
        login_item/2,                   % -Server, -HTML_DOM
        login/2,                        % +Server, +Request
        user_info/3.                    % +Request, -Server, -Info

/** <module> Make HTTP locations known to JSON code
*/

		 /*******************************
		 *	       CONFIG		*
		 *******************************/

%%	swish_reply_config(+Request, +Options) is semidet.
%
%	Emit a configuration object to the client if the client requests
%	for '.../swish_config.json', regardless  of   the  path  prefix.

swish_reply_config(Request, Options) :-
	option(path(Path), Request),
	file_base_name(Path, 'swish_config.json'),
	json_config(JSON, Options),
	reply_json(JSON).

%%	swish_config_hash(-Hash, +Options) is det.
%
%	True if Hash is the SHA1 of the SWISH config.

swish_config_hash(Hash, Options) :-
	json_config(Config, Options),
	variant_sha1(Config, Hash).

json_config(json{ http: json{ locations:JSON
			    },
		  swish: SWISHConfig
		}, Options) :-
	http_locations(JSON),
	swish_config_dict(SWISHConfig, Options).

http_locations(JSON) :-
	findall(ID-Path,
		( http_current_handler(Path, _:_, Options),
		  memberchk(id(ID), Options)
		), Pairs),
	keysort(Pairs, Sorted),
	remove_duplicate_ids(Sorted, Cleaned),
	dict_pairs(JSON, json, Cleaned).

remove_duplicate_ids([], []).
remove_duplicate_ids([Id-Path1,Id-Path2|T], [Id-Path1|Cleaned]) :- !,
	same_ids(T, Id, T1, Paths0),
	sort([Path1,Path2|Paths0], Unique),
	(   Unique = [_]
	->  true
	;   print_message(warning, http(duplicate_handlers(Id, Unique)))
	),
	remove_duplicate_ids(T1, Cleaned).
remove_duplicate_ids([H|T0], [H|T]) :-
	remove_duplicate_ids(T0, T).

same_ids([], _, [], []).
same_ids([Id-Path|T0], Id, T, [Path|TP]) :- !,
	same_ids(T0, Id, T, TP).
same_ids(T, _, T, []).


%%	swish_config_dict(-Config:dict, +Options) is det.
%
%	Obtain name-value pairs from swish_config:config/2

swish_config_dict(Config, Options) :-
	findall(Key-Value, swish_config(Key, Value, Options), Pairs),
	dict_pairs(Config, json, Pairs).

%%	config(-Key, -Value) is nondet.
%%	swish_config(-Key, -Value) is nondet.
%
%	Define a name/value pair that will end   up  in the SWISH config
%	object (see =web/js/config.js=)

swish_config(Key, Value) :-
	swish_config(Key, Value, []).

swish_config(Key, Value, Options) :-
	config(Key, Value, Options).
swish_config(Key, Value, _) :-
	config(Key, Value).

% We need to use '$swish wrapper' with a variable _residuals in
% versions that support the `var_prefix` option.
:- if(current_prolog_flag(var_prefix, _)).
config(residuals_var, '_residuals').
:- endif.

		 /*******************************
		 *             LOGIN		*
		 *******************************/

%!	login_item(-Server, -Item) is nondet.
%
%	This hook is called  to  find   all  possible  login options. It
%	should bind Item to an HTML description for html//1 that must be
%	clicked to login  with  this  option.   The  item  may  have the
%	following HTML attributes:
%
%	  - 'data-server'(+Server)
%	  This must be present and provides the first argument for the
%	  login/2 hook.
%
%	  - 'data-frame'(+Style)
%	  The login is realised in a popup to avoid reloading the
%	  current swish page.  If Style is `popup`, a browser popup window
%	  is used. This is necessary for identity providers that refuse to
%	  open inside a frame. The default is `iframe`, which handles
%	  the login inside an =iframe= element in a modal popup.
%
%	The Item is often  an  image.  The   image  must  have  a  class
%	=login-with=. Below is an example to login with Google:
%
%	```
%	swish_config:login_item(Item) :-
%	    http_absolute_location(icons('social_google_box.png'), Img, []),
%	    Item = img([ src(Img),
%	                 class('login-with'),
%	                 'data-server'(google),
%	                 title('Login with Google')
%	               ]).
%	```
%
%	@arg Item may be of the form  `Tag-Item`. In this case the items
%	are ordered by Tag. The default tag is `0`.

%!	login(+Server, +Request) is det.
%
%	If a login item with   `'data-server'(+Server)`  is clicked, the
%	HTTP handler with id `login` is called. This handler figures the
%	selected login _server_ and calls this hook.

%!	user_info(+Request, -Server, -UserInfo:dict) is semidet.
%
%	Each login facility must provide  this   hook.  The  hook should
%	succeed if-and-only-if the user is logged in using this facility
%	and the hook must bind UserInfo with   a  dict that contains the
%	following fields:
%
%	  - user: User
%	  User name (id) if the logged in user.
%	  - name: Name
%	  Common name of the logged in user.
%	  - email: Email
%	  Email address of the logged in user.
%	  - picture: URL
%	  If present, URL is used to indicate the currently logged in
%	  user.
%	  - auth_method: Method
%	  Authentication method used. Currently one of `basic`, `digest`
%	  or `oauth2`.
%	  - logout_url: URL
%	  URL that must be used to logout.  Needed if `auth_method` is
%	  not one of the HTTP authentication methods (`basic` or
%	  `digest`).
%
%	If this hook fails the user is not logged in.


		 /*******************************
		 *          OTHER HOOKS		*
		 *******************************/

%%	source_alias(?Alias, ?Options) is nondet.
%
%	Multifile hook that  defines   properties  of file_search_path/2
%	aliases wrt. storage handling.  Defined options are:
%
%	  - access(Access)
%	  One of `read` or `both`.
%	  - search(Pattern)
%	  The _New Tab_ search form searches in files that satisfy the
%	  given pattern in the matching directories.  Pattern is handed
%	  to expand_file_name/2.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(http(duplicate_handlers(Id, Paths))) -->
	[ 'Duplicate HTTP handler IDs: "~w"'-[Id] ],
	paths(Paths).

paths([]) --> [].
paths([H|T]) --> [ '\t~q'-[H], nl ], paths(T).
