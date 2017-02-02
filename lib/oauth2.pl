/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, CWI Amsterdam
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

:- module(oauth2,
	  [ oauth2_login/2,			% +Request, +Options
	    oauth2_reply/2,			% +Request, +Options
	    oauth2_validate_access_token/3,	% +ServerID, +AccessToken,
						% -Info:dict
	    oauth2_user_info/3,			% +ServerID, +TokenInfo, -UserInfo
	    oauth2_claim/2			% +TokenInfo, -Claim
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/json)).
:- use_module(library(base64)).
:- use_module(library(utf8)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(apply)).

/** <module> Oauth2 based login

This  module  provides  oauth2  based  login.  Oauth2  is  a  _federated
identity_  protocol.  It  allows  a  user  to  login  to  a  service  by
redirecting to an _identity provider_. After   validating  the user, the
identity provider redirects back  to  our   service.  In  the process we
obtain an anonymous  identifier  for  the   user  and  optionally  _user
attributes_ such as the user's name, email, etc.

As oauth2 does not use HTTP  authentication   the  fact  that a user has
logged in must be handled using an HTTP session.

Using this module requires the user to define two _hooks_:

  - server_attribute/3 defines properties about one or more _identity
    providers_.
  - login/3 establishes the login, typically by ensuring an HTTP session
    and relating the identity to this session.
*/

:- multifile
	server_attribute/3,		% +ServerID, +Attribute, -Value
	login/3.			% +Request, +ServerID, +TokenInfo

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(oauth2, root(oauth2), [priority(-100)]).

:- http_handler(oauth2(.), oauth2, [prefix]).

%!	server_attribute(?ServerID, ?Attribute, ?Value) is nondet.
%
%	Multifile hook that defines available   oauth2 servers. ServerID
%	is our internal  nickname  for   the  oauth2  identity provider.
%	Attribute and Value provide the various   attributes  we need to
%	know to contact the server. Defined attributes are:
%
%	  - url
%	  Base URL for the identity provider.  Normally points at the
%	  root of the server.  Other locations are relative to this URL.
%
%	  - redirect_uri
%	  URI to which the identity provider will redirect back.  This
%	  is the public URL for oauth2(ServerID/reply).  It may be left
%	  undefined if the server can find its own location.  This URI
%	  is normally registered with the identity provider.
%
%	  - discovery_endpoint
%	  Endpoint for automatic configuration.  The default is `url`,
%	  followed by =|/.well-known/openid-configuration|=.  The
%	  discovery URL is used if one of the other required attributes
%	  is not defined by the hook.
%
%	  - authorization_endpoint
%	  Path on the identity provider that initiates a login.  The
%	  default is obtained from the `discovery_endpoint.
%
%	  - token_endpoint
%	  Location to validate the access code and obtain an access
%	  token.   The default is obtained from the `discovery_endpoint.
%
%	  - userinfo_endpoint
%	  Path to get info on the user from the access token. The
%	  default is obtained from the `discovery_endpoint.
%
%	  - tokeninfo_endpoint
%	  Needed for _implicit_ and _hybrid_ login flows (typically not
%	  used by servers)
%
%	  - client_id
%	  Identity by which we are known at the identity provider.
%
%	  - client_secret
%	  Secret we need to identify ourselves with the identity
%	  provider
%
%	  - scope
%	  Set of attributes we wish to have from the identity provider.
%
%	  - cert_verify_hook
%	  Set the certificate verification hook.  Default is to verify
%	  the certificate.  If set to `cert_accept_any`, any certificate
%	  is accepted. This can be used to deal with self-signed
%	  certificates in expertimental setups.

%!	oauth2(+Request)
%
%	HTTP handler to deal with oauth2   requests. The addresses served
%	are
%
%	  * oauth2(Server/login)
%	  * oauth2(Server/reply)

oauth2(Request) :-
	option(path_info(Path), Request),
	atomic_list_concat([ServerID,Action], /, Path), !,
	oauth2(Action, ServerID, Request).
oauth2(Request) :-
	http_404([], Request).

oauth2(_, ServerID, Request) :-
	\+ server_attribute(ServerID, _, _), !,
	http_404([], Request).
oauth2(login, ServerID, Request) :- !,
	oauth2_login(Request, [server(ServerID)]).
oauth2(reply, ServerID, Request) :- !,
	oauth2_reply(Request, [server(ServerID)]).
oauth2(_, _, Request) :- !,
	http_404([], Request).


%!	oauth2_login(+Request, +Options)
%
%	HTTP handler to login using oauth2. It  causes a redirect to the
%	oauth2  identity  server,   which   will    redirect   back   to
%	oauth2(reply).

oauth2_login(Request, Options) :-
	option(server(Server), Options),
	oauth2_redirect_uri(Server, URI),
	debug(oauth, 'Redirect to ~p', [URI]),
	http_redirect(see_other, URI, Request).

oauth2_redirect_uri(ServerID, URI) :-
	server_attr(ServerID, url,		      ServerURI),
	server_attr(ServerID, authorization_endpoint, Path),
	server_attr(ServerID, redirect_uri,	      RedirectURI),
	server_attr(ServerID, client_id,	      ClientID),
	server_attr(ServerID, scope,		      Scope),

	anti_forgery_state(AntiForgery),
	get_time(Now),
	asserta(forgery_state(AntiForgery, ServerID, RedirectURI, Now)),

	uri_extend(ServerURI, Path,
		   [ response_type(code),
		     client_id(ClientID),
		     redirect_uri(RedirectURI),
		     scope(Scope),
		     state(AntiForgery)
		   ], URI).


%!	oauth2_reply(+Request, +Options)
%
%	HTTP handler for the  redirect  we   get  back  from  the oauth2
%	server.
%
%	@tbd Deal with `expires_in` and `id_token` fields.

oauth2_reply(Request, Options) :-
	option(server(ServerID), Options),
	http_parameters(Request,
			[ code(AuthCode, [string]),
			  state(State, [])
			]),
	debug(oauth, 'Code: ~p', [AuthCode]),
	validate_forgery_state(State, _ServerID, _Redirect),
	debug(oauth, 'State: OK', []),
	oauth2_token_details(ServerID, AuthCode, TokenInfo),
	call_login(Request, ServerID, TokenInfo).

%!	login(+Request, +ServerID, +TokenInfo) is semidet.
%
%	Multifile hook to realise the actual   login. Normally this hook
%	shall create a session  and  associate   the  session  with  the
%	identity of the user.  This  hook  may   keep  track  of  a user
%	profile.
%
%	If  this  hook  fails,  oauth2_reply/2  returns  a  `text/plain`
%	document with the obtained information.  This   can  be used for
%	debugging and development purposes.
%
%	@arg Request is the HTTP request dealing with the redirect back
%	from the identity provider.
%	@arg ServerID identifies the identity provider.
%	@arg TokenInfo is a dict containing information about the access
%	token.
%	@arg UserInfo is a dict containing information about the user.

call_login(Request, ServerID, TokenInfo) :-
	login(Request, ServerID, TokenInfo).
call_login(_Request, ServerID, TokenInfo) :-
	oauth2_user_info(ServerID, TokenInfo, UserInfo),
	format('Content-type: text/plain~n~n'),
	format('Oauth2 login using ~w succeeded~n', [ServerID]),
	format('Token info: ~n'),
	print_term(TokenInfo, [output(current_output)]),
	format('~nUser info: ~n'),
	print_term(UserInfo, [output(current_output)]).

%!	oauth2_validate_access_token(+ServerID, +AccessToken, -Info:dict)
%
%	Validates the AccessToken with  Unity   (_implicit_  or _hybrid_
%	flow).

oauth2_validate_access_token(ServerID, AuthCode, Info) :-
	server_attr(ServerID, url,		  ServerURI),
	server_attr(ServerID, tokeninfo_endpoint, Path),

	uri_extend(ServerURI, Path, [], URI),
	http_options(ServerID, Options),

	setup_call_cleanup(
	    http_open(URI, In,
		      [ authorization(bearer(AuthCode)),
			header(content_type, ContentType),
			status_code(Code)
		      | Options
		      ]),
	    read_reply(Code, ContentType, In, Info),
	    close(In)).

%!	oauth2_user_info(+ServerID, +TokenInfo, -UserInfo) is det.
%
%	Given the token details obtained in oauth2_reply/2, get extended
%	information about the user from the identity provider. TokenInfo
%	is a dict that must contain `access_token`.

oauth2_user_info(ServerID, TokenInfo, UserInfo) :-
	user_info(ServerID, TokenInfo.access_token, UserInfo).


%!	user_info(+ServerID, +BearerToken, -Info:dict) is det.
%
%	Ask info about a user.

user_info(ServerID, AccessToken, Info) :-
	server_attr(ServerID, url,	     ServerURI),
	server_attr(ServerID, userinfo_endpoint, Path),

	uri_extend(ServerURI, Path, [], URI),
	http_options(ServerID, Options),

	setup_call_cleanup(
	    http_open(URI, In,
		      [ authorization(bearer(AccessToken)),
			header(content_type, ContentType),
			status_code(Code)
		      | Options
		      ]),
	    read_reply(Code, ContentType, In, Info),
	    close(In)).

%!	oauth2_token_details(+ServerID, +AuthCode, -Info:dict)
%
%	Get information using the provided code.   This  is used for the
%	_code_ flow.

oauth2_token_details(ServerID, AuthCode, Dict) :-
	server_attr(ServerID, url,	      ServerURI),
	server_attr(ServerID, token_endpoint, Path),
	server_attr(ServerID, redirect_uri,   RedirectURI),
	server_attr(ServerID, client_id,      ClientID),
	server_attr(ServerID, client_secret,  ClientSecret),

	uri_extend(ServerURI, Path, [], URI),
	http_options(ServerID, Options),

	setup_call_cleanup(
	    http_open(URI, In,
		      [ authorization(basic(ClientID, ClientSecret)),
			post(form([ grant_type(authorization_code),
				    scope(profile),
				    code(AuthCode),
				    redirect_uri(RedirectURI),
				    client_id(ClientID),
				    client_secret(ClientSecret)
				  ])),
			request_header('Accept'='application/json;q=1.0,\c
					         */*;q=0.1'),
			header(content_type, ContentType),
			status_code(Code)
		      | Options
		      ]),
	    read_reply(Code, ContentType, In, Dict),
	    close(In)).

read_reply(Code, ContentType, In, Dict) :-
	debug(oauth, 'Token details returned ~p ~p', [Code, ContentType]),
	http_parse_header_value(content_type, ContentType, Parsed),
	read_reply2(Code, Parsed, In, Dict).

%!	read_reply2(+Code, +ContentType, +Stream, -Dict) is det.
%
%	Read the server reply as a dict.   Normally, the reply is a JSON
%	object, but stackexchange seems to send it as a www-form-encoded
%	string.

read_reply2(200, media(application/json, _Attributes), In, Dict) :- !,
	json_read_dict(In, Dict).
read_reply2(200, media(text/plain, _Attributes), In, Dict) :- !,
	read_string(In, _, Reply),
	uri_query_components(Reply, Fields0),
	maplist(convert_field, Fields0, Fields),
	dict_create(Dict, _, Fields).
read_reply2(Code, media(application/json, _Attributes), In,
	   error{code:Code, details:Details}) :- !,
	json_read_dict(In, Details).
read_reply2(Code, Type, In,
	   error{code:Code, message:Reply}) :-
	debug(oauth(token), 'Got code ~w, type ~q', [Code, Type]),
	read_string(In, _, Reply).

convert_field(expires=Atom, expires=Number) :-
	atom_number(Atom, Number), !.
convert_field(Field, Field).


%!	server_attr(+ServerID, +Attr, -Value) is det.
%
%	True when Value is the value for Attr on ServerID.

server_attr(ServerID, Attr, Value) :-
	(   server_attribute(ServerID, Attr, Value0)
	->  Value = Value0
	;   default_attribute(Attr, ServerID, Value0)
	->  Value = Value0
	;   existence_error(oauth2_server_attribute, Attr)
	).

%!	default_attribute(+Attr, +ServerID, -Value0) is semidet.
%
%	Compute a default value for a server attribute.

default_attribute(redirect_uri, ServerID, URI) :- !,
	http_current_request(Request),
	http_public_host_url(Request, HostURL),
	http_absolute_location(oauth2(ServerID/reply), Path, []),
	atom_concat(HostURL, Path, URI).
default_attribute(discovery_endpoint, ServerID, URI) :- !,
	server_attr(ServerID, url, Base),
	uri_extend(Base, '/.well-known/openid-configuration', [], URI).
default_attribute(cert_verify_hook, _, Hook) :- !,
	Hook = default.
default_attribute(url, _, _) :- !,
	fail.
default_attribute(Attribute, ServerID, URI) :-
	oauth2_discover(ServerID, Dict),
	URI = Dict.get(Attribute).

%!	http_options(+ServerID, -Options:list) is det.
%
%	Provide additional options  for  http_open/3   to  talk  to  the
%	identity provider.

http_options(ServerID, Options) :-
	server_attr(ServerID, cert_verify_hook, Hook),
	Hook \== default, !,
	Options = [ cert_verify_hook(Hook) ].
http_options(_, []).


		 /*******************************
		 *      ANTI FORGERY STATE	*
		 *******************************/

:- dynamic forgery_state/4.

validate_forgery_state(State, Site, Redirect) :-
	(   forgery_state(State, Site, Redirect, Stamp)
	->  retractall(forgery_state(State, Site, Redirect, Stamp))
	;   throw(http_reply(not_acceptable('Invalid state parameter')))
	).

anti_forgery_state(State) :-
	Rand is random(1<<100),
	variant_sha1(Rand, State).


		 /*******************************
		 *	METADATA DISCOVERY	*
		 *******************************/

%%	oauth2_discover(+ServerID, -Dict) is det.
%
%	True when Dict represents _The Discovery document_.

:- dynamic
	discovered_data/3.		% URL, Time, Data

oauth2_discover(ServerID, Dict) :-
	(   discovered_data(ServerID, Dict0)
	->  Dict = Dict0
	;   discover_data(ServerID, Expires, Dict0),
	    cache_data(ServerID, Expires, Dict0),
	    Dict = Dict0
	).

discover_data(ServerID, Expires, Dict) :-
	server_attr(ServerID, discovery_endpoint, DiscoverURL),
	http_options(ServerID, Options),

	http_open(DiscoverURL, In,
                  [ header(expires, Expires)
		  | Options
		  ]),
	json_read_dict(In, Dict),
	close(In).

discovered_data(URL, Data) :-
	discovered_data(URL, Expires, Data0),
	get_time(Now),
	(   Now =< Expires
	->  Data = Data0
	;   retractall(discovered_data(URL, Expires, _)),
	    fail
	).

cache_data(URL, Expires, Data) :-
	parse_time(Expires, _Format, Stamp), !,
	asserta(discovered_data(URL, Stamp, Data)).
cache_data(_, _, _).


		 /*******************************
		 *	     URI BASICS		*
		 *******************************/

%!	uri_extend(+Base:atom, +Rel:atom, +Query:list, -URI:atom) is det.
%
%	Create a URI from Base, A relative URI and a query.

uri_extend(Base, Relative, Query, URI) :-
	uri_resolve(Relative, Base, URI0),
	uri_extend_query(URI0, Query, URI).

%!	uri_extend_query(+URI0:atom, +Query:list, -URI:atom) is det.
%
%	Extend a URI with a query. If URI0 already has a query, keep all
%	parameters that do not conflict.

uri_extend_query(URI0, Query, URI) :-
	uri_components(URI0, Components0),
	extend_query(Components0, Query, Query1),
	uri_data(search, Components0, Query1, Components1),
	uri_components(URI, Components1).

extend_query(Components, QueryEx, Query) :-
	uri_data(search, Components, Query0),
	(   var(Query0)
	->  uri_query_components(Query, QueryEx)
	;   uri_query_components(Query0, Q0),
	    merge_components(Q0, QueryEx, Q),
	    uri_query_components(Query, Q)
	).

merge_components([], Q, Q).
merge_components([N=_|T0], Q1, Q) :-
	memberchk(N=_, Q1), !,
	merge_components(T0, Q1, Q).
merge_components([H|T0], Q1, [H|Q]) :-
	merge_components(T0, Q1, Q).


		 /*******************************
		 *		JWT		*
		 *******************************/

%%	oauth2_claim(+TokenInfo, -Claim) is semidet.
%
%	True when Claim is the claim made in TokenInfo.

oauth2_claim(TokenInfo, Claim) :-
	jwt(TokenInfo.get(id_token), Claim).


%%	jwt(+String, -Object) is det.
%
%	True if Object is claimed in the JWT represented in String.
%
%	@tbd Currently does not validate the claim using the signature.

jwt(String, Object) :-
	nonvar(String),
	split_string(String, ".", "", [Header64,Object64|_Parts]),
	base64url_json(Header64, _Header),
	base64url_json(Object64, Object).

%%	base64url_json(+String, -JSONDict) is semidet.
%
%	True when JSONDict is represented  in   the  Base64URL and UTF-8
%	encoded String.

base64url_json(String, JSON) :-
	string_codes(String, Codes),
	phrase(base64url(Bytes), Codes),
	phrase(utf8_codes(Text), Bytes),
	setup_call_cleanup(
	    open_string(Text, Stream),
	    json_read_dict(Stream, JSON),
	    close(Stream)).
