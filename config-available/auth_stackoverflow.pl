/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
			 CWI Amsterdam
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

:- module(config_auth_stackoverflow, []).
:- use_module(swish(lib/oauth2)).
:- use_module(swish(lib/plugin/login)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(uri)).
:- use_module(library(debug)).
:- use_module(library(apply)).

/** <module> Enable login with stackexchange

This module allows for configures _login  with Stack exchange. To enable
this module:

  1. Follow these [steps](https://api.stackexchange.com/docs/authentication) to
     register your application on Stack Apps.  Get the following info:

     - The client ID
     - The client secret
     - The key
     - Register a redirect url.  To test from localhost, this should be
       =|http://localhost:3050/oauth2/stackexchange/reply|=

  2. COPY this file to =config-enabled=

  3. EDIT the server attributes
*/

:- multifile
    oauth2:login/3,
    oauth2:server_attribute/3,
    swish_config:login_item/2,          % -Server, -HTML_DOM
    swish_config:login/2,               % +Server, +Request
    swish_config:user_info/2.           % +Request, ?Server, -Info

:- http_set_session_options([create(noauto)]).

:- http_handler(swish(logout), stackexchange_logout, []).

%!  oauth2:server_attribute(?ServerID, ?Attribute, ?Value)
%
%   Declare properties of an oauth2 identity  provider. The values below
%   are for [StackExchange](http://stackexchange.com/) server.
%
%   EDIT:
%     - url: base URL of the stackexchange authentication server
%     - api_endpoint: Stack Apps API endpoint
%     - redirect_uri: our location
%     - client_id: Client Id from Stack Apps
%     - client_secret: Client Secret from Stack Apps
%     - key: Key from Stack Apps to access their API (/me)
%     - site: Stack exchange site for which to ask info
%
%   @see swish(lib/oauth2) for a description of the attributes.

oauth2:server_attribute(stackexchange, url,
                        'https://stackexchange.com').
oauth2:server_attribute(stackexchange, redirect_uri,
                        'http://localhost:3050/oauth2/stackexchange/reply').
oauth2:server_attribute(stackexchange, authorization_endpoint,
                        '/oauth').
oauth2:server_attribute(stackexchange, token_endpoint,
                        '/oauth/access_token').
oauth2:server_attribute(stackexchange, api_endpoint,
                        'https://api.stackexchange.com').
oauth2:server_attribute(stackexchange, client_id,
                        '****').
oauth2:server_attribute(stackexchange, client_secret,
                        '****').
oauth2:server_attribute(stackexchange, key,
                        '****').
oauth2:server_attribute(stackexchange, site,
                        'stackoverflow').
oauth2:server_attribute(stackexchange, scope,
                        '').


		 /*******************************
		 *          SWISH HOOKS		*
		 *******************************/

swish_config:login_item(stackexchange, 10-Item) :-
    http_absolute_location(icons('so-icon.png'), Img, []),
    Item = img([ src(Img),
                 class('login-with'),
                 'data-server'(stackexchange),
                 'data-frame'(popup),
                 title('Login with StackOverflow')
               ]).

swish_config:login(stackexchange, Request) :-
    oauth2_login(Request, [server(stackexchange)]).

oauth2:login(_Request, stackexchange, TokenInfo) :-
    debug(oauth, 'TokenInfo: ~p', [TokenInfo]),
    stackexchange_me(TokenInfo.access_token, Claim),
    debug(oauth, 'Claim: ~p', [Claim]),
    map_user_info(Claim, UserInfo),
    http_open_session(_SessionID, []),
    session_remove_user_data,
    http_session_assert(oauth2(stackexchange, TokenInfo)),
    http_session_assert(user_info(stackexchange, UserInfo)),
    reply_logged_in([ identity_provider('StackOverflow'),
                      name(UserInfo.name),
                      user_info(UserInfo)
                    ]).

%!  stackexchange_me(+AccessToken, -Info)
%
%   Stack exchange does not  support   the  oauth2 `user_info` endpoint.
%   Instead, we must use the =/me= API. This   is a little unlucky as we
%   need to duplicate some  infrastructure   from  the generic oauth2.pl
%   module.

stackexchange_me(AccessToken, Info) :-
    oauth2:server_attribute(stackexchange, api_endpoint,  URLBase),
    oauth2:server_attribute(stackexchange, client_id,     ClientID),
    oauth2:server_attribute(stackexchange, client_secret, ClientSecret),
    oauth2:server_attribute(stackexchange, key,           Key),
    oauth2:server_attribute(stackexchange, site,          Site),

    uri_extend(URLBase, '/2.2/me',
               [ key(Key),
                 site(Site),
                 access_token(AccessToken)
               ],
               URL),

    setup_call_cleanup(
        http_open(URL, In,
                  [ authorization(basic(ClientID, ClientSecret)),
                    header(content_type, ContentType),
                    status_code(Code)
                  ]),
        read_reply(Code, ContentType, In, Info0),
        close(In)),
    me_info(Info0, Info).

me_info(Info, Me) :-
    [Me] = Info.get(items),
    !.
me_info(Info, Info).


read_reply(Code, ContentType, In, Dict) :-
    debug(oauth, '/me returned ~p ~p', [Code, ContentType]),
    http_parse_header_value(content_type, ContentType, Parsed),
    read_reply2(Code, Parsed, In, Dict).

%!  read_reply2(+Code, +ContentType, +Stream, -Dict) is det.
%
%   Read the server reply as  a  dict.   Normally,  the  reply is a JSON
%   object, but stackexchange seems to  send   it  as a www-form-encoded
%   string.

read_reply2(200, media(application/json, _Attributes), In, Dict) :- !,
    json_read_dict(In, Dict).
read_reply2(Code, media(application/json, _Attributes), In,
            error{code:Code, details:Details}) :- !,
    json_read_dict(In, Details).
read_reply2(Code, Type, In,
            error{code:Code, message:Reply}) :-
    debug(oauth(token), 'Got code ~w, type ~q', [Code, Type]),
    read_string(In, _, Reply).


%!  stackexchange_logout(+Request)
%
%   Logout by removing the session data

stackexchange_logout(_Request) :-
    catch(session_remove_user_data, _, true),
    reply_logged_out([]).

%!  swish_config:user_info(+Request, ?Server, -Info:dict) is semidet.
%
%   True if Info represents describes the currently logged in user.

swish_config:user_info(_Request, stackexchange, UserInfo) :-
    http_in_session(_SessionID),
    http_session_data(user_info(stackexchange, UserInfo)).

%!  map_user_info(+OAuthInfo, -UserInfo) is det.
%
%   u{user:User, group:Group, name:Name, email:Email}

map_user_info(Dict0, Dict) :-
    dict_pairs(Dict0, Tag, Pairs0),
    maplist(map_user_field, Pairs0, Pairs),
    http_link_to_id(stackexchange_logout, [], LogoutURL),
    dict_pairs(Dict, Tag,
               [ identity_provider-stackexchange,
                 auth_method-oauth2,
                 logout_url-LogoutURL
               | Pairs
               ]).

map_user_field(display_name-Name, name-Name) :- !.
map_user_field(profile_image-URL, picture-URL) :- !.
map_user_field(link-URL,          profile_url-URL) :- !.
map_user_field(Field, Field).

session_remove_user_data :-
    http_session_retractall(oauth2(_,_)),
    http_session_retractall(user_info(_,_)).


		 /*******************************
		 *	     URI BASICS		*
		 *******************************/

%!  uri_extend(+Base:atom, +Rel:atom, +Query:list, -URI:atom) is det.
%
%   Create a URI from Base, A relative URI and a query.

uri_extend(Base, Relative, Query, URI) :-
    uri_resolve(Relative, Base, URI0),
    uri_extend_query(URI0, Query, URI).

%!  uri_extend_query(+URI0:atom, +Query:list, -URI:atom) is det.
%
%   Extend a URI with a query. If  URI0   already  has a query, keep all
%   parameters that do not conflict.

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


