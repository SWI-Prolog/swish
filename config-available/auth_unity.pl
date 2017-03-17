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

:- module(auth_config_unity, []).
:- use_module(swish(lib/oauth2)).
:- use_module(swish(lib/plugin/login)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(debug)).

/** <module> Enable login with a Unity-idm server

Client config to login to a Unity-idm   server using oauth2. This oauth2
config has the following properties:

  - The login page does not open in an iframe due to
    =|X-Frame-Options: DENY|=.  Therefore we need `'data-frame'(popup)`.
  - The server does not provide a JWT `id_token`, so we must explicitly
    use the `userinfo_endpoint`.
  - The server has no discovery endpoint and thus we must specify all
    URLs explicitly.  Note that the endpoints can be configured in the
    Unity configuration and thus may be wrong for your setup.
  - As we use self-signed SSL certificates we must set
    `cert_verify_hook` to accept any certificate.

@see http://www.unity-idm.eu/
*/

:- multifile
    oauth2:login/3,
    oauth2:server_attribute/3,
    swish_config:login_item/2,          % -Server, -HTML_DOM
    swish_config:login/2,               % +Server, +Request
    swish_config:user_info/2.           % +Request, ?Server, -Info

:- http_set_session_options([create(noauto)]).

:- http_handler(swish(logout), unity_logout, []).

swish_config:login_item(unity, 10-Item) :-
    http_absolute_location(icons('unity.png'), Img, []),
    Item = img([ src(Img),
                 class('login-with'),
                 'data-server'(unity),
                 'data-frame'(popup),
                 title('Login with Unity')
               ]).

swish_config:login(unity, Request) :-
    oauth2_login(Request, [server(unity)]).

oauth2:login(_Request, unity, TokenInfo) :-
    debug(oauth, 'Token: ~p', [TokenInfo]),
    oauth2_user_info(unity, TokenInfo, Claim),
    debug(oauth, 'Claim: ~p', [Claim]),
    map_user_info(Claim, UserInfo),
    http_open_session(_SessionID, []),
    session_remove_user_data,
    http_session_assert(oauth2(unity, TokenInfo)),
    http_session_assert(user_info(unity, UserInfo)),
    reply_logged_in([ identity_provider('Unity'),
                      name(UserInfo.name),
                      user_info(UserInfo)
                    ]).

%!  map_user_info(+OAuthInfo, -UserInfo) is det.
%
%   u{user:User, group:Group, name:Name, email:Email}

map_user_info(Dict0, Dict) :-
    dict_pairs(Dict0, Tag, Pairs0),
    maplist(map_user_field, Pairs0, Pairs),
    http_link_to_id(unity_logout, [], LogoutURL),
    dict_pairs(Dict, Tag,
               [ auth_method-oauth2,
                 logout_url-LogoutURL,
                 identity_provider-unity
               | Pairs
               ]).

map_user_field(cn-Name, name-Name) :- !.
map_user_field(Field, Field).

%!  unity_logout(+Request)
%
%   Logout by removing the session data

unity_logout(_Request) :-
    catch(session_remove_user_data, _, true),
    reply_logged_out([]).

session_remove_user_data :-
    http_session_retractall(oauth2(_,_)),
    http_session_retractall(user_info(_,_)).

%!  swish_config:user_info(+Request, ?Server, -Info:dict) is semidet.
%
%   True if Info represents describes the currently logged in user.

swish_config:user_info(_Request, unity, UserInfo) :-
    http_in_session(_SessionID),
    http_session_data(user_info(unity, UserInfo)).

%!  oauth2:server_attribute(?Server, ?Attribute, ?Value)
%
%   EDIT:
%     - url: base URL of the Unity server
%     - redirect_uri: our location
%     - client_id: userName of the oauth-clients user
%     - client_secret: password of the above user
%     - profile: the configured scope in Unity

oauth2:server_attribute(unity, url,            'https://woezel.ia.cwi.nl:2443').
oauth2:server_attribute(unity, redirect_uri,
                        'http://localhost:3050/oauth2/unity/reply').
oauth2:server_attribute(unity, authorization_endpoint,
                        '/oauth2-as/oauth2-authz').
oauth2:server_attribute(unity, token_endpoint,     '/oauth2/token').
oauth2:server_attribute(unity, userinfo_endpoint,  '/oauth2/userinfo').
oauth2:server_attribute(unity, tokeninfo_endpoint, '/oauth2/tokeninfo').
oauth2:server_attribute(unity, client_id,          '****').
oauth2:server_attribute(unity, client_secret,      '****').
oauth2:server_attribute(unity, scope,              profile).
% EDIT: We use a self-signed certificate.  Remove/comment for production
oauth2:server_attribute(unity, cert_verify_hook,   cert_accept_any).
