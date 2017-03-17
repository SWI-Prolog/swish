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

:- module(config_auth_google, []).
:- use_module(swish(lib/oauth2)).
:- use_module(swish(lib/plugin/login)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(debug)).

/** <module> Enable login with Google

This module allows for configures _login   with  Google_. To enable this
module:

  1. Follow these
  [steps](https://developers.google.com/accounts/docs/OpenIDConnect) to
  create a Google project and get

     - A client ID
     - A client secret
     - Register a redirect url.  To test from localhost, this should be
       `http://localhost:3050/oauth2/google/reply`

  2. COPY this file to =config-enabled=

  3. EDIT the following server attributes (near the end of this file)
     - redirect_uri: the location of your swish server.
     - client_id: the client id you obtained from Google.
     - client_secret: the client secret you obtained from Google.
*/

:- multifile
    oauth2:login/3,
    oauth2:server_attribute/3,
    swish_config:login_item/2,          % -Server, -HTML_DOM
    swish_config:login/2,               % +Server, +Request
    swish_config:user_info/2.           % +Request, ?Server, -Info

:- http_set_session_options([create(noauto)]).

:- http_handler(swish(logout), google_logout, []).

swish_config:login_item(google, 10-Item) :-
    http_absolute_location(icons('social_google_box.png'), Img, []),
    Item = img([ src(Img),
                 class('login-with'),
                 'data-server'(google),
                 'data-frame'(popup),
                 title('Login with Google')
               ]).

swish_config:login(google, Request) :-
    oauth2_login(Request, [server(google)]).

oauth2:login(_Request, google, TokenInfo) :-
    token_info_to_user_info(TokenInfo, UserInfo),
    debug(oauth, 'UserInfo: ~p', [UserInfo]),
    http_open_session(_SessionID, []),
    http_session_assert(oauth2(google, TokenInfo)),
    reply_logged_in([ identity_provider('Google'),
                      name(UserInfo.name),
                      user_info(UserInfo)
                    ]).

%!  google_logout(+Request)
%
%   Logout by removing the session data

google_logout(_Request) :-
    catch(http_session_retractall(oauth2(_,_)), _, true),
    reply_logged_out([]).

%!  swish_config:user_info(+Request, ?Server, -Info:dict) is semidet.
%
%   True if Info represents describes the currently logged in user.

swish_config:user_info(_Request, google, UserInfo) :-
    http_in_session(_SessionID),
    http_session_data(oauth2(google, TokenInfo)),
    token_info_to_user_info(TokenInfo, UserInfo).

token_info_to_user_info(TokenInfo, UserInfo) :-
    oauth2_claim(TokenInfo, Claim),
    map_user_info(Claim, Claim1),
    http_link_to_id(google_logout, [], LogoutURL),
    UserInfo = Claim1.put(_{ auth_method:oauth2,
                             logout_url:LogoutURL,
                             identity_provider:google
                           }).

%!  map_user_info(+OAuthInfo, -UserInfo) is det.
%
%   u{user:User, group:Group, name:Name, email:Email}

map_user_info(Dict, Dict) :-
    debug(oauth, 'Got: ~p', [Dict]).

%!  oauth2:server_attribute(?ServerID, ?Attribute, ?Value)
%
%   Declare properties of an oauth2 identity  provider. The values below
%   are for a [Unity](http://www.unity-idm.eu/) server.
%
%   @see swish(lib/oauth2) for a description of the attributes.

% from https://accounts.google.com/.well-known/openid-configuration

oauth2:server_attribute(google, url,
                        'https://accounts.google.com').
oauth2:server_attribute(google, redirect_uri,
                        'http://localhost:3050/oauth2/google/reply').
oauth2:server_attribute(google, client_id,
                        '****').
oauth2:server_attribute(google, client_secret,
                        '****').
oauth2:server_attribute(google, scope,
                        profile).

