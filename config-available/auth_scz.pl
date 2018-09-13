/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2018, CWI Amsterdam
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

:- module(config_auth_scz, []).
:- use_module(swish(lib/oauth2)).
:- use_module(swish(lib/plugin/login)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(debug)).

/** <module> Enable login with SCZ (EDUGain Science Collaborative Zones)

This module allows for configures _login with EDUGain_. To enable this
module:

  1. Get

     - A client ID
     - A client secret
     - Register a redirect url.  To test from localhost, this should be
       `https://localhost:3050/oauth2/scz/reply`  Note that the server
       must be accessible using HTTPS to be acceptable by SCZ.

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

:- http_handler(swish(logout), scz_logout, []).

swish_config:login_item(scz, 10-Item) :-
    http_absolute_location(icons('eduGAIN.png'), Img, []),
    Item = img([ src(Img),
                 class('login-with'),
                 'data-server'(scz),
                 'data-frame'(popup),
                 title('Login with eduGAIN')
               ]).

swish_config:login(scz, Request) :-
    oauth2_login(Request, [server(scz)]).

oauth2:login(_Request, scz, TokenInfo) :-
    oauth2_user_info(scz, TokenInfo, Claim),
    debug(oauth, 'Claim: ~p', [Claim]),
    map_user_info(Claim, UserInfo),
    debug(oauth, 'UserInfo: ~p', [UserInfo]),
    http_open_session(_SessionID, []),
    http_session_assert(oauth2(scz, TokenInfo)),
    http_session_assert(user_info(scz, UserInfo)),
    (   Name = UserInfo.get(name)
    ->  Extra = [name(Name)]
    ;   Extra = []
    ),
    reply_logged_in([ identity_provider('eduGAIN'),
                      user_info(UserInfo)
		    | Extra
                    ]).

%!  scz_logout(+Request)
%
%   Logout by removing the session data

scz_logout(_Request) :-
    catch(session_remove_user_data, _, true),
    reply_logged_out([]).

session_remove_user_data :-
    http_session_retractall(oauth2(_,_)),
    http_session_retractall(user_info(_,_)).

%!  swish_config:user_info(+Request, ?Server, -Info:dict) is semidet.
%
%   True if Info represents describes the currently logged in user.

swish_config:user_info(_Request, scz, UserInfo) :-
    http_in_session(_SessionID),
    http_session_data(user_info(scz, UserInfo)).

%!  map_user_info(+OAuthInfo, -UserInfo) is det.
%
%   u{user:User, group:Group, name:Name, email:Email}

map_user_info(Dict0, Dict) :-
    debug(oauth, 'Mapping ~p', [Dict0]),
    dict_pairs(Dict0, Tag, Pairs0),
    maplist(map_user_field, Pairs0, Pairs),
    http_link_to_id(scz_logout, [], LogoutURL),
    dict_pairs(Dict, Tag,
               [ auth_method-oauth2,
                 logout_url-LogoutURL,
                 identity_provider-scz
               | Pairs
               ]).

map_user_field(nickname-Name, name-Name) :- !.
map_user_field(Field, Field).

%!  oauth2:server_attribute(?ServerID, ?Attribute, ?Value)
%
%   Declare properties of an oauth2 identity  provider. The values below
%   are for a [Unity](http://www.unity-idm.eu/) server.
%
%   @see swish(lib/oauth2) for a description of the attributes.

% from https://proxy.pilot.scz.lab.surf.nl/.well-known/openid-configuration

oauth2:server_attribute(scz, url,
                        'https://proxy.pilot.scz.lab.surf.nl/SamlSP/OIDC/authorization').
oauth2:server_attribute(scz, redirect_uri,
                        'https://localhost:3050/oauth2/scz/reply').
oauth2:server_attribute(scz, client_id,
                        '***').
oauth2:server_attribute(scz, client_secret,
                        '***').
oauth2:server_attribute(scz, scope,
                        'openid email profile').
oauth2:server_attribute(scz, claims,
                        _{ userinfo:_{ edumember_is_member_of:null,
                                       eduperson_entitlement:null,
                                       given_name:null,
                                       eduperson_targeted_id:null,
                                       nickname:null,
                                       eduperson_affiliation:null,
                                       family_name:null,
                                       eduperson_principalname:null,
                                       eduperson_scoped_affiliation:null,
                                       schac_home_organisation:null,
                                       uid:null,
                                       email:null,
                                       name:null,
                                       address:_{ street_address:null
                                                }
                                     }
                         }).
