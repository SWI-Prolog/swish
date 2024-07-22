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

:- module(swish_authenticate,
          [ authenticate/2,                     % +Request, -Authentity
            user_property/2                     % +Authentity, ?Property
          ]).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).
:- use_module(library(http/http_session)).
:- use_module(config).

/** <module> Authentication access for SWISH

This module (depending on the loaded  configuration) identifies the user
based on the HTTP request.

@see pep.pl for _authorization_ issues.
*/

%!  authenticate(+Request, -Identity:dict) is det.
%
%   Establish the identity behind  the  HTTP   Request.  There  are  two
%   scenarios.
%
%     - The entire server is protected using HTTP authentication.  In
%       this case this predicate may throw an HTTP challenge or a
%       forbidden exception.
%     - The server allows for mixed anonymous and logged in usage. Login
%       may use HTTP or federated login (oauth2).
%
%   @throws http_reply(_) HTTP authentication and permission exceptions
%   if config-available/auth_http_always.pl is enabled.

authenticate(Request, Auth) :-
    http_peer(Request, Peer),
    http_auth(Request, HTTPAuth),
    profile_auth(Request, ProfileAuth),
    Auth2 = HTTPAuth.put(ProfileAuth).put(peer, Peer),
    % 세션에서 사용자 ID를 가져옴
    (   catch(http_session_data(user(UserID)), _, fail)
        ->(
            catch(http_session_data(user_role(Role)), _, fail)
            ->  Auth3 = Auth2.put(user_id, UserID).put(id, UserID).put(user_role, Role)
            ;   reply_json_dict(_{success: false, message: "Failed to retrieve admin information from session"}),
                fail
        )
    ;   Auth3 = Auth2.put(user_id, guest).put(id, guest)
    ),
    identity(Request, Auth3, Auth),
    debug(authenticate, 'Identity: ~p', [Auth]).

:- multifile
    swish_config:user_info/3,
    swish_config:authenticate/2,
    swish_config:user_profile/2.

http_auth(Request, Auth) :-
    (   swish_config:authenticate(Request, User)   % throws http_reply(_)
    ->  true
    ;   swish_config:user_info(Request, local, UserInfo),
        User = UserInfo.get(user)
    ),
    !,
    Auth = auth{user:User, identity_provider:local, external_identity:User}.
http_auth(_Request, auth{}).

profile_auth(Request, Auth) :-
    swish_config:user_profile(Request, Profile),
    Pattern = _{ identity_provider: _,
                 external_identity: _,
                 profile_id:_},
    Pattern :< Profile,
    Auth = auth{}.put(Pattern).
profile_auth(_, auth{}).

identity(Request, Auth0, Auth) :-
    _{identity_provider:Provider, external_identity:ExtID} :< Auth0,
    !,
    (   swish_config:user_info(Request, Provider, UserInfo),
        is_dict(UserInfo, Tag),
        (   var(Tag)
        ->  Tag = user_info
        ;   true
        )
    ->  true
    ;   UserInfo = user_info{}
    ),
    atomic_list_concat([Provider,ExtID], :, Identity),
    Auth = Auth0.put(_{identity:Identity, user_info:UserInfo}).
identity(_, Auth, Auth).

%!  user_property(+Identity, ?Property) is nondet.
%
%   True when Identity has Property. Defined properties are:
%
%     - peer(Atom)
%     Remote IP address
%     - identity(Atom)
%     Identity as provided by some identity provider
%     - identity_provider(Atom)
%     Subsystem that identified the user
%     - external_identity(Atom)
%     Identity as provided by the identity_provider
%     - profile_id(Atom)
%     Identifier of the profile we have on this user.
%     - login(Atom)
%     Same as identity_provider(Atom)
%     - name(Atom)
%     Name associated with the identity
%     - email(Atom)
%     Email associated with the identity

user_property(Identity, Property) :-
    current_user_property(Property, How),
    user_property_impl(Property, How, Identity).

user_property_impl(Property, dict, Identity) :- !,
    Property =.. [Name,Value],
    Value = Identity.get(Name).
user_property_impl(Property, broadcast, Identity) :-
    broadcast_request(identity_property(Identity, Property)).
user_property_impl(login(By), _, Identity) :-
    By = Identity.get(identity_provider).

current_user_property(peer(_Atom),                dict).
current_user_property(identity(_Atom),            dict).
current_user_property(external_identity(_String), dict).
current_user_property(identity_provider(_Atom),   dict).
current_user_property(profile_id(_Atom),          dict).
current_user_property(avatar(_String),            dict).

current_user_property(login(_IdProvider),         derived).
current_user_property(name(_Name),                broadcast).
current_user_property(email(_Email),              broadcast).

		 /*******************************
		 *        PENGINE HOOKS		*
		 *******************************/

%!  pengines:authentication_hook(+Request, +Application, -User)
%
%   Is called from the /pengine/create request   to establish the logged
%   in user.

:- multifile pengines:authentication_hook/3.

pengines:authentication_hook(Request, _Application, User) :-
    authenticate(Request, User).