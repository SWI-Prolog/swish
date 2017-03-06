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
          [ authenticate/2                      % +Request, -Authentity
          ]).
:- use_module(library(http/http_wrapper)).

:- use_module(config).

/** <module> Authentication access for SWISH

This module (depending on the loaded  configuration) identifies the user
based on the HTTP request.

@see pep.pl for _authorization_ issues.
*/

%!  authenticate(+Request, -Authentity:dict) is det.
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
    http_auth(Request, Auth0),
    profile_auth(Request, Auth1),
    Auth = Auth0.put(Auth1).put(peer, Peer).

:- multifile
    swish_config:authenticate/2,
    swish_config:user_profile/2.

http_auth(Request, Auth) :-
    swish_config:authenticate(Request, User),   % throws http_reply(_)
    !,
    Auth = auth{user:User}.
http_auth(_Request, auth{}).

profile_auth(Request, Auth) :-
    swish_config:user_profile(Request, Profile),
    Auth = auth{identity_provider: _,
                external_identity: _,
                profile_id:_},
    Auth :< Profile,
    !.
profile_auth(_, auth{}).


