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

:- module(config_user_profile, []).
:- use_module(library(settings)).
:- use_module(library(broadcast)).
:- use_module(library(user_profile)).
:- use_module(library(profile/backend/profile_prolog), []).

:- use_module(swish(lib/plugin/profile)).

:- set_setting(user_profile:backend, impl_profile_prolog).
:- initialization profile_open_db([]).

/** <module> User profile configuration

Configure  maintenance  of  user  profiles.  This  config  file  may  be
optionally enabled if one  or   more,  notably federated, authentication
modules are loaded. It maintains a database of identified users.

The user profile infra structure depends on the pack _profile_, which is
linked to SWISH as a git submodule.  To use profiles, run

    ```
    git submodule update --init pack/profile
    ```
*/

:- multifile
    user_profile:attribute/3,
    user_profile:attribute_mapping/3.


%!  user_profile:attribute(?Name, ?Type, ?Options) is nondet.
%
%   Declare profile properties.

user_profile:attribute(name,                string,           []).
user_profile:attribute(given_name,          string,           []).
user_profile:attribute(family_name,         string,           []).
user_profile:attribute(nick_name,           string,           []).
user_profile:attribute(email,               email,            []).
user_profile:attribute(email_verified,      boolean,          [access(ro)]).
user_profile:attribute(email_notifications, oneof([immediate,daily,never]),
                                                           [default(immediate)]).
user_profile:attribute(avatar,              url(http),        []).
user_profile:attribute(home_page,           url(http),        []).
user_profile:attribute(last_login,          time_stamp('%+'), [access(ro)]).
user_profile:attribute(last_peer,           string,           [access(ro)]).
user_profile:attribute(identity_provider,   atom,             [access(ro)]).
user_profile:attribute(external_identity,   string,           [hidden(true)]).

%!  user_profile:attribute_mapping(+ProfileAttr, +IDProvider, -IDProviderAttr)
%
%   Provide a mapping from profile attributed (e.g., oauth2 _scopes_) to
%   our profile attributes. This is  used   to  fill the initial profile
%   after a user was identified by IDProvider.

user_profile:attribute_mapping(external_identity, _,     sub).
user_profile:attribute_mapping(avatar,            _,     picture).
user_profile:attribute_mapping(nick_name,         local, user).
user_profile:attribute_mapping(external_identity, local, user).
user_profile:attribute_mapping(Attr,              _,     Attr).

		 /*******************************
		 *         DEPENDENCIES		*
		 *******************************/

:- listen(user_profile(modified(User, Name, _Old, New)),
          propagate_profile_change(User, Name, New)).

propagate_profile_change(User, email, _) :-
    !,
    set_profile(User, email_verified=false).
propagate_profile_change(_, _, _).
