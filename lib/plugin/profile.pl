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

:- module(swish_plugin_user_profile,
          [
          ]).
:- use_module(library(option)).
:- use_module(library(user_profile)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).
:- use_module(library(pairs)).

:- use_module('../config', []).
:- use_module(login).
:- use_module('../authenticate').
:- use_module('../bootstrap').
:- use_module('../form').
:- use_module('../avatar').


/** <module> User profile configuration

Complementary to authentication, this module  configures the maintenance
of user profiles.

There are several  places  where  we   need  interaction  with  the user
profile:

  - Prolog gathering and maintenance

    1. If a new user is found we want to welcome the user and
       optionally complete the profile.  For example, we may wish
       to ask the `email` for the new user and start a process to
       verify this.
    2. A user must be able to edit and delete his/her profile.
    3. A user must be able to migrate a profile, probably only from
       a profile with the same verified email address.

  - Profile usage

    1. Claim ownership
       - To files
       - To comments
    2. Grant access.  Access points in SWISH should be
       - Execution of goals
	 - Normal sandboxed/not-sandboxed operations
         - Grant/Deny access to certain sensitive (database)
           predicates.
       - Viewing/using code
       - Saving code
         - Save in general (e.g., do not save when anonymous)
         - Make revisions to files that are not yours
         - Save non-versioned files
         - Add modules to the version store?
    3. Send notifications
       - By mail
       - Maintain notification queue for a user
*/

:- http_handler(swish(user_profile),   user_profile,   [id(user_profile)]).
:- http_handler(swish(save_profile),   save_profile,   []).
:- http_handler(swish(update_profile), update_profile,   []).
:- http_handler(swish(delete_profile), delete_profile, []).


:- multifile
    swish_config:user_info/3,
    swish_config:reply_logged_in/1,     % +Options
    swish_config:reply_logged_out/1,    % +Options
    swish_config:user_profile/2,        % +Request, -Info
    user_profile:attribute/3,
    user_profile:attribute_mapping/3.


		 /*******************************
		 *            LOGIN		*
		 *******************************/

%!  swish_config:reply_logged_in(+Options)
%
%   Hook logins from federated identity provides.  Options processed:
%
%     - user_info(+UserInfo:Dict)
%     Provides information about the user provided by the external
%     identity provider.
%     - reply(+Format)
%     If Format = `html`, reply with an HTML page.  Other values
%     are left for future extensions.
%     - profile_id(-Id)
%     Unify Id with the found or created profile id.

swish_config:reply_logged_in(Options) :-
    option(user_info(Info), Options),
    known_profile(Info, ProfileID),
    !,
    option(profile_id(ProfileID), Options, _),
    associate_profile(ProfileID),
    (   option(reply(html), Options, html)
    ->  reply_html_page(
            title('Logged in'),
            [ h4('Welcome back'),
              p(\last_login(ProfileID)),
              \login_continue_button
            ])
    ;   true
    ).
swish_config:reply_logged_in(Options) :-
    option(user_info(Info), Options),
    create_profile(Info, Info.get(identity_provider), ProfileID),
    !,
    option(profile_id(ProfileID), Options, _),
    http_open_session(_SessionID, []),
    associate_profile(ProfileID),
    update_last_login(ProfileID),
    (   option(reply(html), Options, html)
    ->  reply_html_page(
            title('Logged in'),
            [ h4('Welcome'),
              p([ 'You appear to be a new user.  You may inspect, update \c
                  and delete your profile using the drop-down menu associated \c
                  with the login/logout widget.'
                ]),
              \login_continue_button
            ])
    ;   true
    ).

%!  known_profile(+Info, -ProfileID) is semidet.
%
%   True when ProfileID is the profile  identifier for the authenticated
%   user.

known_profile(Info, ProfileID) :-
    IdProvider = Info.get(identity_provider),
    profile_default(IdProvider, Info, external_identity(ID)),
    profile_property(ProfileID, external_identity(ID)),
    profile_property(ProfileID, identity_provider(IdProvider)),
    !.


%!  associate_profile(+ProfileID) is det.
%
%   Associate the current session with   the given ProfileID. Broadcasts
%   SWISH event profile(ProfileID).

associate_profile(ProfileID) :-
    http_session_assert(profile_id(ProfileID)),
    broadcast(swish(profile(ProfileID))).


%!  init_session_profile
%
%   This deals with the case where  a   session  is opened, but login is
%   continued because it is based on HTTP authentication.  If the server
%   opens a session, we check for the current identity and associate the
%   related profile.

:- listen(http_session(begin(_SessionID, _Peer)),
          init_session_profile).

init_session_profile :-
    http_current_request(Request),
    authenticate(Request, Identity),
    known_profile(Request, Identity, ProfileID),
    associate_profile(ProfileID).

known_profile(_Request, Identity, ProfileID) :-
    known_profile(Identity, ProfileID),
    !.
known_profile(Request, Identity, ProfileID) :-
    local == Identity.get(identity_provider),
    swish_config:user_info(Request, local, UserInfo),
    create_profile(UserInfo, local, ProfileID).


%!  swish_config:reply_logged_out(+Options)
%
%   Perform a logout, removing the link to the session

swish_config:reply_logged_out(Options) :-
    http_in_session(_),
    !,
    forall(http_session_retract(profile_id(ProfileID)),
           broadcast(swish(logout(ProfileID)))),
    reply_logged_out_page(Options).
swish_config:reply_logged_out(_) :-
    broadcast(swish(logout(-))).        % ?

:- listen(swish(logout(http)), cancel_session_profile).

cancel_session_profile :-
    (   http_in_session(_)
    ->  forall(http_session_retract(profile_id(ProfileID)),
               broadcast(swish(logout(ProfileID))))
    ;   true
    ).

%!  create_profile(+UserInfo, +IDProvider, -ProfileID)
%
%   Create a new user profile.

create_profile(UserInfo, IdProvider, ProfileID) :-
    user_profile_values(UserInfo, IdProvider, Defaults),
    profile_create(ProfileID, Defaults).

user_profile_values(UserInfo, IdProvider, Defaults) :-
    findall(Default,
            profile_default(IdProvider, UserInfo, Default),
            Defaults0),
    add_gravatar(Defaults0, Defaults).

profile_default(IdProvider, UserInfo, Default) :-
    (   nonvar(Default)
    ->  functor(Default, Name, 1)
    ;   true
    ),
    user_profile:attribute(Name, _, _),
    user_profile:attribute_mapping(Name, IdProvider, UName),
    catch(profile_canonical_value(Name, UserInfo.get(UName), Value),
          error(type_error(_,_),_),
          fail),
    Default =.. [Name,Value].
profile_default(local, UserInfo, email_verified(true)) :-
    _ = UserInfo.get(email).                    % trust our own user data

add_gravatar(Defaults0, Defaults) :-
    \+ memberchk(avatar(_), Defaults0),
    memberchk(email(Email), Defaults0),
    email_gravatar(Email, Avatar0),
    valid_gravatar(Avatar0),
    catch(profile_canonical_value(avatar, Avatar0, Avatar),
          error(type_error(_,_),_),
          fail),
    !,
    Defaults = [avatar(Avatar)|Defaults0].
add_gravatar(Defaults, Defaults).


%!  last_login(+User)//
%
%   Indicate when the user used this server for the last time.

last_login(User) -->
    { profile_property(User, last_login(TimeStamp)),
      profile_property(User, last_peer(Peer)),
      format_time(string(Time), '%+', TimeStamp),
      update_last_login(User)
    },
    !,
    html('Last login: ~w from ~w'-[Time, Peer]).
last_login(User) -->
    { update_last_login(User) }.

update_last_login(User) :-
    http_current_request(Request),
    http_peer(Request, Peer),
    get_time(Now),
    NowInt is round(Now),
    set_profile(User, last_peer(Peer)),
    set_profile(User, last_login(NowInt)).

%!  swish_config:user_profile(+Request, -Profile) is semidet.
%
%   Provide the profile for the current  user. The Profile dict contains
%   the profile keys and the `profile_id` key.

swish_config:user_profile(_Request, Profile) :-
    http_in_session(_SessionID),
    http_session_data(profile_id(User)),
    current_profile(User, Profile0),
    Profile = Profile0.put(profile_id, User).


		 /*******************************
		 *         PROFILE GUI		*
		 *******************************/

%!  user_profile(+Request)
%
%   Emit an HTML page that allows for   viewing, updating and deleting a
%   user profile.

user_profile(_Request) :-
    http_in_session(_SessionID),
    http_session_data(profile_id(User)), !,
    current_profile(User, Profile),
    findall(Field, user_profile:attribute(Field, _, _), Fields),
    convlist(bt_field(Profile), Fields, FieldWidgets),
    buttons(Buttons),
    append(FieldWidgets, Buttons, Widgets),
    reply_html_page(
        title('User profile'),
        \bt_form(Widgets,
                 [ class('form-horizontal'),
                   label_columns(sm-3)
                 ])).
user_profile(_Request) :-
    reply_html_page(
        title('User profile'),
        [ p('You must be logged in to view your profile'),
          \bt_form([ button_group(
                         [ button(cancel, button,
                                  [ type(danger),
                                    data([dismiss(modal)])
                                  ])
                         ], [])
                   ],
                   [ class('form-horizontal'),
                     label_columns(sm-3)
                   ])
        ]).


bt_field(Profile, Name, Field) :-
    user_profile:attribute(Name, Type, AOptions),
    !,
    \+ option(hidden(true), AOptions),
    bt_field(Profile, Name, Type, AOptions, Field).

bt_field(Profile, Name, Type, AOptions, select(Name, Values, Options)) :-
    Type = oneof(Values),
    !,
    phrase(( (value_opt(Profile, Type, Name) -> [] ; []),
             (access_opt(AOptions)           -> [] ; [])
           ), Options).
bt_field(Profile, Name, Type, AOptions, input(Name, IType, Options)) :-
    input_type(Type, IType),
    phrase(( (value_opt(Profile, Type, Name) -> [] ; []),
             (access_opt(AOptions)           -> [] ; []),
             (data_type_opt(Type)            -> [] ; [])
           ), Options).

input_type(boolean, checkbox) :-
    !.
input_type(_,       text).

value_opt(Profile, Type, Name) -->
    { Value0 = Profile.get(Name),
      display_value(Type, Value0, Value)
    },
    [ value(Value) ].
access_opt(AOptions) -->
    { option(access(ro), AOptions) },
    [ disabled(true) ].
data_type_opt(_Type) -->                % TBD
    [].

display_value(time_stamp(Format), Stamp, Value) :-
    !,
    format_time(string(Value), Format, Stamp).
display_value(_, Value0, Value) :-
    atomic(Value0),
    !,
    Value = Value0.
display_value(_, Value0, Value) :-
    format(string(Value), '~w', [Value0]).

buttons(
    [ button_group(
          [ button(done, button,
                   [ type(primary),
                     data([dismiss(modal)])
                   ]),
            button(save, submit,
                   [ type(success),
                     label('Save profile'),
                     data([action(SaveHREF)])
                   ]),
            button(reset, submit,
                   [ type(warning),
                     label('Reset profile'),
                     data([action(UpdateHREF), form_data(false)])
                   ]),
            button(delete, submit,
                   [ type(danger),
                     label('Delete profile'),
                     data([action(DeleteHREF), form_data(false)])
                   ])
          ],
          [
          ])
    ]) :-
    http_link_to_id(save_profile, [], SaveHREF),
    http_link_to_id(update_profile, [], UpdateHREF),
    http_link_to_id(delete_profile, [], DeleteHREF).


		 /*******************************
		 *        MODIFY PROFILE	*
		 *******************************/

%!  save_profile(+Request)
%
%   Update the profile for the  current  user.   The  form  sends a JSON
%   object that contains a value for all non-disabled fields that have a
%   non-null value.

save_profile(Request) :-
    http_read_json_dict(Request, Dict),
    debug(profile(update), 'Got ~p', [Dict]),
    http_in_session(_SessionID),
    http_session_data(profile_id(User)),
    dict_pairs(Dict, _, Pairs),
    maplist(validate_term, Pairs, VPairs, Validate),
    catch(validate_form(Dict, Validate), E, true),
    (   var(E)
    ->  dict_pairs(VDict, _, VPairs),
        save_profile(User, VDict),
        current_profile(User, Profile),
        reply_json_dict(_{status:success, profile:Profile})
    ;   message_to_string(E, Msg),
        Error = _{code:form_error, data:Msg},
        reply_json_dict(_{status:error, error:Error})
    ).

validate_term(Name-_, Name-Value,
              field(Name, Value, [strip,default("")|Options])) :-
    user_profile:attribute(Name, Type, FieldOptions),
    (   (   option(access(ro), FieldOptions)
        ;   option(hidden(true), FieldOptions)
        )
    ->  permission_error(modify, profile, Name)
    ;   true
    ),
    type_options(Type, Options).

type_options(Type, [Type]).

%!  save_profile(+User, +Dict) is det.
%
%   Update the profile for User with values from Dict.

save_profile(User, Dict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(save_profile_field(User), Pairs).

save_profile_field(User, Name-Value) :-
    (   Term =.. [Name,Old],
        profile_property(User, Term)
    ->  true
    ;   Old = ""
    ),
    update_profile_field(User, Name, Old, Value).

update_profile_field(User, Name, Old, "") :-
    !,
    profile_remove(User, Name),
    broadcast(user_profile(modified(User, Name, Old, ""))).
update_profile_field(User, Name, Old, New0) :-
    profile_canonical_value(Name, New0, New),
    (   Old == New
    ->  true
    ;   set_profile(User, Name=New),
        broadcast(user_profile(modified(User, Name, Old, New)))
    ).


%!  update_profile(+Request)
%
%   Update a profile with new information from the identity provider

update_profile(Request) :-
    swish_config:user_info(Request, Server, UserInfo),
    http_in_session(_SessionID),
    http_session_data(profile_id(User)),
    user_profile_values(UserInfo, Server, ServerInfo),
    dict_pairs(ServerInfo, _, Pairs),
    maplist(update_profile_field(User), Pairs),
    current_profile(User, Profile),
    reply_json_dict(_{status:success, profile:Profile}).

update_profile_field(User, Name-Value) :-
    set_profile(User, Name=Value).

%!  delete_profile(+Request)
%
%   Completely delete the profile for the current user

delete_profile(_Request) :-
    http_in_session(SessionID),
    http_session_data(profile_id(User)),
    http_close_session(SessionID),      % effectively logout
    profile_remove(User),
    reply_json_dict(true).


		 /*******************************
		 *           PROPERTIES		*
		 *******************************/

:- listen(identity_property(Identity, Property),
          from_profile(Identity, Property)).

from_profile(Identity, Property) :-
    profile_property(Identity.get(profile_id), Property).

%!  profile_name(+ProfileID, -Name) is semidet.
%
%   Name is the public name associated with Profile.

profile_name(ProfileID, Name) :-
    user_field(Field),
    Term =.. [Field, Name],
    profile_property(ProfileID, Term),
    !.

user_field(name).
user_field(given_name).
user_field(nick_name).
user_field(family_name).


		 /*******************************
		 *           TYPE AHEAD		*
		 *******************************/

:- multifile
	swish_search:typeahead/4.	% +Set, +Query, -Match, +Options

%!  swish_search:typeahead(+Set, +Query, -Match, +Options) is nondet.
%
%   Find users based on their  profile.   This  handler  defines the set
%   `user`. A Match is a dict holding:
%
%     - id:ProfileID
%     - label:Name
%     A reasonable name for the user
%     - email:Email
%     Only present if the match was found on the email.
%     - hit:hit{key:Key,value:Value}
%     Field key and value on which the hit was found
%     - avatar:Avatar
%     Avatar URL

swish_search:typeahead(user, Query, User, _Options) :-
    current_profile(ProfileID, Attributes),
    Keys = [name,given_name,family_name,email],
    pairs_keys_values(Pairs, Keys, _),
    dict_pairs(Profile, _, Pairs),
    Profile >:< Attributes,
    profile_match_query(Query, Pairs, Key),
    user_dict(ProfileID, Key, Attributes, User).

profile_match_query(Query, Pairs, Key) :-
    member(Key-Value, Pairs),
    text(Value),
    sub_atom_icasechk(Value, 0, Query),
    !.

text(Value) :-
    string(Value),
    !.
text(Value) :-
    atom(Value).

user_dict(ProfileID, SearchKey, Attributes, Dict) :-
    findall(Key-Value,
            user_search_property(ProfileID,SearchKey,Attributes,Key,Value),
            Pairs),
    dict_pairs(Dict, user, Pairs).

user_search_property(ProfileID, _, _, id,    ProfileID).
user_search_property(ProfileID, _, _, name,  Name) :-
    profile_name(ProfileID, Name).
user_search_property(_, email,  Attrs, email,  Attrs.get(email)).
user_search_property(_, Search, Attrs, hit,    hit{key:Search,
                                                   value:Attrs.get(Search)}).
user_search_property(_, _,      Attrs, avatar, Attrs.get(avatar)).

