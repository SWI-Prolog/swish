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

:- module(swish_login,
          [ login_button//1,            % +Options
            login_continue_button//0,
            reply_logged_in/1,          % +Options
            reply_logged_in_page/1      % +Options
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(pairs)).

:- use_module(config, []).
:- use_module(bootstrap).

:- multifile
    swish_config:profile_field/3,       % +Field, -Order, -Type
    swish_config:reply_logged_in/1,     % +Options
    swish_config:user_info/3,           % +Request, -Server, -Info
    swish_config:user_profile/2.        % +Request, -Info

/** <module> SWISH login support

This module provides the generic  code   to  deal  with _optional_ login
using multiple protocols. _Optional_ means that   SWISH may be used both
anonymously and after login.

This module cooperates with web/js/login.js. Login providers are defined
using configuration hooks. The various login options are accompagnied by
configuration files in =config-available=.
*/

:- http_handler(swish(login),        swish_login,  [id(login)]).
:- http_handler(swish(user_info),    user_info,    [id(user_info)]).
:- http_handler(swish(user_profile), user_profile, [id(user_profile),
                                                    priority(-10)]).


		 /*******************************
		 *          UI ELEMENTS		*
		 *******************************/

%!  login_button(+Options)//
%
%   Add a login/logout button. This button is added if there is at least
%   one option for optional login.

login_button(_Options) -->
    { findall(Item, login_item(Item), Items0),
      Items0 \== [],
      sort(Items0, Items),
      http_link_to_id(login, [], Login)
    },
    !,
    html(a([ href(Login), id(login), class(login) ],
           [ span(class(login),
                  \login_items(Items)),
             span([ class(logout)
                  ],
                  [ span(class(value), 'Logout')
                  ])
           ])).
login_button(_Options) -->
    [].

login_item(item(Tag, Server, Item)) :-
    swish_config:login_item(Server, Item0),
    (   Item0 = Tag-Item
    ->  true
    ;   Item = Item0,
        Tag = 0
    ).

%!  login_items(+Items)
%
%   Show the login options. If there is only   one, we just show a login
%   field.

login_items([item(_Tag, Server, _Item)]) -->
    !,
    html(span('data-server'(Server),
              [ span(class([glyphicon, 'glyphicon-log-in']), []),
                span(class(value), 'Login')
              ])).
login_items(Items) -->
    { maplist(arg(3), Items, HTML) },
    html([ span(class(value), HTML)
         ]).


%!  reply_logged_in(+Options) is det.
%!  reply_logged_in_page(+Options) is det.
%
%   Reply with an HTML  document  that   the  login  succeeded.  This is
%   normally called from the protocol-specific login handler to indicate
%   that the login succeeded.  Options:
%
%     - identity_provider(+Provider)
%     Indicate the identity provider that did the login.  Provider is
%     a term for html//1.
%     - user(+User)
%     User id of the identified user.
%     - name(+Name)
%     Common name of the identified user.
%     - user_info(+Dict)
%     Information provided by the identity provider.
%
%   At least one of user(User) or name(Name) must be present.
%
%   The     predicate     reply_logged_in/1     calls     the     _hook_
%   swish_config:reply_logged_in/1.   This   hook   is    provided   for
%   interacting with a user profile manager.

reply_logged_in(Options) :-
    swish_config:reply_logged_in(Options),
    !.
reply_logged_in(Options) :-
    reply_logged_in_page(Options).

reply_logged_in_page(Options) :-
    reply_html_page(
        title('Logged in'),
        [ h4('Welcome'),
          p([ 'You have been identified ',
              \identity_provider(Options),
              ' as ',
              \user(Options)
            ]),
          \login_continue_button
        ]).

identity_provider(Options) -->
    { option(identity_provider(Provider), Options) },
    !,
    html(['by ', Provider]).
identity_provider(_) --> [].

user(Options) -->
    { option(user(User), Options) },
    !,
    html(User),
    (   { option(name(Name), Options) }
    ->  html([' (', Name, ')' ])
    ;   []
    ).
user(Options) -->
    { option(name(Name), Options) },
    !,
    html(Name).
user(_) -->
    html(unknown).

%!  login_continue_button//
%
%   The login page is opened either  inside   an  iframe  inside a SWISH
%   modal dialog or inside a browser popup   window. This scripts adds a
%   button to dismiss the browser popup window.

login_continue_button -->
    html(style(\[ 'div.login-continue { text-align: center; margin-top: 2em; }'
                ])),

    js_script({|javascript||
function inIframe() {
  try {
    return window.self !== window.top;
  } catch (e) {
    return true;
  }
}

function append( elString, parent ) {
  var div = document.createElement( "div" );
  div.innerHTML = elString;
  document.querySelector( parent || "body" ).appendChild( div.firstChild );
}

if ( !inIframe() ) {
  append('<div class="login-continue">\n'+
         '  <button onclick="window.close()">\n'+
         '    Continue\n'+
         '  </button>\n'+
         '</div>');
}
              |}).



		 /*******************************
		 *          HTTP HANDLERS	*
		 *******************************/

%!  swish_login(+Request)
%
%   HTTP handler that deals with  login.   This  handler  is called from
%   web/js/login.js which adds  the  selected   login  server  from  the
%   =data-server= attribute.

swish_login(Request) :-
    http_parameters(Request,
                    [ server(Server, [default(default)])
                    ]),
    swish_config:login(Server, Request).

%!  user_info(+Request)
%
%   HTTP handler to obtain information on  the currently logged in user.
%   This handler tries the clauses  dealing   with  login for a specific
%   protocol.

user_info(Request) :-
    swish_config:user_info(Request, _Server, UserInfo),
    (   swish_config:user_profile(Request, Profile)
    ->  copy_fields([identity_provider, auth_method, logout_url],
                    UserInfo, Profile, Info)
    ;   Info = UserInfo
    ),
    !,
    reply_json_dict(Info).
user_info(_Request) :-
    reply_json_dict(null).

copy_fields([], _From, Dict, Dict).
copy_fields([H|T], From, Dict0, Dict) :-
    (   V = From.get(H)
    ->  copy_fields(T, From, Dict0.put(H,V), Dict)
    ;   copy_fields(T, From, Dict0, Dict)
    ).


%!  user_profile(+Request)
%
%   HTTP handler to display basic information  about the logged in user.
%   The handler must reply  with  an   HTML  document  that provides the
%   content for a modal window.

user_profile(Request) :-
    swish_config:user_info(Request, _ServerID, Info),
    !,
    dict_pairs(Info, _, Pairs0),
    select_fields(Pairs0, Pairs),
    reply_html_page(title('User profile'),
                    [ table(class([table, 'table-striped']),
                            \profile_rows(Pairs, Info)),
                      div(class(['btn-group', 'btn-group-justified']),
                          \ok_button)
                    ]).
user_profile(_Request) :-
    reply_html_page(title('User profile'),
                    [ p('Not logged in')
                    ]).


profile_rows([], _) --> [].
profile_rows([H|T], Info) --> profile_row(H, Info), profile_rows(T, Info).

profile_row(Name-Value, Info) -->
    { name_label(Name, Label),
      get_profile_field(Name, _Tag, Type)
    },
    !,
    html(tr([ th(class('text-nowrap'), Label),
              td(\profile_value(Type, Value, Info))
            ])).
profile_row(Name-Value, _Info) -->
    html(tr(class(warning),
            [ th(class('text-nowrap'), Name),
              td(\profile_value(string, Value, _{}))
            ])).


%!  profile_value(+Type, +Value, +Profile)//
%
%   Render a profile value  Value  of  type   Type.  Profile  is  a dict
%   providing the entire profile.

profile_value(url(img), URL, _) -->
    !,
    html(img([src(URL), class('profile-picture')])).
profile_value(url, URL, _) -->
    !,
    html(a(href(URL), URL)).
profile_value(email, Address, _) -->
    !,
    html(a(href('mailto:'+Address), Address)).
profile_value(time_stamp(Format), Stamp, _) -->
    !,
    { format_time(string(S), Format, Stamp) },
    html(S).
profile_value(identity_provider, ServerID, Info) -->
    { Info.get(auth_method) == oauth2,
      oauth2:server_attribute(ServerID, url, URL)
    },
    !,
    html(a(href(URL), ServerID)).
profile_value(_, Value, _) -->
    html('~w'-[Value]).

ok_button -->
    html(div(class('btn-group'),
             \bt_button(done, button,
                        [ type(primary),
                          data([dismiss(modal)])
                        ], []))).

%!  select_fields(+PairsIn, -Pairs) is det.
%
%   Select and order fields we display for the user info.

select_fields(PairsIn, Pairs) :-
    tag_fields(PairsIn, Tagged),
    keysort(Tagged, Ordered),
    pairs_values(Ordered, Pairs).

tag_fields([], []).
tag_fields([Name-Value|T0], [Tag-(Name-Value)|T]) :-
    (   get_profile_field(Name, Tag, _Type)
    ->  true
    ;   debugging(profile),
        Tag = 1000
    ),
    !,
    tag_fields(T0, T).
tag_fields([_|T0], T) :-
    tag_fields(T0, T).

%!  get_profile_field(+Name, -Tag, -Type) is semidet.
%
%   Get properties for a field for the user profile.

get_profile_field(Name, Tag, Type) :-
    swish_config:profile_field(Name, Tag, Type),
    !.
get_profile_field(Name, Tag, Type) :-
    def_profile_field(Name, Tag, Type),
    !.

%!  swish_config:profile_field(+Name, -Tag, -Type)
%
%   Determine which of the  fields   from  swish_config:user_info/3  are
%   displayed in which order and using which type of rendering.  Type is
%   one of
%
%     - url(img)
%     Value is an image URL
%     - url
%     Value is a URL
%     - email
%     Value is an email address
%     - time_stamp(Format)
%     Value is a time stamp (seconds since Jan 1, 1970) and it is
%     formatted according to Format, which is passed to format_time/3.
%     - identity_provider
%     Render a link to federated identity providers.

def_profile_field(picture,           100, url(img)).
def_profile_field(name,              200, string).
def_profile_field(given_name,        230, string).
def_profile_field(family_name,       260, string).
def_profile_field(reputation,	     350, int).
def_profile_field(user_type,	     370, string).
def_profile_field(email,             400, email).
def_profile_field(profile_url,       450, url).
def_profile_field(website_url,       480, url).
def_profile_field(creation_date,     490, time_stamp('%D')).
def_profile_field(locale,            500, string).
def_profile_field(identity_provider, 600, identity_provider).

