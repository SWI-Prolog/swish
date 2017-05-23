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
            reply_logged_in_page/1,     % +Options
            reply_logged_out/1,         % +Options
            reply_logged_out_page/1,    % +Options
            current_user_info/2         % +Request, -UserInfo
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(broadcast)).

:- use_module('../config', []).

:- multifile
    swish_config:login/2,
    swish_config:login_item/2,
    swish_config:li_login_button//1,    % +Options
    swish_config:reply_logged_in/1,     % +Options
    swish_config:reply_logged_out/1,    % +Options
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


		 /*******************************
		 *          UI ELEMENTS		*
		 *******************************/

%!  swish_config:li_login_button(+Options)//
%
%   Hook called from page.pl to include the login buttons.

swish_config:li_login_button(Options) -->
    html(li(\login_button(Options))).

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
login_button(_Options) -->              % config-available/auth_http_always.pl
    html(a([ id(login), class([login, 'no-logout']) ],
           [ span([ class(logout)
                  ],
                  [ span(class(value), [])
                  ])
           ])).

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

login_items([item(_Tag, Server, Item)]) -->
    !,
    { findall(Attr, login_attr(Item, Attr), Attrs)
    },
    html(span(['data-server'(Server)|Attrs],
              [ span(class([glyphicon, 'glyphicon-log-in']), []),
                span(class(value), 'Login')
              ])).
login_items(Items) -->
    { maplist(arg(3), Items, HTML) },
    html([ span(class(value), HTML)
         ]).

login_attr(Item, 'data-frame'(Frame)) :-
    sub_term('data-frame'(Frame), Item).




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



%!  reply_logged_out(+Options)
%
%   Perform pluggable logout

reply_logged_out(Options) :-
    swish_config:reply_logged_out(Options),
    !.
reply_logged_out(Options) :-
    reply_logged_out_page(Options).

reply_logged_out_page(Options) :-
    option(reply(Format), Options, json),
    (   Format == json
    ->  reply_json_dict(true)
    ;   true
    ).


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
%   protocol.  This is called by login.update() from login.js.

user_info(Request) :-
    http_parameters(Request,
                    [ reason(Reason, [optional(true)])
                    ]),
    (   current_user_info(Request, Info)
    ->  reply_json_dict(Info)
    ;   (   Reason == logout_by_http
        ->  broadcast(swish(logout(http)))
        ;   true
        ),
        reply_json_dict(null)
    ).

%!  current_user_info(+Request, -Info) is semidet.
%
%   If there is a logged in user, Info is a dict with information about
%   this user.

current_user_info(Request, Info) :-
    swish_config:user_info(Request, _Server, UserInfo),
    (   swish_config:user_profile(Request, Profile)
    ->  copy_fields([identity_provider, auth_method, logout_url],
                    UserInfo, Profile, Info)
    ;   Info = UserInfo
    ).

copy_fields([], _From, Dict, Dict).
copy_fields([H|T], From, Dict0, Dict) :-
    (   V = From.get(H)
    ->  copy_fields(T, From, Dict0.put(H,V), Dict)
    ;   copy_fields(T, From, Dict0, Dict)
    ).

