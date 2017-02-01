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
            reply_logged_in/1           % +Options
          ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(option)).
:- use_module(library(apply)).

:- use_module(config, []).

/** <module> SWISH login support

This module provides the generic  code   to  deal  with _optional_ login
using multiple protocols. _Optional_ means that   SWISH may be used both
anonymously and after login.

This module cooperates with web/js/login.js. Login providers are defined
using configuration hooks. The various login options are accompagnied by
configuration files in =config-available=.
*/

:- http_handler(swish(login),     swish_login, [id(login)]).
:- http_handler(swish(user_info), user_info,   [id(user_info)]).


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
             span([ class(logout),
                    title('Click to logout')
                  ],
                  [ span(class([glyphicon, 'glyphicon-log-out']), []),
                    span(class(value), 'Logout')
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
%
%   At least one of user(User) or name(Name) must be present.

reply_logged_in(Options) :-
    reply_html_page(
        title('Logged in'),
        [ h4('Welcome'),
          p([ 'You have been identified ',
              \identity_provider(Options),
              ' as ',
              \user(Options)
            ]),
          \script
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

%!  script//
%
%   The login page is opened either  inside   an  iframe  inside a SWISH
%   modal dialog or inside a browser popup   window. This scripts adds a
%   button to dismiss the browser popup window.

script -->
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
    swish_config:user_info(Request, _Server, Info),
    !,
    reply_json_dict(Info).
user_info(_Request) :-
    reply_json_dict(null).
