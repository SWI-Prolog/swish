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

:- module(swish_email,
          [ smtp_send_mail/3,           % +To, :Goal, +Options
            smtp_send_html/3,           % +To, :Content, +Options
            email_action_link//4,	% :Label, :Reply, :Action, +Options
            email_cleanup_db/0,
            public_url/4                % +To, +Query, -URL, +Options
          ]).
:- use_module(library(smtp)).           % from pack smtp
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(base64)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/html_write)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(persistency)).
:- use_module(library(broadcast)).
:- use_module(library(user_profile)).

/** <module> Email plugin for SWISH

This module deals with sending  email  from   SWISH.  Email  is sent for
confirmation (of the email address) as well as for notifications.
*/

:- html_meta
    smtp_send_html(+, html, +),
    email_action_link(html, 1, 0, +, ?, ?).

:- setting(timeout, integer, 24*3600*7,
           "Timeout for handling email reply").
:- setting(database, callable, swish('data/confirm.db'),
           "File specification for E-mail confirmations").

:- http_handler(swish('mail/action/'), on_mail_link,
                [prefix, id(on_mail_link)]).


		 /*******************************
		 *            DATABASE		*
		 *******************************/

:- persistent
        request(key:string,
                deadline:integer,
                action:callable,
                reply:callable).

email_open_db :-
    db_attached(_),
    !.
email_open_db :-
    setting(database, Spec),
    absolute_file_name(Spec, Path, [access(write)]),
    db_attach(Path, [sync(close)]).

%!  email_cleanup_db
%
%   Strip the email confirmation queue from outdated messages.

email_cleanup_db :-
    with_mutex(swish_email, email_cleanup_db_sync).

email_cleanup_db_sync :-
    get_time(Now),
    forall(( request(Key, Deadline, _, _),
             Now > Deadline
           ),
           retract_request(Key, Deadline, _, _)),
    db_sync(gc).



		 /*******************************
		 *           EMAIL		*
		 *******************************/

%!  smtp_send_html(+To, :Content, +Options)
%
%   Send an HTML mail to To  using   HTML  content  Content. Options are
%   passed  to  smtp_send_mail/3,  passing    as   default  content-type
%   `test/html`.

smtp_send_html(To, Content, Options) :-
    merge_options(Options,
                  [ header('MIME-Version'('1.0')),
                    content_type(text/html)
                  ], Options1),
    smtp_send_mail(To, html_body(Content), Options1).

html_body(Content, Out) :-
    phrase(html(html(Content)), Tokens),
    print_html(Out, Tokens).

%!  generate_key(-Key) is det.
%
%   Generate a random confirmation key

generate_key(Key) :-
    length(Codes, 16),
    maplist(random_between(0,255), Codes),
    phrase(base64url(Codes), Encoded),
    string_codes(Key, Encoded).


		 /*******************************
		 *           WEB PAGE		*
		 *******************************/

%!  public_url(+To, +Query, -URL, +Options) is det.
%
%   True when URL is a link to handler To with Query

public_url(To, Query, URL, Options) :-
    http_link_to_id(To, Query, RequestURI),
    host_url(HostURL, Options),
    atom_concat(HostURL, RequestURI, URL).

host_url(HostURL, Options) :-
    option(host_url(HostURL), Options),
    !.
host_url(HostURL, _Options) :-
    http_current_request(Request),
    http_public_host_url(Request, HostURL).


%!  on_mail_link(Request)
%
%   React after the user opens a link from the email.

on_mail_link(Request) :-
    email_open_db,
    option(path_info(Path), Request),
    atom_string(Path, Key),
    with_mutex(swish_email,
               retract_request(Key, Deadline, Action, Reply)),
    get_time(Now),
    Now =< Deadline,
    call(Action),
    call(Reply, Request).


		 /*******************************
		 *            ACTIONS		*
		 *******************************/

%!  email_action_link(:Label, :Reply, :Action, +Options)//
%
%   Generate a link in an HTML mail   page  that, when clicked, executes
%   Action and if successful replies to the request using Reply.

email_action_link(Label, Reply, Action, Options) -->
    { email_open_db,
      generate_key(Key),
      public_url(on_mail_link, path_postfix(Key), HREF, Options),
      setting(timeout, TMODef),
      option(timeout(TMO), Options, TMODef),
      get_time(Now),
      Deadline is round(Now+TMO),
      with_mutex(swish_email,
                 assert_request(Key, Deadline, Action, Reply))
    },
    html(a(href(HREF), Label)).


		 /*******************************
		 *             EVENTS		*
		 *******************************/

:- listen(user_profile(modified(User, email, Old, New)),
          email_verify(User, Old, New)).

email_verify(_User, _Old, "") :-
    !.
email_verify(User, Old, Email) :-
    smtp_send_html(Email, \email_verify(User, Old, Email),
                   [ subject("[SWISH] Please verify email")
                   ]).


email_verify(User, "", New) -->
    html([ p(['Dear ', \name(User), ',']),
           p(['We have received a request to set the email account \c
               for SWISH', \host, ' to ', b(New), '.' ]),
           ul([ li(\confirm_link(User, New))
              ])
         ]).
email_verify(User, Old, New) -->
    html([ p(['Dear ', \name(User), ',']),
           p(['We have received a request to change the email account \c
               for SWISH', \host, ' from ', b(Old), ' to ', b(New), '.' ]),
           ul([ li(\confirm_link(User, New))
              ])
         ]).

confirm_link(User, New) -->
    email_action_link(["Verify email as ", New], verified_email(User, New),
                      verify_email(User), []).

verify_email(User) :-
    set_profile(User, email_verified(true)).


name(User) -->
    { user_field(Field),
      Term =.. [Field, Name],
      profile_property(User, Term)
    },
    html(Name).

user_field(name).
user_field(given_name).
user_field(nick_name).
user_field(family_name).

host -->
    { host_url(HostURL, []) },
    !,
    html([' at ', HostURL]).
host -->
    [].

verified_email(User, NewEmail, _Request) :-
    reply_html_page(
        email_confirmation,
        title('[SWISH] Email verified'),
        [ p(['Dear ', \name(User), ',']),
          p(['Your email address ~w has been verified'-[NewEmail]])
        ]).
