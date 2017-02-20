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
            email_confirm/4,            % +To, :Message, :Action, +Options
            email_cleanup_db/0
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
    email_confirm(+, html, 0, +),
    smtp_send_html(+, html, +).

:- setting(timeout, integer, 24*3600*7,
           "Timeout for handling email reply").
:- setting(database, callable, swish('data/confirm.db'),
           "File specification for E-mail confirmations").

:- http_handler(swish('mail/confirm/'), confirm, [prefix, id(email_confirm)]).
:- http_handler(swish('mail/decline/'), decline, [prefix, id(email_decline)]).


		 /*******************************
		 *            DATABASE		*
		 *******************************/

:- persistent
        request(key:string,
                deadline:integer,
                action:callable).

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
    forall(( request(Key, Deadline, _),
             Now > Deadline
           ),
           retract_request(Key, Deadline, _)),
    db_sync(gc).



		 /*******************************
		 *           EMAIL		*
		 *******************************/

%!  email_confirm(+To, :Message, :Action, +Options)
%
%   Send an email to confirm some action.  Options are passed to
%   smtp_send_mail/3.   Additional options are:
%
%     - host_url(+URL)
%     URL of the host to contact for confirm/decline.  Default is
%     obtained using http_public_host_url/2.
%     - timeout(+Seconds)
%     Time in seconds that the request is valid.  Can be a formula.
%     - subject(+Subject)
%     Subject of the message.  Default is "Confirmation request"

email_confirm(To, Message, Action, Options) :-
    email_open_db,
    host_url(HostURL, Options),
    generate_key(Key),
    http_link_to_id(email_confirm, path_postfix(Key), Confirm0),
    http_link_to_id(email_decline, path_postfix(Key), Decline0),
    atom_concat(HostURL, Confirm0, Confirm),
    atom_concat(HostURL, Decline0, Decline),
    merge_options(Options, [subject("Confirmation request")], MailOptions),
    smtp_send_html(To, \body(Message, Confirm, Decline), MailOptions),
    setting(timeout, TMODef),
    option(timeout(TMO), Options, TMODef),
    get_time(Now),
    Deadline is round(Now+TMO),
    with_mutex(swish_email,
               assert_request(Key, Deadline, Action)).

host_url(HostURL, Options) :-
    option(host_url(HostURL), Options),
    !.
host_url(HostURL, _Options) :-
    http_current_request(Request),
    http_public_host_url(Request, HostURL).

body(Message, Confirm, Decline) -->
    html(Message),
    html(ul([ li(['To confirm, visit ', a(href(Confirm),Confirm)]),
              li(['To decline, visit ', a(href(Decline),Decline)])
            ])).


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

%!  confirm(+Request)
%
%   HTTP handler to deal with an email confirmation

confirm(Request) :-
    email_open_db,
    option(path_info(Path), Request),
    atom_string(Path, Key),
    with_mutex(swish_email,
               retract_request(Key, Deadline, Action)),
    get_time(Now),
    Now =< Deadline,
    call(Action),
    reply_html_page(
        email_confirmation,
        title('Confirmed'),
        [ h4('Confirmed'),
          p([ \action(Action), ' has been confirmed.' ])
        ]).


%!  decline(+Request)
%
%   HTTP handler to deal with an email declineation

decline(Request) :-
    email_open_db,
    option(path_info(Path), Request),
    atom_string(Path, Key),
    with_mutex(swish_email,
               retract_request(Key, _Deadline, Action)),
    reply_html_page(
        email_confirmation,
        title('Declined'),
        [ h4('Confirmed'),
          p([ \action(Action), ' has been declined.' ])
        ]).

body(email_confirmation, Body) -->
    { host_url(HostURL, []) },
    html(Body),
    html([ hr([]),
           address([ 'SWISH at ', a(href(HostURL), HostURL) ])
         ]).

action(_Module:Goal) -->
    action_message(Goal),
    !.
action(_:Goal) -->
    html('Unknown (~p)'-[Goal]).

action_message(set_profile(_User, email_verified=true)) -->
    html('Email verification').


		 /*******************************
		 *             EVENTS		*
		 *******************************/

:- listen(user_profile(modified(User, email, Old, New)),
          email_verify(User, Old, New)).

email_verify(_User, _Old, "") :-
    !.
email_verify(User, Old, Email) :-
    email_confirm(Email,
                  \email_verify(User, Old, Email),
                  set_profile(User, email_verified=true),
                  [subject("Please verify email for SWISH")]).

email_verify(User, "", New) -->
    html([ p(['Dear ', \name(User), ',']),
           p(['We have received a request to set the email account \c
               for SWISH', \host, ' to ', b(New), '.' ])
         ]).
email_verify(User, Old, New) -->
    html([ p(['Dear ', \name(User), ',']),
           p(['We have received a request to change the email account \c
               for SWISH', \host, ' from ', b(Old), ' to ', b(New), '.' ])
         ]).

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
