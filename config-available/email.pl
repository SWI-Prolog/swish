/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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

:- module(config_email,
          [ email_test/1                % +To
          ]).
:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(swish(lib/plugin/email)).

/** <module> Configure email

This module configures the email client   to  send confirmation messages
and notifications.
*/

% EDIT: Configure email client. See library(smtp) for details. Next, run
% email_test(MyMailAddress) to verify that the   setup is correct. Watch
% your spam box if Prolog reports no problems   but you do not receive a
% message.
:- set_setting_default(smtp:host,        '****').
:- set_setting_default(smtp:port,        ****).
:- set_setting_default(smtp:security,    ****).
:- set_setting_default(smtp:from,        '****').
:- set_setting_default(smtp:user,        '****').
:- set_setting_default(smtp:password,    "****").
%:- set_setting_default(smtp:auth_method, default).
%:- set_setting_default(smtp:hostname,    '').

%!  email_test(+To)
%
%   Test basic setup of the email client.

email_test(To) :-
    setup_call_cleanup(
        debug(smtp),
        smtp_send_mail(To,
                       send_message,
                       [ subject('SWISH email test'),
                         mailed_by(true)
                       ]),
        nodebug(smtp)).

send_message(Out) :-
    format(Out, 'This is a test message from the swish email client.\n', []).

