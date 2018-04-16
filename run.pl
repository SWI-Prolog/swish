/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2017, VU University Amsterdam
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

:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(server).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Usage:

    swipl run.pl [--port=Port]

Simple start script for running SWISH  in an interactive Prolog session.
This version is intended for development  and testing purposes. Checkout
daemon.pl if to deploy SWISH as a server
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Using `localhost:Port`, we only bind to localhost interface!
% Use plain `3050` (or any port number you like) to make the server
% accessible from all network interfaces.

:- initialization(run_swish, main).

run_swish :-
    set_prolog_flag(toplevel_goal, prolog), % run interactively
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _, Options),
    option(port(Port), Options, 3050),
    server(localhost:Port).

