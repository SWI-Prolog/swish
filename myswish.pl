/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2019, VU University Amsterdam
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

:- module(my_swish_ide,
          [ swish/0,
            swish/1
          ]).

/** <module> Provide swish/0 from any Prolog session

This module provides swish/0 and swish/1   as  auto-loaded commands from
any Prolog shell. This allows you to run   the  command below in a shell
and both enjoy swish and keep your local development environment.

    ?- swish.

## Installation

    mkdir -p ~/lib/prolog
    cp myswish.pl ~/lib/prolog
    edit ~/lib/prolog/myswish.pl	# fix path below
    swipl
    ?- make_library_index('~/lib/prolog').
    ?- halt.

And test the result by starting Prolog   in a __writable directory__ and
run `?- swish.`. If all is right,  Prolog should report it started swish
and your browser should be directed to the newly created swish instance.

## Remarks

Running swish creates a directory `data`  in the current directory where
SWISH stores settings and your  programs.  You   can  copy  this  to new
locations or delete it when done.
*/

% EDIT THIS PATH
:- use_module('/usr/local/share/swish/ide').
