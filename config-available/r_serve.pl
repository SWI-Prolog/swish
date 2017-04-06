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

:- module(config_r_serve, []).
:- use_module(library(settings)).
:- use_module(swish:swish(lib/r_swish)).

/** <module> Provide R access using Rserve

This config file provides access   to [R](https://www.r-project.org/) by
means                               of                               the
[Rserve](https://cran.r-project.org/web/packages/Rserve/index.html)
package.

To use this config file, you need to

  1. Install the Rserve client Prolog pack from Prolog:

     ```
     $ git submodule update pack/rserve_client
     $ swipl run.pl
     ?- pack_rebuild(rserve_client).
     ```

  2. Install R in an OS sandbox.  We provided a Docker recipe for that
     at https://github.com/JanWielemaker/rserve-sandbox.  This docker
     image makes Rserve accessible at `/home/rserve/socket`, which is
     accessible for users in the group `rserve`

  3. Add the user running SWISH to the group `rserve` to provide access
     to the Rserve socket.
*/

% EDIT: COMMENT IF R IS NOT RUNNING IN AN OS SANDBOX.
:- use_module(library(r/r_sandbox)).

% EDIT: Uncomment and edit these values if Rserve is accessible
% from another location.  If a `socket` is configured and exists
% the socket is used.  Otherwise it will try a TCP connection to
% the given host and port.
%
% IF R IS NOT RUNNING IN AN OS SANDBOX, COMMENT THE SANDBOX LINE
% ABOVE AND RUN SWISH WITH AUTHENTICATION SUPPORT.

%:- set_setting_default(rserve:socket, '/home/rserve/socket').
%:- set_setting_default(rserve:host,   '127.0.0.1').
%:- set_setting_default(rserve:port,   6311).
