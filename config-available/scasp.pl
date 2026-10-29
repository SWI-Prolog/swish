/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017-2020, VU University Amsterdam
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

:- module(swish_config_scasp, []).
% Uncomment for the old version
:- set_prolog_flag(sscasp, true).
:- use_module(library(scasp)).
:- use_module(library(scasp/swish)).

/** <module> Make s(CASP) available

library(scasp) is either of the two implementations the s(CASP) pack
ships: the SWI-Prolog port, or ssCASP when SWISH is started with
`swipl -Dsscasp`.  Both give library(scasp/swish), the adapter that shows
the model and the justification.  Their stylesheet and script are the
same files under different names, `scasp.*` and `sscasp.*`, because both
are found through the `js` and `css` search paths.
*/

:- multifile
    swish_config:web_plugin/1.

swish_config:web_plugin(
    json{name: scasp,
         js:   root(JS),
         css:  root(CSS)
        }) :-
    web_files(JS, CSS).

web_files('js/sscasp.js', 'css/sscasp.css') :-
    predicate_property(scasp:scasp(_,_), imported_from(sscasp_dyncall)),
    !.
web_files('js/scasp.js', 'css/scasp.css').
