/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.
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

:- module(swish_render_ide,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module('../render').
:- use_module(library(prolog_code)).

:- register_renderer(ide, "IDE rendering support").

:- http_handler(swish(ide/edit), ide_edit, [id(ide_edit)]).

%!  term_rendering(+Predicate, +Vars, +Options)//
%
%   Renders a predicate indicator.

term_rendering(PI, _Vars, _Options) -->
    { is_predicate_indicator(PI),
      predicate_title(PI, Title),
      pi_link(PI, Attrs)
    },
    html(a([ 'data-render'('Predicate indicator'),
             class('render-predicate-indicator'),
             title(Title)
           | Attrs
           ],
           '~q'-PI)).


predicate_title(PI, Title) :-
    pi_head(PI, Head),
    findall(Line, title_line(user:Head, Line), Lines),
    atomics_to_string(Lines, "\n", Title).

title_line(Head, Title) :-
    predicate_property(Head, tabled),
    (   conj_of_properties(Head, [monotonic,incremental], Conj)
    ->  format(string(Title), 'Tabled as (~q)', [Conj])
    ;   Title = 'Tabled'
    ).
title_line(Head, Title) :-
    predicate_property(Head, dynamic),
    (   conj_of_properties(Head, [monotonic,incremental], Conj)
    ->  format(string(Title), 'Dynamic as (~q)', [Conj])
    ;   Title = 'Dynamic'
    ).
title_line(Head, Title) :-
    predicate_property(Head, file(File)),
    predicate_property(Head, line_count(Line)),
    format(string(Title), 'Defined at ~w:~d', [File, Line]).

conj_of_properties(Head, Candidates, Conj) :-
    findall(P, (predicate_property(Head, P), memberchk(P, Candidates)), Ps),
    comma_list(Conj, Ps).


%!  pi_link(+PI, -HTMLAttributes) is det.
%
%   Create a link for PI. Currently creates  an edit link for predicates
%   with a source.

pi_link(PI, [onClick("$.get('~w')"-[HREF])]) :-
    pi_head(PI, Head),
    predicate_property(user:Head, file(_)),
    predicate_property(user:Head, line_count(_)),
    !,
    format(string(PIs), '~q', [PI]),
    http_link_to_id(ide_edit, [pi(PIs)], HREF).
pi_link(_, []).


%!  ide_edit(+Request)
%
%   HTTP handler to edit a predicate

ide_edit(Request) :-
    http_parameters(Request,
                    [ pi(PIs, [])
                    ]),
    term_string(PI, PIs),
    edit(PI),
    reply_json_dict(true).
