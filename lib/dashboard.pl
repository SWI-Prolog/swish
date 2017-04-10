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

:- module(swish_dashboard,
          [ parameters/1                        % +Parameters
          ]).
:- use_module(library(pengines)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(http/html_write)).

:- use_module(bootstrap).
:- use_module(form).

/** <module> Provide non-programmer query execution

@tbd	Colour support for this term
*/

%!  parameters(+Spec:list) is det.
%
%   Fill query parameters. Spec is a list   of query parameters that use
%   the following syntax:
%
%       Var: Option ("+" Option)*
%
%   Defined options are:
%
%       - type(+Type)
%       Defines the type to be used for Var.  Types are defined by
%       library(error).  Type terms can also be used without the
%       type(Type) wrapper.
%       - default(+Default)
%       Default value to use.

parameters(List) :-
    include(not_filled, List, ToFill),
    debug(dashboard(param), 'ToFill: ~p', [ToFill]),
    fill(ToFill).

not_filled(Var:_) :-
    var(Var).

fill([]) :-
    !.
fill(NotFilled) :-
    maplist(input, NotFilled, FieldWidgets),
    !,
    buttons(Buttons),
    append(FieldWidgets, Buttons, Widgets),
    html_string(\bt_form(Widgets,
                         [ class('form-horizontal'),
                           label_columns(sm-3)
                         ]), HTML),
    Prompt = _{ type: form,
                html: HTML
              },
    pengine_input(Prompt, Reply),
    bind_form_reply(NotFilled, Reply).

buttons(
    [ button_group(
          [ button(run, submit,
                   [ type(primary),
                     data([action(run)])
                   ]),
            button(cancel, button,
                   [ type(danger),
                     data([action(cancel)])
                   ])
          ],
          [])
    ]).


bind_form_reply(_NotFilled, cancel) :-
    !,
    fail.
bind_form_reply(NotFilled, Reply) :-
    maplist(form_field, NotFilled, Fields),
    validate_form(Reply, Fields).

form_field(Var:Opts, field(Name, Var, [Type|Extra])) :-
    opt_list(Opts, Options),
    option(name(Name), Options),
    option(type(Type), Options),
    (   option(default(Default), Options)
    ->  Extra = [default(Default)]
    ;   Extra = []
    ).


%!  input(+ParamSpec, -InputItem) is det.
%
%   Construct a Bootstrap input item from ParamSpec.

input(_Var:Opts, Input) :-
    opt_list(Opts, Options),
    select_option(type(Type), Options, Options1),
    select_option(name(Name), Options1, Options2),
    input(Type, Name, Options2, Input).

input(Type, Name, Options,
      input(Name, text,
            [ data('search-in'=Set),
              class(typeahead)
            | Options
            ])) :-
    typeahead(Type, Set),
    !.
input(string, Name, Options,
      textarea(Name, Options)) :-
    option(rows(_Rows), Options),
    !.
input(_, Name, Options,
      input(Name, text, Options)).

%!  typeahead(+Type, -Set)
%
%   True when an object of Type can be   selected from an input set that
%   uses type-ahead search using the data set Set.

typeahead(user, users).

:- multifile error:has_type/2.

error:has_type(user, _Dict) :-
    true.


		 /*******************************
		 *   INSPECT VARIABLE NAMES	*
		 *******************************/

:- multifile
    swish:goal_expansion/2.

swish:goal_expansion(parameters(Params0), parameters(Params)) :-
    add_var_names(Params0, 1, Params),
    Params0 \== Params.

add_var_names([], _, []).
add_var_names([H0|T0], N0, [H|T]) :-
    add_var_name(H0, N0, H),
    N is N0 + 1,
    add_var_names(T0, N, T).


add_var_name(Var:Options, _, Var:Options) :-
    opt(name(_), Options),
    !.
add_var_name(Var:Options, N, Var:name(Name)+Options) :-
    (   var_property(Var, name(Name))
    ->  true
    ;   atom_concat('Param', N, Name)
    ).


		 /*******************************
		 *            BASICS		*
		 *******************************/

opt_list(Opts, List) :-
    phrase(opts(Opts), List0),
    add_type(List0, List).

opts(OptA+OptB) -->
    !,
    opts(OptA),
    opts(OptB).
opts(Opt) -->
    [Opt].

add_type(Options, Options) :-
    option(type(_), Options), !.
add_type(List, Options) :-
    select(Type, List, Options1),
    current_type(Type, _, _),
    !,
    Options = [type(Type)|Options1].
add_type(Options, [type(term)|Options]).


%!  opt(?Option, +Options) is nondet.
%
%   Opt is a member of the Options term.

opt(Opt, Opts) :-
    \+ functor(Opts, +, 2), !,
    Opt = Opts.
opt(Opt, Opt+_).
opt(Opt, _+Opts) :-
    opt(Opt, Opts).

html_string(HTML, String) :-
    phrase(html(HTML), Tokens),
    !,
    delete(Tokens, nl(_), SingleLine),
    with_output_to(string(String), print_html(SingleLine)).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_dashboard:parameters(_)).
