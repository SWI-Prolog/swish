/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2018, VU University Amsterdam
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

:- module(swish_html_output,
          [ html/1,                             % +Spec
            html//1                             % +Spec, //
          ]).
:- use_module(library(http/html_write), except([html//1])).
:- use_module(library(pengines)).
:- use_module(library(sandbox)).
:- use_module(library(lists)).
:- use_module(library(error)).

/** <module> SWISH HTML Output
*/


%!  html(+Spec) is det.
%
%   Include HTML into the output.

:- html_meta
    html(html).

html(Spec) :-
    make_safe_html(Spec, SafeSpec),
    output_html(SafeSpec).

html(Spec) -->
    { make_safe_html(Spec, SafeSpec) },
    html_write:html(SafeSpec).

output_html(SafeSpec) :-
    pengine_self(_),
    !,
    pengines_io:send_html(SafeSpec).
output_html(Spec) :-
    phrase(html(div(Spec)), Tokens),
    with_output_to(
        string(HTML),
        print_html(current_output, Tokens)),
    format('~w', [HTML]).

make_safe_html(HTML0, HTML) :-
    (   prolog_load_context(module, M)
    ->  true
    ;   pengine_self(M)
    ),
    make_safe_html(HTML0, M, HTML).

make_safe_html(Var, M, swish_html_output:html(M:Var)) :-
    var(Var),
    !.
make_safe_html(Module:HTML0, M, Module:HTML) :-
    !,
    Module == M,
    make_safe_html(HTML0, M, HTML).
make_safe_html([], _, []) :-
    !.
make_safe_html([H0|T0], M, [H|T]) :-
    !,
    make_safe_html(H0, M, H),
    make_safe_html(T0, M, T).
make_safe_html(Format-Args, _M, Format-Args) :-
    !,
    safe_goal(format(Format, Args)).
make_safe_html(\Goal, M, \Goal) :-
    !,
    must_be(callable, Goal),
    dcg_extend(Goal, DcgGoal),
    safe_goal(M:DcgGoal).
make_safe_html(Elem0, M, Elem) :-
    Elem0 =.. [Name, Attrs0, Content0],
    !,
    safe_attrs(Attrs0, M, Attrs),
    make_safe_html(Content0, M, Content),
    Elem =.. [Name, Attrs, Content].
make_safe_html(Elem0, M, Elem) :-
    Elem0 =.. [Name, AttrsOrContent0],
    !,
    (   html_write:layout(Name, _, empty)
    ->  safe_attrs(AttrsOrContent0, M, Safe)
    ;   make_safe_html(AttrsOrContent0, M, Safe)
    ),
    Elem =.. [Name, Safe].
make_safe_html(Text, _, Text) :-
    atomic(Text).

safe_attrs([], _, []) :-
    !.
safe_attrs([H0|T0], M, [H|T]) :-
    !,
    safe_attrs(H0, M, H),
    safe_attrs(T0, M, T).
safe_attrs(Name=Value0, M, Name=Value) :-
    !,
    safe_attr_value(Value0, M, Value).
safe_attrs(NameValue0, M, NameValue) :-
    !,
    NameValue0 =.. [Name,Value0],
    safe_attr_value(M, Value0, Value),
    NameValue =.. [Name,Value].

safe_attr_value(_, Value0, _) :-
    var(Value0),
    !,
    instantiation_error(Value0).
safe_attr_value(M, Value0, Value) :-
    is_list(Value0),
    !,
    maplist(safe_attr_value(M), Value0, Value).
safe_attr_value(_M, Format-Args, Format-Args) :-
    !,
    safe_goal(format(Format, Args)).
safe_attr_value(M, A0+B0, A+B) :-
    safe_attr_value(M, A0, A),
    safe_attr_value(M, B0, B).
safe_attr_value(_, V, V) :-
    atomic(V).


dcg_extend(Goal, DcgGoal) :-
    must_be(callable, Goal),
    Goal \= (_:_),
    Goal =.. List,
    append(List, [_,_], ExList),
    DcgGoal =.. ExList.

:- multifile sandbox:safe_primitive/1.

sandbox:safe_meta_predicate(swish_html_output:html/1).
sandbox:safe_meta_predicate(swish_html_output:html/3).
