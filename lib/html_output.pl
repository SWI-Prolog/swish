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
          [ html/1,                     % +Spec
            safe_html/1,                % +Spec
            safe_html//1                % +Spec
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(sandbox)).


		 /*******************************
		 *         OTHER OUTPUTS	*
		 *******************************/

%!  html(+Spec) is det.
%
%   Include HTML into the output.

:- html_meta
    html(html),
    safe_html(html),
    safe_html(html,?,?).

html(Spec) :-
    phrase(html(div(Spec)), Tokens),
    with_output_to(
        string(HTML),
        print_html(current_output, Tokens)),
    format('~w', [HTML]).

safe_html(Spec) :-
    is_safe_html(Spec),
    !,
    html(Spec).

safe_html(Spec) -->
    { is_safe_html(Spec) },
    !,
    html(Spec).

is_safe_html(M:Spec) :-
    prolog_load_context(module, M),
    must_be(ground, Spec),
    forall(sub_term(\(Eval), Spec),
           safe_eval(Eval, M)).

safe_eval(Goal, M) :-
    dcg_extend(Goal, DcgGoal),
    safe_goal(M:DcgGoal).

dcg_extend(Goal, DcgGoal) :-
    must_be(callable, Goal),
    Goal \= (_:_),
    Goal =.. List,
    append(List, [_,_], ExList),
    DcgGoal =.. ExList.

swish:goal_expansion(html(Spec), safe_html(Spec)).
swish:goal_expansion(html(Spec, L,T), safe_html(Spec, L, T)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_meta_predicate(md_eval:safe_html/1).
sandbox:safe_meta_predicate(md_eval:safe_html/3).
sandbox:safe_primitive(md_eval:html(Spec)) :-
    \+ sub_term(\(_), Spec).
sandbox:safe_meta_predicate(md_eval:is_safe_html/1).
