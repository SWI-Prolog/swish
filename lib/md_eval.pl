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

:- module(md_eval,
          [ html/1                              % +Spec
          ]).

:- use_module(library(modules)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(occurs)).
:- use_module(library(settings)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(http/html_write)).

/** <module> Provide evaluable markdown

This  module  adds  evaluable  sections  to   markdown  cells  in  SWISH
notebooks. Such cells are written as

  ==
  ```{eval}
  <Prolog code>
  ```
  ==
*/

%!  eval_dom(+DOM0, -DOM, +Options) is semidet.
%
%   This predicate post-processes  the  wiki  DOM   result  if  the  DOM
%   contains at least one  `eval`  code   fragment.  The  evaluation  is
%   executed  in  a  sandboxed  environment,   much  like  the  Pengines
%   infrastructure.
%
%   A code fragment is represented by a term of this shape:
%
%       pre([class(code), ext(Ext)], Text)

eval_dom(DOM0, DOM, Options) :-
    contains_eval(DOM0),
    !,
    in_temporary_module(
        Module,
        prepare(Module),
        md_eval(Module, Options, DOM0, DOM, 0, _)).

prepare(Module) :-
    setting(swish:program_space, SpaceLimit),
    set_module(Module:program_space(SpaceLimit)),
    delete_import_module(Module, user),
    add_import_module(Module, swish, start).

%!  md_eval(+Module, +Options, +DOM0, -DOM, +FragI0, -FragI1)

md_eval(Module, Options, Pre, Evaluated, I0, I) :-
    pre(Pre, eval, Code),
    !,
    eval(Module, I0, Code, Evaluated, Options),
    I is I0 + 1.
md_eval(Module, Options, Compound, Evaluated, I0, I) :-
    compound(Compound),
    !,
    compound_name_arguments(Compound, Name, Args0),
    foldl(md_eval(Module, Options), Args0, Args, I0, I),
    compound_name_arguments(Evaluated, Name, Args).
md_eval(_, _, DOM, DOM, I, I) :-
    !.

:- meta_predicate
    call_collect_messages(0, -).

eval(Module, I, Code, div(class(eval), Evaluated), Options) :-
    catch(( call_collect_messages(
                do_eval(Module, I, Code, Evaluated0, Options),
                Messages),
            append(Evaluated0, Messages, Evaluated)
          ),
          Error,
          failed(Error, Evaluated)).

do_eval(Module, I, Code, [div(class(output), DOM)], _Options) :-
    debug(md(eval), 'Evaluating ~p', [Code]),
    format(atom(Id), 'eval://~w-~w', [Module, I]),
    with_output_to(
        codes(Codes),
        setup_call_cleanup(
            open_string(Code, In),
            load_files(Module:Id,
                       [ stream(In),
                         sandboxed(true)
                       ]),
            close(In))),
    wiki_codes_to_dom(Codes, [], DOM).

contains_eval(DOM) :-
    sub_term(Pre, DOM),
    pre(Pre, eval, _),
    !.

pre(pre(Attrs, Text), Ext, Text) :-
    memberchk(ext(Ext), Attrs).

:- thread_local
    saved_message/1.

failed(Error, [div(class(error), Message)]) :-
    message_to_string(Error, Message).

call_collect_messages(Goal, Messages) :-
    setup_call_cleanup(
        asserta((user:thread_message_hook(Term, Kind, Lines) :-
                   save_message(Term, Kind, Lines)), Ref),
        Goal,
        collect_messages(Ref, Messages)).

save_message(_Term, Kind, Lines) :-
    kind_prefix(Kind, Prefix),
    with_output_to(
        string(Msg),
        print_message_lines(current_output, Prefix, Lines)),
    assertz(saved_message(div(class(Kind), Msg))).

kind_prefix(error,   '% ERROR: ').
kind_prefix(warning, '% Warning: ').

collect_messages(Ref, Messages) :-
    erase(Ref),
    findall(Msg, retract(saved_message(Msg)), Messages).


		 /*******************************
		 *         OTHER OUTPUTS	*
		 *******************************/

%!  html(+Spec) is det.
%
%   Include HTML into the output.

html(Spec) :-
    phrase(html(html(Spec)), Tokens),
    with_output_to(
        string(HTML),
        print_html(current_output, Tokens)),
    format('~w', [HTML]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(md_eval:html(Spec)) :-
    \+ sub_term(\(_), Spec).


		 /*******************************
		 *           ACTIVATE		*
		 *******************************/

:- multifile
    swish_markdown:dom_expansion/2.

swish_markdown:dom_expansion(DOM0, DOM) :-
    eval_dom(DOM0, DOM, []).
