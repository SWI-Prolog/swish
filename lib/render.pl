/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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

:- module(swish_render,
	  [ use_rendering/1,		% +Renderer
	    use_rendering/2,		% +Renderer, +Options

	    register_renderer/2,	% Declare a rendering module
	    current_renderer/2		% Name, Comment
	  ]).
:- use_module(library(pengines_io), []).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(option)).
:- use_module(library(error)).

:- meta_predicate
	register_renderer(:, +),
	use_rendering(:),
	use_rendering(:, +).

/** <module> SWISH term-rendering support

This module manages rendering answers  using alternative vizualizations.
The idea is that a  specific  context   _uses_  zero  or  more rendering
modules.  These  rendering  modules   provide    an   alternative   HTML
representation for the target term. If  multiple possible renderings are
found,  a  =|<div  class="render-multi">|=  element  is  generated  that
contains the alternative renderings. The   jQuery  plugin =renderMulti=,
defined in =answer.js= adds the  behaviour   to  change rendering to the
generated div.

The user can import rendering schemes into the current context using the
directive below. `Spec` is either an atom   or string, making the system
look for render(Spec), or it is a   (single) file specification that can
be used for use_module/1.

  ==
  :- use_rendering(Spec).
  ==

A rendering module is a  Prolog   module  that  defines the non-terminal
term_rendering//3,  which  will  be  called  as  below.  `Term`  is  the
(non-var) term that must be rendered, `Vars` is a list of variable names
bound to this term and `Options` is a   list of write options that would
normally  be  passed  to  write_term/3.  The   grammar  is  executed  by
library(http/html_write) and must  generate   compatible  tokens  (which
means it must call html//1 to generate HTML tokens).

  ==
  phrase(Renderer:term_rendering(Term, Vars, Options), Tokens)
  ==
*/

:- multifile user:file_search_path/2.

user:file_search_path(render, swish('lib/render')).


%%	use_rendering(+FileOrID)
%
%	Register an answer  renderer.   Same  as use_rendering(FileOrID,
%	[]).
%
%	@see use_rendering/2.

:- multifile system:term_expansion/2.

use_rendering(Rendering) :-
	use_rendering(Rendering, []).

%%	use_rendering(:ID, +Options)
%
%	Register an answer renderer  with   options.  Options are merged
%	with   write-options   and   passed     to    the   non-terminal
%	term_rendering//3 defined in the rendering module.

use_rendering(Rendering, Options) :-
	Rendering = Into:Renderer,
	must_be(atom, Renderer),
	(   renderer(Renderer, _, _)
	->  true
	;   existence_error(renderer, Renderer)
	),
	retractall(Into:'swish renderer'(Renderer, _)),
	assertz(Into:'swish renderer'(Renderer, Options)).

system:term_expansion((:- use_rendering(Renderer)), Expanded) :-
	expand_rendering(Renderer, [], Expanded).
system:term_expansion((:- use_rendering(Renderer, Options)), Expanded) :-
	expand_rendering(Renderer, Options, Expanded).

expand_rendering(Module:Renderer, Options,
		 [ (:- discontiguous(Module:'swish renderer'/2)),
		   Module:'swish renderer'(Renderer, Options)
		 ]) :- !,
	must_be(atom, Module),
	must_be(atom, Renderer).
expand_rendering(Renderer, Options,
		 [ (:- discontiguous('swish renderer'/2)),
		   'swish renderer'(Renderer, Options)
		 ]) :-
	must_be(atom, Renderer).

%%	pengines_io:binding_term(+Term, +Vars, +Options) is semidet.
%
%	Produce alternative renderings for Term, which  is a binding for
%	Vars.

:- multifile pengines_io:binding_term//3.

pengines_io:binding_term(Term, Vars, Options) -->
	{ option(module(Module), Options),
	  findall(Tokens,
		  call_term_rendering(Module, Term, Vars, Options, Tokens),
		  NestedTokens),
	  NestedTokens \== [], !
	},
	alt_renderer(NestedTokens, Term, Options).

%%	call_term_rendering(+Module, +Term, +Vars, +Options, -Tokens) is nondet.
%
%	Call  term_rendering//3  in  all  modules    from  which  Module
%	inherits.

call_term_rendering(Module, Term, Vars, Options, Tokens) :-
	State = state([]),
	default_module(Module, Target),
	current_predicate(Target:'swish renderer'/2),
	Target:'swish renderer'(Name, RenderOptions),
	atom(Name),
	is_new(State, Name),
	renderer(Name, RenderModule, _Comment),
	merge_options(RenderOptions, Options, AllOptions),
	catch(phrase(RenderModule:term_rendering(Term, Vars, AllOptions), Tokens),
	      E, rendering_error(E, Name, Tokens)).

rendering_error(Error, Renderer, Tokens) :-
	message_to_string(Error, Msg),
	phrase(html(div(class('render-error'),
			[ 'Renderer ', span(Renderer),
			  ' error: ', span(class(error), Msg)
			])), Tokens).


%%	is_new(!State, +M) is semidet.
%
%	Only succeeds once for each new ground value M.

is_new(State, M) :-
	arg(1, State, Seen),
	(   memberchk(M, Seen)
	->  fail
	;   nb_linkarg(1, State, [M|Seen])
	).

%%	alt_renderer(+Specialised, +Term, +Options)//
%
%	Create a rendering selection object after we have found at least
%	one alternative rendering for Term.

alt_renderer(Specialised, Term, Options) -->
	html(div(class('render-multi'),
		 \specialised(Specialised, Term, Options))).

specialised([], Term, Options) -->
	html(span([ class('render-as-prolog'),
		    'data-render'('Prolog term')
		  ],
		  \term(Term, Options))).
specialised([H|T], Term, Options) -->
	tokens(H),
	specialised(T, Term, Options).

tokens([]) --> [].
tokens([H|T]) --> [H], tokens(T).


		 /*******************************
		 *	   REGISTRATION		*
		 *******************************/

:- multifile
	renderer/3.

%%	current_renderer(Name, Comment) is nondet.
%
%	True when renderer Name is declared with Comment.

current_renderer(Name, Comment) :-
	renderer(Name, _Module, Comment).

%%	register_renderer(:Name, +Comment)
%
%	Register a module as SWISH rendering component.

register_renderer(Name, Comment) :-
	throw(error(context_error(nodirective, register_renderer(Name, Comment)),
		    _)).

system:term_expansion((:- register_renderer(Name, Comment)),
		    swish_render:renderer(Name, Module, Comment)) :-
	prolog_load_context(module, Module).
