/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(swish_trace,
	  [ '$swish wrapper'/1		% +Goal
	  ]).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(pengines_io), [pengine_io_predicate/1]).
:- use_module(library(sandbox), []).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_write)).

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	'$swish wrapper'(0).

/** <module>

Allow tracing pengine execution under SWISH.
*/

:- multifile
	user:prolog_trace_interception/4.

user:prolog_trace_interception(Port, Frame, _CHP, Action) :-
	pengine_self(Pengine),
	prolog_frame_attribute(Frame, predicate_indicator, PI),
	debug(trace, 'HOOK: ~p ~p', [Port, PI]),
	pengine_property(Pengine, module(Module)),
	wrapper_frame(Frame, WrapperFrame),
	debug(trace, 'Me: ~p, wrapper: ~p', [Frame, WrapperFrame]),
	prolog_frame_attribute(WrapperFrame, level, WrapperDepth),
	prolog_frame_attribute(Frame, goal, Goal0),
	prolog_frame_attribute(Frame, level, Depth0),
	Depth is Depth0 - WrapperDepth - 1,
	unqualify(Goal0, Module, Goal),
	debug(trace, '[~d] ~w: Goal ~p', [Depth0, Port, Goal]),
	term_html(Goal, GoalString),
	functor(Port, PortName, _),
	pengine_input(_{type:  trace,
			port:  PortName,
			depth: Depth,
			goal:  GoalString
		       },
		      Reply),
	trace_action(Reply, Port, Frame, Action), !,
	debug(trace, 'Action: ~p --> ~p', [Reply, Action]).
user:prolog_trace_interception(Port, Frame0, _CHP, nodebug) :-
	pengine_self(_),
	prolog_frame_attribute(Frame0, goal, Goal),
	prolog_frame_attribute(Frame0, level, Depth),
	debug(trace, '[~d] ~w: Goal ~p --> NODEBUG', [Depth, Port, Goal]).

trace_action(continue, _Port, Frame, continue) :-
	pengine_self(Me),
	prolog_frame_attribute(Frame, predicate_indicator, Me:Name/Arity),
	functor(Head, Name, Arity),
	\+ pengine_io_predicate(Head), !,
	prolog_skip_level(_, very_deep),
        debug(trace, '~p', [Me:Name/Arity]).
trace_action(continue, Port, _, skip) :-
	box_enter(Port), !.
trace_action(continue, _, _, continue) :-
	prolog_skip_level(_, very_deep).
trace_action(nodebug,  _, _, nodebug).
trace_action(skip,     _, _, skip).
trace_action(retry,    _, _, retry).
trace_action(up   ,    _, _, up).
trace_action(abort,    _, _, abort).

box_enter(call).
box_enter(redo(_)).

wrapper_frame(Frame0, Frame) :-
	parent_frame(Frame0, Frame),
	prolog_frame_attribute(Frame, predicate_indicator, PI),
	debug(trace, 'Parent: ~p', [PI]),
	(   PI == swish_call/1
	->  true
	;   PI == swish_trace:swish_call/1
	), !.

parent_frame(Frame, Frame).
parent_frame(Frame, Parent) :-
	prolog_frame_attribute(Frame, parent, Parent0),
	parent_frame(Parent0, Parent).

unqualify(M:G, M, G) :- !.
unqualify(system:G, _, G) :- !.
unqualify(user:G, _, G) :- !.
unqualify(G, _, G).

term_html(Term, HTMlString) :-
	pengine_self(Pengine),
	pengine_property(Pengine, module(Module)),
	phrase(html(\term(Term,
			  [ module(Module),
			    quoted(true)
			  ])), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)).


%%	'$swish wrapper'(:Goal)
%
%	Wrap a SWISH goal in '$swish  wrapper'. This has two advantages:
%	we can detect that the tracer is   operating  on a SWISH goal by
%	inspecting the stack and we can  save/restore the debug state to
%	deal with debugging next solutions.

:- meta_predicate swish_call(0).

'$swish wrapper'(Goal) :-
	swish_call(Goal),
	deterministic(Det),
	(   tracing,
	    Det == false
	->  (   notrace,
	        debug(trace, 'Saved tracer', [])
	    ;	debug(trace, 'Restoring tracer', []),
	        trace,
		fail
	    )
	;   notrace
	).

swish_call(Goal) :-
	Goal,
	no_lco.

no_lco.

:- '$hide'(swish_call/1).
:- '$hide'(no_lco/0).

		 /*******************************
		 *	 ALLOW DEBUGGING	*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(system:trace).
sandbox:safe_primitive(system:notrace).
sandbox:safe_primitive(system:tracing).
sandbox:safe_primitive(system:deterministic(_)).
