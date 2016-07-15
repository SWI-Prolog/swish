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
/**/
	  [ '$swish wrapper'/1		% +Goal
/*
	  [ '$swish wrapper'/2		% +Goal, -Residuals
*/
	  ]).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(library(pengines)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(solution_sequences)).
:- use_module(library(edinburgh), [debug/0]).
:- use_module(library(pengines_io), [pengine_io_predicate/1]).
:- use_module(library(sandbox), []).
:- use_module(library(prolog_clause)).
:- use_module(library(prolog_breakpoints)).
:- use_module(library(http/term_html)).
:- use_module(library(http/html_write)).

:- use_module(storage).

:- if(current_setting(swish:debug_info)).
:- set_setting(swish:debug_info, true).
:- endif.

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
/**/
	'$swish wrapper'(0).
/*
	'$swish wrapper'(0, -).
*/

/** <module>

Allow tracing pengine execution under SWISH.
*/

:- multifile
	user:prolog_trace_interception/4,
	user:message_hook/3.

user:message_hook(trace_mode(_), _, _) :-
	pengine_self(_), !.

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
	Prompt0 = _{type:    trace,
		    port:    PortName,
		    depth:   Depth,
		    goal:    GoalString,
		    pengine: Pengine
		   },
	add_context(Port, Frame, Prompt0, Prompt1),
	add_source(Port, Frame, Prompt1, Prompt),
	pengine_input(Prompt, Reply),
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
trace_action(nodebug(Breakpoints), _, _, Action) :-
	catch(update_breakpoints(Breakpoints), E,
	      print_message(warning, E)),
	(   Breakpoints == []
	->  Action = nodebug
	;   Action = continue,
	    notrace
	).

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

%%	add_context(+Port, +Frame, +Prompt0, -Prompt) is det.
%
%	Add additional information  about  the   context  to  the  debug
%	prompt.

add_context(exception(Exception0), _Frame, Prompt0, Prompt) :-
	strip_stack(Exception0, Exception),
	message_to_string(Exception, Msg), !,
	debug(trace, 'Msg = ~s', [Msg]),
	(   term_html(Exception, String)
	->  Ex = json{term_html:String, message:Msg}
	;   Ex = json{message:Msg}
	),
	Prompt = Prompt0.put(exception, Ex).
add_context(_, _, Prompt, Prompt).

strip_stack(error(Error, context(prolog_stack(S), Msg)),
	    error(Error, context(_, Msg))) :-
	nonvar(S).
strip_stack(Error, Error).

/*
%%	'$swish wrapper'(:Goal, -Residuals)
*/

%%	'$swish wrapper'(:Goal)
%
%	Wrap a SWISH goal in '$swish  wrapper'. This has two advantages:
%	we can detect that the tracer is   operating  on a SWISH goal by
%	inspecting the stack and we can  save/restore the debug state to
%	deal with debugging next solutions.

:- meta_predicate swish_call(0).

/**/
'$swish wrapper'(Goal) :-
	catch(swish_call(Goal), E, throw(E)),
	deterministic(Det),
/*
'$swish wrapper'(Goal, '$residuals'(Residuals)) :-
	catch(swish_call(Goal), E, throw(E)),
	deterministic(Det),
	Goal = M:_,
	residuals(M, Residuals),
*/
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


/*
%%	residuals(+PengineModule, -Goals:list(callable)) is det.
%
%	Find residual goals  that  are  not   bound  to  the  projection
%	variables. We must do so while  we   are  in  the Pengine as the
%	goals typically live in global variables   that  are not visible
%	when formulating the answer  from   the  projection variables as
%	done in library(pengines_io).
%
%	This relies on the SWI-Prolog 7.3.14 residual goal extension.

:- if(current_predicate(prolog:residual_goals//0)).
residuals(TypeIn, Goals) :-
	phrase(prolog:residual_goals, Goals0),
	maplist(unqualify_residual(TypeIn), Goals0, Goals).

unqualify_residual(M, M:G, G) :- !.
unqualify_residual(T, M:G, G) :-
	predicate_property(T:G, imported_from(M)), !.
unqualify_residual(_, G, G).
:- else.
residuals(_, []).
:- endif.


*/
		 /*******************************
		 *	  SOURCE LOCATION	*
		 *******************************/

add_source(Port, Frame, Prompt0, Prompt) :-
	debug(trace(line), 'Add source?', []),
	source_location(Frame, Port, Location), !,
	Prompt = Prompt0.put(source, Location),
	debug(trace(line), 'Source ~p ~p: ~p', [Port, Frame, Location]).
add_source(_, _, Prompt, Prompt).

%%	source_location(+Frame, +Port, -Location) is semidet.
%
%	Determine the appropriate location to show for Frame at Port.
%
%	  1. If we have a PC (integer), we have a concrete
%	  clause-location, so use it if it is in the current file.
%	  2. If we have a port, but the parent is not associated
%	  with our file, use it.  This ensures that the initial
%	  query is shown in the source window.

source_location(Frame, Port, Location) :-
	parent_frame(Frame, Port, _Steps, ShowFrame, PC),
	(   clause_position(PC)
	->  true			% real PC
	;   prolog_frame_attribute(ShowFrame, parent, Parent),
	    frame_file(Parent, ParentFile),
	    \+ pengine_file(ParentFile)
	),
	(   debugging(trace(file))
	->  prolog_frame_attribute(ShowFrame, level, Level),
	    prolog_frame_attribute(ShowFrame, predicate_indicator, PI),
	    debug(trace(file), '\t[~d]: ~p', [Level, PI])
	;   true
	),
	frame_file(ShowFrame, File),
	pengine_file(File), !,
	source_position(ShowFrame, PC, Location).

%%	parent_frame(+FrameIn, +PCOrPortIn, -Steps,
%%		     -FrameOut, -PCOrPortOut) is nondet.
%
%	True  when  FrameOut/PCOrPortOut  is  a  parent  environment  of
%	FrameIn/PCOrPortIn. Backtracking yields higher frames.

parent_frame(Frame0, Port0, Steps, Frame, Port) :-
	parent_frame(Frame0, Port0, 0, Steps, Frame, Port).

parent_frame(Frame, Port, Steps, Steps, Frame, Port).
parent_frame(Frame, _Port, Steps0, Steps, Parent, PC) :-
	direct_parent_frame(Frame, DirectParent, ParentPC),
	Steps1 is Steps0+1,
	parent_frame(DirectParent, ParentPC, Steps1, Steps, Parent, PC).

direct_parent_frame(Frame, Parent, PC) :-
	prolog_frame_attribute(Frame, parent, Parent),
	prolog_frame_attribute(Frame, pc, PC).


%%	frame_file(+Frame, -File) is semidet.
%
%	True when Frame is associated with   a predicate that is defined
%	in File.

frame_file(Frame, File) :-
	prolog_frame_attribute(Frame, clause, ClauseRef), !,
	(   clause_property(ClauseRef, predicate(system:'<meta-call>'/1))
	->  prolog_frame_attribute(Frame, parent, Parent),
	    frame_file(Parent, File)
	;   clause_property(ClauseRef, file(File))
	).
frame_file(Frame, File) :-
	prolog_frame_attribute(Frame, goal, Goal),
	qualify(Goal, QGoal),
	\+ predicate_property(QGoal, foreign),
	clause(QGoal, _Body, ClauseRef), !,
	clause_property(ClauseRef, file(File)).

%%	pengine_file(+File) is semidet.
%
%	True if File is a Pengine controlled file. This is currently the
%	main file (pengine://) and (swish://) for included files.

pengine_file(File) :-
	sub_atom(File, 0, _, _, 'pengine://'), !.
pengine_file(File) :-
	sub_atom(File, 0, _, _, 'swish://').

%%	clause_position(+PC) is semidet.
%
%	True if the position can be related to a clause.

clause_position(PC) :- integer(PC), !.
clause_position(exit).
clause_position(unify).
clause_position(choice(_)).

%%	subgoal_position(+Clause, +PortOrPC,
%%			 -File, -CharA, -CharZ) is semidet.
%
%	Character  range  CharA..CharZ  in  File   is  the  location  to
%	highlight for the given clause at the given location.

subgoal_position(ClauseRef, PortOrPC, _, _, _) :-
	debugging(trace(save_pc)),
	debug(trace(save_pc), 'Position for ~p at ~p', [ClauseRef, PortOrPC]),
	asserta(subgoal_position(ClauseRef, PortOrPC)),
	fail.
subgoal_position(ClauseRef, unify, File, CharA, CharZ) :- !,
	clause_info(ClauseRef, File, TPos, _),
	head_pos(ClauseRef, TPos, PosTerm),
	nonvar(PosTerm),
	arg(1, PosTerm, CharA),
	arg(2, PosTerm, CharZ).
subgoal_position(ClauseRef, choice(CHP), File, CharA, CharZ) :- !,
	(   prolog_choice_attribute(CHP, type, jump),
	    prolog_choice_attribute(CHP, pc, To)
	->  debug(gtrace(position), 'Term-position: choice-jump to ~w', [To]),
	    subgoal_position(ClauseRef, To, File, CharA, CharZ)
	;   clause_end(ClauseRef, File, CharA, CharZ)
	).
subgoal_position(ClauseRef, Port, File, CharA, CharZ) :-
	end_port(Port), !,
	clause_end(ClauseRef, File, CharA, CharZ).
subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
	debug(trace(source), 'In clause ~p at ~p', [ClauseRef, PC]),
	clause_info(ClauseRef, File, TPos, _),
	(   '$clause_term_position'(ClauseRef, PC, List)
	->  debug(trace(source), 'Term-position: for ref=~w at PC=~w: ~w',
		  [ClauseRef, PC, List]),
	    (   find_subgoal(List, TPos, PosTerm)
	    ->  true
	    ;   PosTerm = TPos,
		debug(trace(source),
		      'Clause source-info could not be parsed', []),
		fail
	    ),
	    nonvar(PosTerm),
	    arg(1, PosTerm, CharA),
	    arg(2, PosTerm, CharZ)
	;   debug(trace(source),
		  'No clause-term-position for ref=~p at PC=~p',
		  [ClauseRef, PC]),
	    fail
	).

end_port(exit).
end_port(fail).
end_port(exception).

clause_end(ClauseRef, File, CharA, CharZ) :-
	clause_info(ClauseRef, File, TPos, _),
	nonvar(TPos),
	arg(2, TPos, CharA),
	CharZ is CharA + 1.

head_pos(Ref, Pos, HPos) :-
	clause_property(Ref, fact), !,
	HPos = Pos.
head_pos(_, term_position(_, _, _, _, [HPos,_]), HPos).

%	warning, ((a,b),c)) --> compiled to (a, (b, c))!!!  We try to correct
%	that in clause.pl.  This is work in progress.

find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
	nth1(A, PosL, Pos), !,
	find_subgoal(T, Pos, SPos).
find_subgoal([1|T], brace_term_position(_,_,Pos), SPos) :- !,
	find_subgoal(T, Pos, SPos).
find_subgoal(_, Pos, Pos).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extracted from show_source/2 from library(trace/trace)

%%	source_position(Frame, PCOrPort, -Position)
%
%	Get the source location for  Frame   at  PCOrPort. Position is a
%	dict.

source_position(Frame, PC, _{file:File, from:CharA, to:CharZ}) :-
	debug(trace(pos), '~p', [source_position(Frame, PC, _)]),
	clause_position(PC),
	prolog_frame_attribute(Frame, clause, ClauseRef), !,
	subgoal_position(ClauseRef, PC, File, CharA, CharZ).
source_position(Frame, _PC, Position) :-
	prolog_frame_attribute(Frame, goal, Goal),
	qualify(Goal, QGoal),
	\+ predicate_property(QGoal, foreign),
	(   clause(QGoal, _Body, ClauseRef)
	->  subgoal_position(ClauseRef, unify, File, CharA, CharZ),
	    Position = _{file:File, from:CharA, to:CharZ}
	;   functor(Goal, Functor, Arity),
	    functor(GoalTemplate, Functor, Arity),
	    qualify(GoalTemplate, QGoalTemplate),
	    clause(QGoalTemplate, _TBody, ClauseRef)
	->  subgoal_position(ClauseRef, unify, File, CharA, CharZ),
	    Position = _{file:File, from:CharA, to:CharZ}
	;   find_source(QGoal, File, Line),
	    debug(trace(source), 'At ~w:~d', [File, Line]),
	    Position = _{file:File, line:Line}
	).

qualify(Goal, Goal) :-
	functor(Goal, :, 2), !.
qualify(Goal, user:Goal).

find_source(Predicate, File, Line) :-
	predicate_property(Predicate, file(File)),
	predicate_property(Predicate, line_count(Line)), !.

%%	pengines:prepare_goal(+GoalIn, -GoalOut, +Options) is semidet.
%
%	Handle the breakpoints(List) option to  set breakpoints prior to
%	execution of the query. If breakpoints  are present and enabled,
%	the goal is executed in debug mode.  `List` is a list, holding a
%	dict for each source that  has   breakpoints.  The dict contains
%	these keys:
%
%	  - `file` is the source file.  For the current Pengine source
%	    this is =|pengine://<pengine>/src|=.
%	  - `breakpoints` is a list of lines (integers) where to put
%	    break points.

:- multifile pengines:prepare_goal/3.

pengines:prepare_goal(Goal0, Goal, Options) :-
	option(breakpoints(Breakpoints), Options),
	Breakpoints \== [],
	pengine_self(Pengine),
	pengine_property(Pengine, source(File, Text)),
	maplist(set_file_breakpoints(Pengine, File, Text), Breakpoints),
	Goal = (debug, Goal0).

set_file_breakpoints(_Pengine, PFile, Text, Dict) :-
	debug(trace(break), 'Set breakpoints at ~p', [Dict]),
	_{file:FileS, breakpoints:List} :< Dict,
	atom_string(File, FileS),
	(   PFile == File
	->  debug(trace(break), 'Pengine main source', []),
	    maplist(set_pengine_breakpoint(File, File, Text), List)
	;   source_file_property(PFile, includes(File, _Time)),
	    atom_concat('swish://', StoreFile, File)
	->  debug(trace(break), 'Pengine included source ~p', [StoreFile]),
	    storage_file(StoreFile, IncludedText, _Meta),
	    maplist(set_pengine_breakpoint(PFile, File, IncludedText), List)
	;   debug(trace(break), 'Not in included source', [])
	).

set_pengine_breakpoint(Owner, File, Text, Line) :-
	debug(trace(break), 'Try break at ~q:~d', [File, Line]),
	line_start(Line, Text, Char),
	(   set_breakpoint(Owner, File, Line, Char, Break)
	->  !, debug(trace(break), 'Created breakpoint ~p', [Break])
	;   print_message(warning, breakpoint(failed(File, Line, 0)))
	).

line_start(1, _, 0) :- !.
line_start(N, Text, Start) :-
	N0 is N - 2,
	offset(N0, sub_string(Text, Start, _, _, '\n')), !.

%%	update_breakpoints(+Breakpoints)
%
%	Update the active breakpoint  by  comparing   with  the  set  of
%	currently active breakpoints.

update_breakpoints(Breakpoints) :-
	breakpoint_by_file(Breakpoints, NewBPS),
	pengine_self(Pengine),
	pengine_property(Pengine, source(PFile, Text)),
	current_pengine_source_breakpoints(PFile, ByFile),
	forall(( member(File-FBPS, ByFile),
		 member(Id-Line, FBPS),
		 \+ ( member(File-NFBPS, NewBPS),
		      member(Line, NFBPS))),
	       delete_breakpoint(Id)),
	forall(( member(File-NFBPS, NewBPS),
		 member(Line, NFBPS),
		 \+ ( member(File-FBPS, ByFile),
		      member(_-Line, FBPS))),
	       add_breakpoint(PFile, File, Text, Line)).

breakpoint_by_file(Breakpoints, NewBPS) :-
	maplist(bp_by_file, Breakpoints, NewBPS).

bp_by_file(Dict, File-Lines) :-
	_{file:FileS, breakpoints:Lines} :< Dict,
	atom_string(File, FileS).

add_breakpoint(PFile, PFile, Text, Line) :- !,
	set_pengine_breakpoint(PFile, PFile, Text, Line).
add_breakpoint(PFile, File, _Text, Line) :-
	atom_concat('swish://', Store, File), !,
	storage_file(Store, Text, _Meta),
	set_pengine_breakpoint(PFile, File, Text, Line).
add_breakpoint(_, _, _, _Line).			% not in our files.

%%	current_pengine_source_breakpoints(+PengineFile, -Pairs) is det.
%
%	Find the currently set breakpoints  for   the  Pengine  with the
%	given source file PengineFile. Pairs is a list File-BreakPoints,
%	where BreakPoints is a list of breakpoint-ID - Line pairs.

current_pengine_source_breakpoints(PFile, ByFile) :-
	findall(Pair, current_pengine_breakpoint(PFile, Pair), Pairs0),
	keysort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, ByFile).

current_pengine_breakpoint(PFile, PFile-(Id-Line)) :-
	breakpoint_property(Id, file(PFile)),
	breakpoint_property(Id, line_count(Line)).
current_pengine_breakpoint(PFile, File-(Id-Line)) :-
	source_file_property(PFile, includes(File, _Time)),
	breakpoint_property(Id, file(File)),
	breakpoint_property(Id, line_count(Line)).


%%	prolog_clause:open_source(+File, -Stream) is semidet.
%
%	Open SWISH non-file sources.

:- multifile prolog_clause:open_source/2.

prolog_clause:open_source(File, Stream) :-
	sub_atom(File, 0, _, _, 'pengine://'), !,
	(   pengine_self(Pengine)
	->  true
	;   debugging(trace(_))
	),
	pengine_property(Pengine, source(File, Source)),
	open_string(Source, Stream).
prolog_clause:open_source(File, Stream) :-
	atom_concat('swish://', GittyFile, File), !,
	storage_file(GittyFile, Data, _Meta),
	open_string(Data, Stream).


		 /*******************************
		 *	 TRAP EXCEPTIONS	*
		 *******************************/

:- dynamic
	user:prolog_exception_hook/4,
	installed/1.

exception_hook(Ex, Ex, _Frame, Catcher) :-
	Catcher \== none,
	Catcher \== 'C',
	prolog_frame_attribute(Catcher, predicate_indicator, PI),
	debug(trace(exception), 'Ex: ~p, catcher: ~p', [Ex, PI]),
	PI == '$swish wrapper'/1,
	trace,
	fail.

%%	install_exception_hook
%
%	Make sure our handler is the first of the hook predicate.

install_exception_hook :-
	installed(Ref),
	(   nth_clause(_, I, Ref)
	->  I == 1, !			% Ok, we are the first
	;   retractall(installed(Ref)),
	    erase(Ref),			% Someone before us!
	    fail
	).
install_exception_hook :-
	asserta((user:prolog_exception_hook(Ex, Out, Frame, Catcher) :-
			exception_hook(Ex, Out, Frame, Catcher)), Ref),
	assert(installed(Ref)).

:- install_exception_hook.


		 /*******************************
		 *	 ALLOW DEBUGGING	*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(system:trace).
sandbox:safe_primitive(system:notrace).
sandbox:safe_primitive(system:tracing).
sandbox:safe_primitive(edinburgh:debug).
sandbox:safe_primitive(system:deterministic(_)).
/*
sandbox:safe_primitive(swish_trace:residuals(_,_)).
*/


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(breakpoint(failed(File, Line, _Char))) -->
	[ 'Failed to set breakpoint at ~w:~d'-[File,Line] ].
