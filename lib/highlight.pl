/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(swish_highlight,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(prolog_colour)).
:- use_module(library(pairs)).

http:location(codemirror, swish(cm), []).

:- http_handler(codemirror(.),      http_404([]),      [id(cm_highlight)]).
:- http_handler(codemirror(change), codemirror_change, []).
:- http_handler(codemirror(tokens), codemirror_tokens, []).
:- http_handler(codemirror(leave),  codemirror_leave,  []).

/** <module> Highlight token server

This module provides the Prolog part of server-assisted highlighting for
SWISH. It is implemented by managing a  shadow copy of the client editor
on the server. On request,  the  server   computes  a  list of _semantic
tokens_.

@tbd	Use websockets
*/

		 /*******************************
		 *	  SHADOW EDITOR		*
		 *******************************/

%%	codemirror_change(+Request)
%
%	Handle changes to the codemirror instances. These are sent to us
%	using  a  POST  request.  The  request   a  POSTed  JSON  object
%	containing:
%
%	  - uuid: string holding the editor's UUID
%	  - change: the change object, which holds:
%	    - from: Start position as {line:Line, ch:Ch}
%	    - to: End position
%	    - removed: list(atom) of removed text
%	    - text: list(atom) of inserted text
%	    - origin: what caused this change event
%	    - next: optional next change event.

codemirror_change(Request) :-
	http_read_json_dict(Request, Change, []),
	debug(codemirror, 'Change ~p', [Change]),
	(   atom_string(UUID, Change.get(uuid))
	->  current_editor(UUID, TextBuffer),
	    Reply = true
	;   create_editor(UUID, TextBuffer, Change),
	    Reply = json{uuid:UUID}
	),
	apply_change(TextBuffer, Change.change),
	reply_json_dict(Reply).


apply_change(_, []) :- !.
apply_change(TB, Change) :-
	_{from:From} :< Change,
	get(TB, scan, 0, line, From.line, start, SOL),
	ChPos is SOL+From.ch,
	remove(Change.removed, TB, ChPos),
	insert(Change.text, TB, ChPos, End),
	send(TB, caret, End),
	(   Next = Change.get(next)
	->  apply_change(TB, Next)
	;   true
	).

remove([], _, _) :- !.
remove([H|T], TB, ChPos) :-
	atom_length(H, Len),
	(   T == []
	->  DLen is Len
	;   DLen is Len+1
	),
	(   DLen == 0
	->  true
	;   send(TB, delete, ChPos, DLen)
	),
	remove(T, TB, ChPos).

insert([], _, ChPos, ChPos) :- !.
insert([H|T], TB, ChPos0, ChPos) :-
	atom_length(H, Len),
	(   T == []
	->  Fmt = '%s',
	    ChPos1 = ChPos0+Len
	;   Fmt = '%s\n',
	    ChPos1 is ChPos0+Len+1
	),
	send(TB, insert, ChPos0, string(Fmt, H), 1),
	insert(T, TB, ChPos1, ChPos).

:- dynamic
	current_editor/2.

create_editor(UUID, Editor, Change) :-
	uuid(UUID),
	(   Role = Change.get(role)
	->  new(Editor, source_buffer(UUID, Role))
	;   new(Editor, source_buffer(UUID))
	),
	(   debugging(text_buffer)
	->  send(Editor, open)
	;   true
	),
	asserta(current_editor(UUID, Editor)).


:- multifile
	prolog:xref_source_identifier/2,
	prolog:xref_open_source/2.

prolog:xref_source_identifier(UUID, UUID) :-
	current_editor(UUID, _).

prolog:xref_open_source(UUID, Stream) :-
	current_editor(UUID, TB), !,
	pce_open(TB, read, Stream).


%%	codemirror_leave(+Request)
%
%	Handle deletion of the client

codemirror_leave(Request) :-
	http_read_json(Request, JSON, []),
	debug(codemirror, 'Leaving editor ~q', [JSON]),
	JSON = json([uuid=UUID]),
	forall(retract(current_editor(UUID, TB)),
	       send(TB, free)),
	reply_json(@true).


		 /*******************************
		 *	CLASS SOURCE BUFFER	*
		 *******************************/

:- pce_begin_class(source_buffer, text_buffer,
		   "Server side buffer for web editors").

variable(uuid,		  name,		  get,	"Associated source id").
variable(role,		  {source,query}, both,	"Associated source id").
variable(file,		  name*,	  get,	"Associated file").
variable(xref_generation, int*,		  get,	"Generation of last xref").

initialise(TB, UUID:uuid=name, Role:role=[{source,query}]) :->
	"Create from UUID"::
	send_super(TB, initialise),
	send(TB, slot, uuid, UUID),
	default(Role, source, TheRole),
	send(TB, slot, role, TheRole).

open(TB) :->
	"Open graphical window (for debugging)"::
	in_pce_thread(show_text_buffer(TB)).

caret(TB, Pos:int) :->
	"Provide feedback on caret location"::
	get(TB, editors, Editors),
	(   send(Editors, empty)
	->  true
	;   in_pce_thread(caret_editors(Editors, Pos))
	).

caret_editors(Editors, Caret) :-
	send(Editors, for_all, message(@arg1, caret, Caret)).

show_text_buffer(TextBuffer) :-
	new(E, editor(TextBuffer)),
	send(view(editor:=E), open).

xref_source(TB, Always:[bool]) :->
	"Run the cross-referencer on buffer"::
	get(TB, generation, G),
	(   (   Always == @on
	    ->  true
	    ;   get(TB, xref_generation, GRef),
		GRef \== G
	    )
	->  xref_source_id(TB, SourceId),
	    (	TB == SourceId
	    ->	true
	    ;	send(TB, attribute, xref_source_id, SourceId)
	    ),
	    xref_source(SourceId, [silent(true)]),
	    send(TB, slot, xref_generation, G)
	;   true
	).

xref_source_id(M, SourceId:any) :<-
	"Xref source identifier"::
	get(M, text_buffer, TB),
	(   get(TB, attribute, xref_source_id, SourceId)
	->  true
	;   SourceId = TB
	).

%%	xref_source_id(+TextBuffer, -SourceID) is det.
%
%	Find the object we need  to   examine  for cross-referencing. If
%	this is an included file, this is the corresponding main file.

xref_source_id(TB, SourceId) :-
	get(TB, file, File), File \== @nil, !,
	get(File, absolute_path, Path0),
	absolute_file_name(Path0, Path),
	master_load_file(Path, [], Master),
	(   Master == Path
	->  SourceId = TB
	;   SourceId = Master
	).
xref_source_id(TB, SourceId) :-
	get(TB, uuid, SourceId).

%%	master_load_file(+File, +Seen, -MasterFile) is det.
%
%	If file is included into another  file, find the outermost file.
%	This is the file that needs to  be reloaded instead of reloading
%	File.

master_load_file(File0, Seen, File) :-
	source_file_property(File0, included_in(File1, _Line)),
	\+ memberchk(File1, Seen), !,
	master_load_file(File1, [File0|Seen], File).
master_load_file(File, _, File).

:- pce_end_class.


		 /*******************************
		 *	  SERVER TOKENS		*
		 *******************************/

%%	codemirror_tokens(+Request)
%
%	HTTP handler that returns  an  array   of  tokens  for the given
%	editor.

codemirror_tokens(Request) :-
	http_parameters(Request,
			[ uuid(UUID, [])
			]),
	(   current_editor(UUID, TB)
	->  (   get(TB, role, source)
	    ->	send(TB, xref_source)
	    ;	true
	    ),
	    server_tokens(TB, Tokens)
	;   Tokens = [[]]
	),
	reply_json(Tokens, [width(0)]).


:- thread_local
	token/3.

%%	show_mirror is det.
%%	server_tokens is det.
%
%	These predicates help debugging the   server side. show_mirror/0
%	opens the XPCE editor,  which   simplifies  validation  that the
%	server  copy  is  in  sync  with    the  client.  The  predicate
%	server_tokens/0 dumps the token list.

show_mirror :-
	once(current_editor(_UUID, TB)),
	send(TB, open).

server_tokens :-
	once(current_editor(_UUID, TB)),
	server_tokens(TB, Tokens),
	print_term(Tokens, [output(user_error)]).

%%	server_tokens(+TextBuffer, -Tokens) is det.
%
%	@arg	Tokens is a nested list of Prolog JSON terms.  Each group
%		represents the tokens found in a single toplevel term.

server_tokens(TB, GroupedTokens) :-
	get(TB, uuid, UUID),
	setup_call_cleanup(
	    pce_open(TB, read, Stream),
	    ( set_stream_file(TB, Stream),
	      prolog_colourise_stream(Stream, UUID, colour_item(TB))
	    ),
	    close(Stream)),
	findall(Start-Token, json_token(TB, Start, Token), Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, Tokens),
	group_by_term(Tokens, GroupedTokens).

set_stream_file(_,_).			% TBD

%%	group_by_term(+Tokens, -Nested) is det.
%
%	Group the tokens by  input   term.  This  simplifies incremental
%	updates of the token  list  at  the   client  sides  as  well as
%	re-syncronizing. This predicate relies on   the `fullstop` token
%	that is emitted at the end of each input term.

group_by_term([], []) :- !.
group_by_term(Flat, [Term|Grouped]) :-
	take_term(Flat, Term, Rest),
	group_by_term(Rest, Grouped).

take_term([], [], []).
take_term([H|T0], [H|T], R) :-
	(   token_type(H, Type),
	    ends_term(Type)
	->  T = [],
	    R = T0
	;   take_term(T0, T, R)
	).

token_type(json([type(Type)|_]), Type).

ends_term(fullstop).
ends_term(syntax_error).

%%	json_token(+TB, -Start, -JSON) is nondet.
%
%	Extract the stored terms.
%
%	@tbd	We could consider to collect the attributes in the
%		colour_item/4 callback and maintain a global variable
%		instead of using assert/retract.  Most likely that would
%		be faster.  Need to profile to check the bottleneck.

json_token(TB, Start, json([type(Type)|Attrs])) :-
	retract(token(Style, Start0, Len)),
	debug(color, 'Trapped ~q.', [token(Style, Start0, Len)]),
	(   atomic_special(Style, Start0, Len, TB, Type, Attrs)
	->  Start = Start0
	;   style(Style, Type0, Attrs0)
	->  (   Type0 = StartType-EndType
	    ->	(   Start = Start0,
		    Type  = StartType
		;   Start is Start0+Len-1,
		    Type  = EndType
		)
	    ;	Type = Type0,
		Start = Start0
	    ),
	    json_attributes(Attrs0, Attrs, TB, Start0, Len)
	).

atomic_special(atom, Start, Len, TB, Type, Attrs) :-
	(   get(TB, character, Start, 0'\')
	->  Type = qatom,
	    Attrs = []
	;   Type = atom,
	    (   Len =< 5			% solo characters, neck, etc.
	    ->  get(TB, contents, Start, Len, string(Text)),
	        Attrs = [text(Text)]
	    ;   Attrs = []
	    )
	).

json_attributes([], [], _, _, _).
json_attributes([H0|T0], [H|T], TB, Start, Len) :-
	json_attribute(H0, H, TB, Start, Len), !,
	json_attributes(T0, T, TB, Start, Len).
json_attributes([_|T0], T, TB, Start, Len) :-
	json_attributes(T0, T, TB, Start, Len).


json_attribute(text, text(Text), TB, Start, Len) :- !,
	get(TB, contents, Start, Len, string(Text)).
json_attribute(Term, Term, _, _, _).

colour_item(_TB, Style, Start, Len) :-
	(   style(Style)
	->  assertz(token(Style, Start, Len))
	;   debug(color, 'Ignored ~q.', [token(Style, Start, Len)])
	).

term_expansion(style(Type, Style, Attrs),
	       [ style(Type, Style, Attrs),
		 style(Type)
	       ]).
term_expansion((style(Type, Style, Attrs) :- Body),
	       [ (style(Type, Style, Attrs) :- Body),
		 style(Type)
	       ]).

:- discontiguous style/3, style/1.

style(neck(Neck),     neck, [ text(Text) ]) :-
	neck_text(Neck, Text).
style(head(Class, Head), Type, [ text, arity(Arity) ]) :-
	goal_arity(Head, Arity),
	head_type(Class, Type).
style(goal(Class, Goal), Type, [ text, arity(Arity) | More ]) :-
	goal_arity(Goal, Arity),
	goal_type(Class, Type, More).
style(file_no_depend(Path), file_no_depends,		   [text, path(Path)]).
style(file(Path),	 file,				   [text, path(Path)]).
style(nofile,		 nofile,			   [text]).
style(option_name,	 option_name,			   [text]).
style(no_option_name,	 no_option_name,		   [text]).
style(flag_name(_Flag),	 flag_name,			   [text]).
style(no_flag_name(_Flag), no_flag_name,		   [text]).
style(fullstop,		 fullstop,			   []).
style(var,		 var,				   [text]).
style(singleton,	 singleton,			   [text]).
style(string,		 string,			   []).
style(codes,		 codes,				   []).
style(chars,		 chars,				   []).
style(atom,		 atom,				   []).
style(meta(_Spec),	 meta,				   []).
style(op_type(_Type),	 op_type,			   [text]).
style(functor,		 functor,			   [text]).
style(control,		 control,			   [text]).
style(identifier,	 identifier,			   [text]).
style(module(_Module),   module,			   [text]).
style(error,		 error,				   [text]).
style(type_error(_Expect), error,			   [text]).
style(syntax_error(_Msg,_Pos), syntax_error,		   []).
style(predicate_indicator, atom,			   [text]).
style(predicate_indicator, atom,			   [text]).
style(arity,		 int,				   []).
style(int,		 int,				   []).
style(float,		 float,				   []).
style(qq(open),		 qq_open,			   []).
style(qq(sep),		 qq_sep,			   []).
style(qq(close),	 qq_close,			   []).
style(qq_type,		 qq_type,			   [text]).
style(dict_tag,		 tag,				   [text]).
style(dict_key,		 key,				   [text]).
style(dict_sep,		 sep,				   []).
style(func_dot,		 atom,				   [text(.)]).
style(dict_return_op,	 atom,				   [text(:=)]).
style(dict_function(F),  dict_function,			   [text(F)]).
style(empty_list,	 list_open-list_close,		   []).
style(list,		 list_open-list_close,		   []).
style(dcg(terminal),	 list_open-list_close,		   []).
style(dcg(plain),	 brace_term_open-brace_term_close, []).
style(brace_term,	 brace_term_open-brace_term_close, []).
style(dict_content,	 dict_open-dict_close,             []).
style(expanded,		 expanded,			   [text]).
style(comment_string,	 comment_string,		   []).
					% from library(http/html_write)
style(html(_Element),	 html,				   []).
style(entity(_Element),	 entity,			   []).
style(html_attribute(_), html_attribute,		   []).
style(sgml_attr_function,sgml_attr_function,		   []).
style(http_location_for_id(_), http_location_for_id,       []).
style(http_no_location_for_id(_), http_no_location_for_id, []).
					% XPCE support
style(method(send),	 xpce_method,			   [text]).
style(method(get),	 xpce_method,			   [text]).
style(class(built_in,_Name),	  xpce_class_built_in,	   [text]).
style(class(library(File),_Name), xpce_class_lib,	   [text, file(File)]).
style(class(user(File),_Name),	  xpce_class_user,	   [text, file(File)]).
style(class(user,_Name),	  xpce_class_user,	   [text]).
style(class(undefined,_Name),	  xpce_class_undef,	   [text]).

neck_text(clause,       (:-)).
neck_text(grammar_rule, (-->)).
neck_text(method(send), (:->)).
neck_text(method(get),  (:<-)).
neck_text(directive,    (:-)).

head_type(exported,	head_exported).
head_type(public(_),	head_public).
head_type(extern(_),	head_extern).
head_type(dynamic,	head_dynamic).
head_type(multifile,	head_multifile).
head_type(unreferenced,	head_unreferenced).
head_type(hook,		head_hook).
head_type(meta,		head_meta).
head_type(constraint,	head_constraint).
head_type(imported,	head_imported).
head_type(built_in,	head_built_in).
head_type(iso,		head_iso).
head_type(def_iso,	head_def_iso).
head_type(def_swi,	head_def_swi).
head_type(_,		head).

goal_type(built_in,	      goal_built_in,	 []).
goal_type(imported(File),     goal_imported,	 [file(File)]).
goal_type(autoload,	      goal_autoload,	 []).
goal_type(global,	      goal_global,	 []).
goal_type(undefined,	      goal_undefined,	 []).
goal_type(thread_local(Line), goal_thread_local, [line(Line)]).
goal_type(dynamic(Line),      goal_dynamic,	 [line(Line)]).
goal_type(multifile(Line),    goal_multifile,	 [line(Line)]).
goal_type(expanded,	      goal_expanded,	 []).
goal_type(extern(_),	      goal_extern,	 []).
goal_type(recursion,	      goal_recursion,	 []).
goal_type(meta,		      goal_meta,	 []).
goal_type(foreign(_),	      goal_foreign,	 []).
goal_type(local(Line),	      goal_local,	 [line(Line)]).
goal_type(constraint(Line),   goal_constraint,	 [line(Line)]).
goal_type(not_callable,	      goal_not_callable, []).

%%	goal_arity(+Goal, -Arity) is det.
%
%	Get the arity of a goal safely in SWI7

goal_arity(Goal, Arity) :-
	(   compound(Goal)
	->  compound_name_arity(Goal, _, Arity)
	;   Arity = 0
	).
