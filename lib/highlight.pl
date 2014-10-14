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
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(prolog_colour)).
:- if(exists_source(library(helpidx))).
:- use_module(library(helpidx), [predicate/5]).
:- endif.

http:location(codemirror, swish(cm), []).

:- http_handler(codemirror(.),      http_404([]),      [id(cm_highlight)]).
:- http_handler(codemirror(change), codemirror_change, []).
:- http_handler(codemirror(tokens), codemirror_tokens, []).
:- http_handler(codemirror(leave),  codemirror_leave,  []).
:- http_handler(codemirror(info),   token_info,        []).

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
	debug(cm(change), 'Change ~p', [Change]),
	shadow_editor(Change, TB),
	apply_change(TB, Change.change),
	reply_json_dict(true).


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
	must_be(atom, UUID),
	uuid_like(UUID),
	(   Role = Change.get(role)
	->  new(Editor, source_buffer(UUID, Role))
	;   new(Editor, source_buffer(UUID))
	),
	(   debugging(text_buffer)
	->  send(Editor, open)
	;   true
	),
	asserta(current_editor(UUID, Editor)).

%%	uuid_like(+UUID) is semidet.
%
%	Do some sanity checking on  the  UUID   because  we  use it as a
%	temporary module name and thus we must be quite sure it will not
%	conflict with anything.

uuid_like(UUID) :-
	split_string(UUID, "-", "", Parts),
	maplist(string_length, Parts, [8,4,4,4,12]),
	\+ current_editor(UUID, _).

destroy_editor(UUID, Editor) :-
	must_be(atom, UUID),
	(   xref_source_id(Editor, SourceID)
	->  xref_clean(SourceID),
	    '$destroy_module'(UUID)	% temp xref module
	;   true
	),
	retractall(current_editor(UUID, Editor)),
	free(Editor).


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
%	POST  handler  that  deals   with    destruction   of  the  XPCE
%	source_buffer  associated  with  an  editor,   as  well  as  the
%	associated cross-reference information.

codemirror_leave(Request) :-
	http_read_json_dict(Request, Data, []),
	debug(cm(leave), 'Leaving editor ~p', [Data]),
	(   atom_string(UUID, Data.get(uuid))
	->  forall(current_editor(UUID, TB),
		   destroy_editor(UUID, TB))
	;   true
	),
	reply_json_dict(true).


		 /*******************************
		 *	CLASS SOURCE BUFFER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This XPCE class reuses XPCE's editor   infrastructructure  to maintain a
mirror of the user's editor. This is   not  ideal because XPCE's objects
are much more heavy weight that what is  needed for this purpos and XPCE
is  not  multi-threaded.  Eventually,  we'll  make   a  snappy  small  C
datastructure to deal with this. An alternative   might be to add insert
and delete behaviour to Prolog's memory files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(source_buffer, text_buffer,
		   "Server side buffer for web editors").

variable(uuid,		  name,		  get,	"Associated source id").
variable(role,		  {source,query}, both,	"Associated source id").
variable(file,		  name*,	  get,	"Associated file").
variable(xref_generation, int*,		  get,	"Generation of last xref").

% do not maintain undo information.
class_variable(undo_buffer_size, int, 0).

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
	    (	xref_module(TB, Module)
	    ->  xref_source(SourceId,
			    [ silent(true),
			      module(Module)
			    ])
	    ;	xref_source(SourceId, [silent(true)])
	    ),
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

%%	xref_module(+TB, -Module) is semidet.
%
%	True if we must run the cross-referencing in Module. Now, we use
%	a single module. Eventually, we should use multiple modules from
%	a pool.

xref_module(TB, Module) :-
	get(TB, uuid, Module),
	(   module_property(foobar, class(temporary))
	->  true
	;   set_module(Module:class(temporary)),
	    add_import_module(Module, swish, start)
	).

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
%	HTTP POST handler that returns an array of tokens for the given
%	editor.

codemirror_tokens(Request) :-
	http_read_json_dict(Request, Data, []),
	debug(cm(tokens), 'Asking for tokens: ~p', [Data]),
	(   shadow_editor(Data, TB)
	->  enriched_tokens(TB, Data, Tokens)
	;   Tokens = [[]]
	),
	reply_json_dict(json{tokens:Tokens}, [width(0)]).

enriched_tokens(TB, _Data, Tokens) :-		% source window
	get(TB, role, source), !,
	send(TB, xref_source),
	server_tokens(TB, Tokens).
enriched_tokens(TB, Data, Tokens) :-		% query window
	atom_string(SourceID, Data.get(sourceID)),
	current_editor(SourceID, SourceTB),
	xref_source_id(SourceTB, XRefID), !,
	get(TB, contents, string(Query)),
	prolog_colourise_query(Query, XRefID, colour_item(TB)),
	collect_tokens(TB, Tokens).
enriched_tokens(TB, _Data, Tokens) :-
	get(TB, contents, string(Query)),
	prolog_colourise_query(Query, swish, colour_item(TB)),
	collect_tokens(TB, Tokens).

shadow_editor(Data, TB) :-
	Text = Data.get(text), !,
	atom_string(UUID, Data.uuid),
	create_editor(UUID, TB, Data),
	send(TB, contents, string(Text)).
shadow_editor(Data, TB) :-
	_{role:_} :< Data, !,
	atom_string(UUID, Data.uuid),
	create_editor(UUID, TB, Data).
shadow_editor(Data, TB) :-
	atom_string(UUID, Data.get(uuid)), !,
	current_editor(UUID, TB).


:- thread_local
	token/3.

%%	show_mirror(+Role) is det.
%%	server_tokens(+Role) is det.
%
%	These predicates help debugging the   server side. show_mirror/0
%	opens the XPCE editor,  which   simplifies  validation  that the
%	server  copy  is  in  sync  with    the  client.  The  predicate
%	server_tokens/1 dumps the token list.
%
%	@arg	Role is one of =source= or =query=, expressing the role of
%		the editor in the SWISH UI.

show_mirror(Role) :-
	current_editor(_UUID, TB),
	get(TB, role, Role), !,
	send(TB, open).

server_tokens(Role) :-
	current_editor(_UUID, TB),
	get(TB, role, Role), !,
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
	collect_tokens(TB, GroupedTokens).

collect_tokens(TB, GroupedTokens) :-
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
	(   ends_term(H.get(type))
	->  T = [],
	    R = T0
	;   take_term(T0, T, R)
	).

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

json_token(TB, Start, Token) :-
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
	),
	dict_create(Token, json, [type(Type)|Attrs]).

atomic_special(atom, Start, Len, TB, Type, Attrs) :-
	(   get(TB, character, Start, 0'\')
	->  Type = qatom,
	    Attrs = []
	;   Type = atom,
	    (   Len =< 5			% solo characters, neck, etc.
	    ->  get(TB, contents, Start, Len, string(Text)),
	        Attrs = [text(#(Text))]
	    ;   Attrs = []
	    )
	).

json_attributes([], [], _, _, _).
json_attributes([H0|T0], [H|T], TB, Start, Len) :-
	json_attribute(H0, H, TB, Start, Len), !,
	json_attributes(T0, T, TB, Start, Len).
json_attributes([_|T0], T, TB, Start, Len) :-
	json_attributes(T0, T, TB, Start, Len).


json_attribute(text, text(#(Text)), TB, Start, Len) :- !,
	get(TB, contents, Start, Len, string(Text)).
json_attribute(Term, Term, _, _, _).

colour_item(_TB, Style, Start, Len) :-
	(   style(Style)
	->  assertz(token(Style, Start, Len))
	;   debug(color, 'Ignored ~q.', [token(Style, Start, Len)])
	).

%%	style(+StyleIn) is semidet.
%%	style(+StyleIn, -SWISHType:atomOrPair, -Attributes:list)
%
%	Declare    that    we    map    StyleIn    as    generated    by
%	library(prolog_colour) into a token of type SWISHType, providing
%	additional context information based on  Attributes. Elements of
%	Attributes are terms of the form Name(Value) or the atom =text=.
%	The latter is mapped to text(String),  where String contains the
%	text that matches the token character range.
%
%	The  resulting  JSON  token  object    has  a  property  =type=,
%	containing  the  SWISHType  and  the    properties   defined  by
%	Attributes.
%
%	Additional translations can be defined by   adding rules for the
%	multifile predicate swish:style/3. The base   type, which refers
%	to the type generated by the   SWISH tokenizer must be specified
%	by adding an  attribute  base(BaseType).   For  example,  if the
%	colour system classifies an  atom  as   refering  to  a database
%	column, library(prolog_colour) may emit  db_column(Name) and the
%	following rule should ensure consistent mapping:
%
%	  ==
%	  swish_highlight:style(db_column(Name),
%				db_column, [text, base(atom)]).
%	  ==

:- multifile
	style/3.

style(Style) :-
	style(Style, _, _).

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
goal_type(autoload(File),     goal_autoload,	 [file(File)]).
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

		 /*******************************
		 *	 HIGHLIGHT CONFIG	*
		 *******************************/

:- multifile
	swish_config:config/2,
	css/3.				% ?Context, ?Selector, -Attributes

%%	swish_config:config(-Name, -Styles) is det.
%
%	Provides the object `config.swish.style`,  a   JSON  object that
%	maps   style   properties   of    user-defined   extensions   of
%	library(prolog_colour). This info is  used   by  the server-side
%	colour engine to populate the CodeMirror styles.
%
%	@tbd	Provide summary information

swish_config:config(cm_style, Styles) :-
	findall(Name-Style, highlight_style(Name, Style), Pairs),
	keysort(Pairs, Sorted),
	remove_duplicate_styles(Sorted, Unique),
	dict_pairs(Styles, json, Unique).
swish_config:config(cm_hover_style, Styles) :-
	findall(Sel-Attrs, css_dict(hover, Sel, Attrs), Pairs),
	dict_pairs(Styles, json, Pairs).

remove_duplicate_styles([], []).
remove_duplicate_styles([H|T0], [H|T]) :-
	H = K-_,
	remove_same(K, T0, T1),
	remove_duplicate_styles(T1, T).

remove_same(K, [K-_|T0], T) :- !,
	remove_same(K, T0, T).
remove_same(_, Rest, Rest).

highlight_style(StyleName, Style) :-
	style(Term, StyleName, _),
	atom(StyleName),
	(   prolog_colour:style(Term, Attrs0)
        ->  maplist(css_style, Attrs0, Attrs),
	    dict_create(Style, json, Attrs)
	).

css_style(bold(true),      'font-weight'(bold)) :- !.
css_style(underline(true), 'text-decoration'(underline)) :- !.
css_style(colour(Name), color(RGB)) :-
	current_prolog_flag(gui, true), !,
	get(colour(Name), red,   R0),
	get(colour(Name), green, G0),
	get(colour(Name), blue,  B0),
	R is R0//256,
	G is G0//256,
	B is B0//256,
	format(atom(RGB), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+', [R,G,B]).
css_style(Style, Style).

%%	css(?Context, ?Selector, -Style) is nondet.
%
%	Multifile hook to define additional style to apply in a specific
%	context.  Currently defined contexts are:
%
%	  - hover
%	  Used for CodeMirror hover extension.
%
%	@arg Selector is a CSS selector, which is refined by Context
%	@arg Style is a list of Name(Value) terms.

css_dict(Context, Selector, Style) :-
	css(Context, Selector, Attrs0),
	maplist(css_style, Attrs0, Attrs),
	dict_create(Style, json, Attrs).


		 /*******************************
		 *	       INFO		*
		 *******************************/

:- multifile
	prolog:predicate_summary/2.

%%	token_info(+Request)
%
%	HTTP handler that provides information  about a token.

token_info(Request) :-
	http_parameters(Request, [], [form_data(Form)]),
	maplist(type_convert, Form, Values),
	dict_create(Token, token, Values),
	reply_html_page(plain,
			title('token info'),
			\token_info_or_none(Token)).

type_convert(Name=Atom, Name=Number) :-
	atom_number(Atom, Number), !.
type_convert(NameValue, NameValue).


token_info_or_none(Token) -->
	token_info(Token), !.
token_info_or_none(_) -->
	html(span(class('token-noinfo'), 'No info available')).

%%	token_info(+Token:dict)// is det.
%
%	Generate HTML, providing details about Token.   Token is a dict,
%	providing  the  enriched  token  as  defined  by  style/3.  This
%	multifile non-terminal can be hooked to provide details for user
%	defined style extensions.

:- multifile token_info//1.

token_info(Token) -->
	{ _{type:Type, text:Name, arity:Arity} :< Token,
	  goal_type(_, Type, _), !,
	  ignore(token_predicate_module(Token, Module)),
	  predicate_info(Module:Name/Arity, Info)
	},
	pred_info(Info).

pred_info([]) -->
	html(span(class('pred-nosummary'), 'No help available')).
pred_info([Info|_]) -->			% TBD: Ambiguous
	(pred_tags(Info)     -> [];[]),
	(pred_summary(Info)  -> [];[]).

pred_tags(Info) -->
	{ Info.get(iso) == true },
	html(span(class('pred-tag'), 'ISO')).

pred_summary(Info) -->
	html(span(class('pred-summary'), Info.get(summary))).


%%	token_predicate_module(+Token, -Module) is semidet.
%
%	Try to extract the module from the token.

token_predicate_module(Token, Module) :-
	source_file_property(Token.get(file), module(Module)), !.

%%	predicate_info(+PI, -Info:list(dict)) is det.
%
%	Info is a list of dicts providing details about predicates that
%	match PI.  Fields in dict are:
%
%	  - module:Atom
%	  Module of the predicate
%	  - name:Atom
%	  Name of the predicate
%	  - arity:Integer
%	  Arity of the predicate
%	  - summary:Text
%	  Summary text extracted from the system manual or PlDoc
%	  - iso:Boolean
%	  Presend and =true= if the predicate is an ISO predicate

predicate_info(PI, Info) :-
	PI = Module:Name/Arity,
	findall(Dict,
		( setof(Key-Value,
			predicate_info(PI, Key, Value),
			Pairs),
		  dict_pairs(Dict, json,
			     [ module - Module,
			       name   - Name,
			       arity  - Arity
			     | Pairs
			     ])
		),
		Info).

%%	predicate_info(?PI, -Key, -Value) is nondet.
%
%	Find information about predicates from   the  system, manual and
%	PlDoc. First, we  deal  with  ISO   predicates  that  cannot  be
%	redefined and are documented in the   manual. Next, we deal with
%	predicates that are documented in  the   manual.
%
%	@bug: Handling predicates documented  in   the  manual  is buggy
%	because their definition may  be  overruled   by  the  user.  We
%	probably must include the file into the equation.

					% ISO predicates
predicate_info(Module:Name/Arity, Key, Value) :-
	compound_name_arity(Head, Name, Arity),
	predicate_property(system:Head, iso), !,
	ignore(Module = system),
	(   catch(predicate(Name, Arity, Summary, _, _), _, fail),
	    Key = summary,
	    Value = Summary
	;   Key = iso,
	    Value = true
	).
predicate_info(_Module:Name/Arity, summary, Summary) :-
	catch(predicate(Name, Arity, Summary, _, _), _, fail), !.
predicate_info(PI, summary, Summary) :-	% PlDoc
	prolog:predicate_summary(PI, Summary).
