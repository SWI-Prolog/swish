/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2017, VU University Amsterdam
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

:- module(swish_highlight,
	  [ current_highlight_state/2
	  ]).
:- use_module(library(debug)).
:- use_module(library(settings)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(pairs)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(prolog_xref)).
:- use_module(library(memfile)).
:- use_module(library(prolog_colour)).
:- use_module(library(lazy_lists)).
:- if(exists_source(library(helpidx))).
:- use_module(library(helpidx), [predicate/5]).
:- endif.

http:location(codemirror, swish(cm), []).

:- http_handler(codemirror(.),      http_404([]),      [id(cm_highlight)]).
:- http_handler(codemirror(change), codemirror_change, []).
:- http_handler(codemirror(tokens), codemirror_tokens, []).
:- http_handler(codemirror(leave),  codemirror_leave,  []).
:- http_handler(codemirror(info),   token_info,        []).

:- setting(swish:editor_max_idle_time, nonneg, 3600,
	   "Maximum time we keep a mirror editor around").

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
%
%	Reply is JSON and either 200 with  `true` or 409 indicating that
%	the editor is not known.

codemirror_change(Request) :-
	call_cleanup(codemirror_change_(Request),
		     check_unlocked).

codemirror_change_(Request) :-
	http_read_json_dict(Request, Change, []),
	debug(cm(change), 'Change ~p', [Change]),
	atom_string(UUID, Change.uuid),
	catch(shadow_editor(Change, TB),
	      cm(Reason), true),
	(   var(Reason)
	->  (	catch(apply_change(TB, Changed, Change.change),
		      cm(outofsync), fail)
	    ->  mark_changed(TB, Changed),
		release_editor(UUID),
		reply_json_dict(true)
	    ;	destroy_editor(UUID),
		change_failed(UUID, outofsync)
	    )
	;   change_failed(UUID, Reason)
	).

change_failed(UUID, Reason) :-
	reply_json_dict(json{ type:Reason,
			      object:UUID
			    },
			[status(409)]).


%%	apply_change(+TB, -Changed, +Changes) is det.
%
%	Note that the argument order is like this to allow for maplist.
%
%	@arg Changed is left unbound if there are no changes or unified
%	to =true= if something has changed.
%
%	@throws	cm(outofsync) if an inconsistent delete is observed.

apply_change(_, _Changed, []) :- !.
apply_change(TB, Changed, Change) :-
	_{from:From} :< Change,
	Line is From.line+1,
	memory_file_line_position(TB, Line, From.ch, ChPos),
	remove(Change.removed, TB, ChPos, Changed),
	insert(Change.text, TB, ChPos, _End, Changed),
	(   Next = Change.get(next)
	->  apply_change(TB, Changed, Next)
	;   true
	).

remove([], _, _, _) :- !.
remove([H|T], TB, ChPos, Changed) :-
	string_length(H, Len),
	(   T == []
	->  DLen is Len
	;   DLen is Len+1
	),
	(   DLen == 0
	->  true
	;   Changed = true,
	    memory_file_substring(TB, ChPos, Len, _, Text),
	    (	Text == H
	    ->	true
	    ;	throw(cm(outofsync))
	    ),
	    delete_memory_file(TB, ChPos, DLen)
	),
	remove(T, TB, ChPos, Changed).

insert([], _, ChPos, ChPos, _) :- !.
insert([H|T], TB, ChPos0, ChPos, Changed) :-
	(   H == ""
	->  Len	= 0
	;   Changed = true,
	    string_length(H, Len),
	    debug(cm(change_text), 'Insert ~q at ~d', [H, ChPos0]),
	    insert_memory_file(TB, ChPos0, H)
	),
	ChPos1 is ChPos0+Len,
	(   T == []
	->  ChPos2 = ChPos1
	;   debug(cm(change_text), 'Adding newline at ~d', [ChPos1]),
	    Changed = true,
	    insert_memory_file(TB, ChPos1, '\n'),
	    ChPos2 is ChPos1+1
	),
	insert(T, TB, ChPos2, ChPos, Changed).

:- dynamic
	current_editor/5,		% UUID, MemFile, Role, Lock, Time
	editor_last_access/2,		% UUID, Time
	xref_upto_data/1.		% UUID

%%	create_editor(+UUID, -Editor, +Change) is det.
%
%	Create a new editor for source UUID   from Change. The editor is
%	created  in  a  locked  state  and    must   be  released  using
%	release_editor/1 before it can be publically used.

create_editor(UUID, Editor, Change) :-
	must_be(atom, UUID),
	uuid_like(UUID),
	new_memory_file(Editor),
	(   RoleString = Change.get(role)
	->  atom_string(Role, RoleString)
	;   Role = source
	),
	get_time(Now),
	mutex_create(Lock),
	with_mutex(swish_create_editor,
		   register_editor(UUID, Editor, Role, Lock, Now)), !.
create_editor(UUID, Editor, _Change) :-
	fetch_editor(UUID, Editor).

% editor and lock are left to symbol-GC if this fails.
register_editor(UUID, Editor, Role, Lock, Now) :-
	\+ current_editor(UUID, _, _, _, _),
	mutex_lock(Lock),
	asserta(current_editor(UUID, Editor, Role, Lock, Now)).

%%	current_highlight_state(?UUID, -State) is nondet.
%
%	Return info on the current highlighter

current_highlight_state(UUID,
			highlight{data:Editor,
				  role:Role,
				  created:Created,
				  lock:Lock,
				  access:Access
				 }) :-
	current_editor(UUID, Editor, Role, Lock, Created),
	(   editor_last_access(Editor, Access)
	->  true
	;   Access = Created
	).


%%	uuid_like(+UUID) is semidet.
%
%	Do some sanity checking on  the  UUID   because  we  use it as a
%	temporary module name and thus we must be quite sure it will not
%	conflict with anything.

uuid_like(UUID) :-
	split_string(UUID, "-", "", Parts),
	maplist(string_length, Parts, [8,4,4,4,12]),
	\+ current_editor(UUID, _, _, _, _).

%%	destroy_editor(+UUID)
%
%	Destroy source admin UUID: the shadow  text (a memory file), the
%	XREF data and the module used  for cross-referencing. The editor
%	must  be  acquired  using  fetch_editor/2    before  it  can  be
%	destroyed.

destroy_editor(UUID) :-
	must_be(atom, UUID),
	current_editor(UUID, Editor, _, Lock, _), !,
	mutex_unlock(Lock),
	retractall(xref_upto_data(UUID)),
	retractall(editor_last_access(UUID, _)),
	(   xref_source_id(UUID, SourceID)
	->  xref_clean(SourceID),
	    destroy_state_module(UUID)
	;   true
	),
	% destroy after xref_clean/1 to make xref_source_identifier/2 work.
	retractall(current_editor(UUID, Editor, _, _, _)),
	free_memory_file(Editor).
destroy_editor(_).

%%	gc_editors
%
%	Garbage collect all editors that have   not been accessed for 60
%	minutes.
%
%	@tbd  Normally,  deleting  a  highlight    state   can  be  done
%	aggressively as it will be recreated  on demand. But, coloring a
%	query passes the UUIDs of related sources and as yet there is no
%	way to restore this. We could fix  that by replying to the query
%	colouring with the UUIDs for which we do not have sources, after
%	which the client retry the query-color request with all relevant
%	sources.

:- dynamic
	gced_editors/1.

editor_max_idle_time(Time) :-
	setting(swish:editor_max_idle_time, Time).

gc_editors :-
	get_time(Now),
	(   gced_editors(Then),
	    editor_max_idle_time(MaxIdle),
	    Now - Then < MaxIdle/3
	->  true
	;   retractall(gced_editors(_)),
	    asserta(gced_editors(Now)),
	    fail
	).
gc_editors :-
	editor_max_idle_time(MaxIdle),
	forall(garbage_editor(UUID, MaxIdle),
	       destroy_garbage_editor(UUID)).

garbage_editor(UUID, TimeOut) :-
	get_time(Now),
	current_editor(UUID, _TB, _Role, _Lock, Created),
	Now - Created > TimeOut,
	(   editor_last_access(UUID, Access)
	->  Now - Access > TimeOut
	;   true
	).

destroy_garbage_editor(UUID) :-
	fetch_editor(UUID, _TB), !,
	destroy_editor(UUID).
destroy_garbage_editor(_).

%%	fetch_editor(+UUID, -MemFile) is semidet.
%
%	Fetch existing editor for source UUID.   Update  the last access
%	time. After success, the editor is   locked and must be released
%	using release_editor/1.

fetch_editor(UUID, TB) :-
	current_editor(UUID, TB, Role, Lock, _),
	catch(mutex_lock(Lock), error(existence_error(mutex,_),_), fail),
	debug(cm(lock), 'Locked ~p', [UUID]),
	(   current_editor(UUID, TB, Role, Lock, _)
	->  update_access(UUID)
	;   mutex_unlock(Lock)
	).

release_editor(UUID) :-
	current_editor(UUID, _TB, _Role, Lock, _),
	debug(cm(lock), 'Unlocked ~p', [UUID]),
	mutex_unlock(Lock).

check_unlocked :-
	check_unlocked(unknown).

%!	check_unlocked(+Reason)
%
%	Verify that all editors locked by this thread are unlocked
%	again.

check_unlocked(Reason) :-
	thread_self(Me),
	current_editor(_UUID, _TB, _Role, Lock, _),
	mutex_property(Lock, status(locked(Me, _Count))), !,
	unlock(Me, Lock),
	print_message(error, locked(Reason, Me)),
	assertion(fail).
check_unlocked(_).

unlock(Me, Lock) :-
	mutex_property(Lock, status(locked(Me, _Count))), !,
	mutex_unlock(Lock),
	unlock(Me, Lock).
unlock(_, _).

%%	update_access(+UUID)
%
%	Update the registered last access. We only update if the time is
%	behind for more than a minute.

update_access(UUID) :-
	get_time(Now),
	(   editor_last_access(UUID, Last),
	    Now-Last < 60
	->  true
	;   retractall(editor_last_access(UUID, _)),
	    asserta(editor_last_access(UUID, Now))
	).

:- multifile
	prolog:xref_source_identifier/2,
	prolog:xref_open_source/2,
	prolog:xref_close_source/2.

prolog:xref_source_identifier(UUID, UUID) :-
	current_editor(UUID, _, _, _, _).

%%	prolog:xref_open_source(+UUID, -Stream)
%
%	Open a source. As we cannot open   the same source twice we must
%	lock  it.  As  of  7.3.32   this    can   be  done  through  the
%	prolog:xref_close_source/2 hook. In older  versions   we  get no
%	callback on the close, so we must leave the editor unlocked.

:- if(current_predicate(prolog_source:close_source/3)).
prolog:xref_open_source(UUID, Stream) :-
	fetch_editor(UUID, TB),
	open_memory_file(TB, read, Stream).

prolog:xref_close_source(UUID, Stream) :-
	release_editor(UUID),
	close(Stream).
:- else.
prolog:xref_open_source(UUID, Stream) :-
	fetch_editor(UUID, TB),
	open_memory_file(TB, read, Stream),
	release_editor(UUID).
:- endif.

%%	codemirror_leave(+Request)
%
%	POST  handler  that  deals  with    destruction  of  our  mirror
%	associated  with  an  editor,   as    well   as  the  associated
%	cross-reference information.

codemirror_leave(Request) :-
	call_cleanup(codemirror_leave_(Request),
		     check_unlocked).

codemirror_leave_(Request) :-
	http_read_json_dict(Request, Data, []),
	(   atom_string(UUID, Data.get(uuid))
	->  debug(cm(leave), 'Leaving editor ~p', [UUID]),
	    (	fetch_editor(UUID, _TB)
	    ->	destroy_editor(UUID)
	    ;	debug(cm(leave), 'No editor for ~p', [UUID])
	    )
	;   debug(cm(leave), 'No editor?? (data=~p)', [Data])
	),
	reply_json_dict(true).

%%	mark_changed(+MemFile, ?Changed) is det.
%
%	Mark that our cross-reference data might be obsolete

mark_changed(MemFile, Changed) :-
	(   Changed == true,
	    current_editor(UUID, MemFile, _Role, _, _)
	->  retractall(xref_upto_data(UUID))
	;   true
	).

%%	xref(+UUID) is det.

xref(UUID) :-
	xref_upto_data(UUID), !.
xref(UUID) :-
	setup_call_cleanup(
	    fetch_editor(UUID, _TB),
	    ( xref_source_id(UUID, SourceId),
	      xref_state_module(UUID, Module),
	      xref_source(SourceId,
			  [ silent(true),
			    module(Module)
			  ]),
	      asserta(xref_upto_data(UUID))
	    ),
	    release_editor(UUID)).

%%	xref_source_id(+Editor, -SourceID) is det.
%
%	SourceID is the xref source  identifier   for  Editor. As we are
%	using UUIDs we just use the editor.

xref_source_id(UUID, UUID).

%%	xref_state_module(+UUID, -Module) is semidet.
%
%	True if we must run the cross-referencing   in  Module. We use a
%	temporary module based on the UUID of the source.

xref_state_module(UUID, UUID) :-
	(   module_property(UUID, class(temporary))
	->  true
	;   set_module(UUID:class(temporary)),
	    add_import_module(UUID, swish, start),
	    maplist(copy_flag(UUID, swish), [var_prefix])
	).

copy_flag(Module, Application, Flag) :-
    current_prolog_flag(Application:Flag, Value), !,
    set_prolog_flag(Module:Flag, Value).
copy_flag(_, _, _).

destroy_state_module(UUID) :-
	module_property(UUID, class(temporary)), !,
	'$destroy_module'(UUID).
destroy_state_module(_).


		 /*******************************
		 *	  SERVER TOKENS		*
		 *******************************/

%%	codemirror_tokens(+Request)
%
%	HTTP POST handler that returns an array of tokens for the given
%	editor.

codemirror_tokens(Request) :-
	setup_call_catcher_cleanup(
	    true,
	    codemirror_tokens_(Request),
	    Reason,
	    check_unlocked(Reason)).

codemirror_tokens_(Request) :-
	http_read_json_dict(Request, Data, []),
	atom_string(UUID, Data.get(uuid)),
	debug(cm(tokens), 'Asking for tokens: ~p', [Data]),
	(   catch(shadow_editor(Data, TB), cm(Reason), true)
	->  (   var(Reason)
	    ->	call_cleanup(enriched_tokens(TB, Data, Tokens),
			     release_editor(UUID)),
		reply_json_dict(json{tokens:Tokens}, [width(0)])
	    ;	check_unlocked(Reason),
		change_failed(UUID, Reason)
	    )
	;   reply_json_dict(json{tokens:[[]]})
	),
	gc_editors.


enriched_tokens(TB, _Data, Tokens) :-		% source window
	current_editor(UUID, TB, source, _Lock, _), !,
	xref(UUID),
	server_tokens(TB, Tokens).
enriched_tokens(TB, Data, Tokens) :-		% query window
	json_source_id(Data.get(sourceID), SourceID), !,
	memory_file_to_string(TB, Query),
	with_mutex(swish_highlight_query,
		   prolog_colourise_query(Query, SourceID, colour_item(TB))),
	collect_tokens(TB, Tokens).
enriched_tokens(TB, _Data, Tokens) :-
	memory_file_to_string(TB, Query),
	prolog_colourise_query(Query, module(swish), colour_item(TB)),
	collect_tokens(TB, Tokens).

%%	json_source_id(+Input, -SourceID)
%
%	Translate the Input, which is  either  a   string  or  a list of
%	strings into an  atom  or  list   of  atoms.  Older  versions of
%	SWI-Prolog only accept a single atom source id.

:- if(current_predicate(prolog_colour:to_list/2)).
json_source_id(StringList, SourceIDList) :-
	is_list(StringList),
	StringList \== [], !,
	maplist(string_source_id, StringList, SourceIDList).
:- else.				% old version (=< 7.3.7)
json_source_id([String|_], SourceID) :-
	maplist(string_source_id, String, SourceID).
:- endif.
json_source_id(String, SourceID) :-
	string(String),
	string_source_id(String, SourceID).

string_source_id(String, SourceID) :-
	atom_string(SourceID, String),
	(   fetch_editor(SourceID, _TB)
	->  release_editor(SourceID)
	;   true
	).


%%	shadow_editor(+Data, -MemoryFile) is det.
%
%	Get our shadow editor:
%
%	  1. If we have one, it is updated from either the text or the changes.
%	  2. If we have none, but there is a `text` property, create one
%	     from the text.
%	  3. If there is a `role` property, create an empty one.
%
%	This predicate fails if the server thinks we have an editor with
%	state that must be reused, but  this   is  not true (for example
%	because we have been restarted).
%
%	@throws cm(existence_error) if the target editor did not exist
%	@throws cm(out_of_sync) if the changes do not apply due to an
%	internal error or a lost message.

shadow_editor(Data, TB) :-
	atom_string(UUID, Data.get(uuid)),
	setup_call_catcher_cleanup(
	    fetch_editor(UUID, TB),
	    once(update_editor(Data, UUID, TB)),
	    Catcher,
	    cleanup_update(Catcher, UUID)), !.
shadow_editor(Data, TB) :-
	Text = Data.get(text), !,
	atom_string(UUID, Data.uuid),
	create_editor(UUID, TB, Data),
	debug(cm(change), 'Create editor for ~p', [UUID]),
	debug(cm(change_text), 'Initialising editor to ~q', [Text]),
	insert_memory_file(TB, 0, Text).
shadow_editor(Data, TB) :-
	_{role:_} :< Data, !,
	atom_string(UUID, Data.uuid),
	create_editor(UUID, TB, Data).
shadow_editor(_Data, _TB) :-
	throw(cm(existence_error)).

update_editor(Data, _UUID, TB) :-
	Text = Data.get(text), !,
	size_memory_file(TB, Size),
	delete_memory_file(TB, 0, Size),
	insert_memory_file(TB, 0, Text),
	mark_changed(TB, true).
update_editor(Data, UUID, TB) :-
	Changes = Data.get(changes), !,
	(   debug(cm(change), 'Patch editor for ~p', [UUID]),
	    maplist(apply_change(TB, Changed), Changes)
	->  true
	;   throw(cm(out_of_sync))
	),
	mark_changed(TB, Changed).

cleanup_update(exit, _) :- !.
cleanup_update(_, UUID) :-
	release_editor(UUID).

:- thread_local
	token/3.

%%	show_mirror(+Role) is det.
%%	server_tokens(+Role) is det.
%
%	These predicates help debugging the   server side. show_mirror/0
%	displays the text the server thinks is in the client editor. The
%	predicate server_tokens/1 dumps the token list.
%
%	@arg	Role is one of =source= or =query=, expressing the role of
%		the editor in the SWISH UI.

:- public
	show_mirror/1,
	server_tokens/1.

show_mirror(Role) :-
	current_editor(_UUID, TB, Role, _Lock, _), !,
	memory_file_to_string(TB, String),
	write(user_error, String).

server_tokens(Role) :-
	current_editor(_UUID, TB, Role, _Lock, _), !,
	enriched_tokens(TB, _{}, Tokens),
	print_term(Tokens, [output(user_error)]).

%%	server_tokens(+TextBuffer, -Tokens) is det.
%
%	@arg	Tokens is a nested list of Prolog JSON terms.  Each group
%		represents the tokens found in a single toplevel term.

server_tokens(TB, GroupedTokens) :-
	current_editor(UUID, TB, _Role, _Lock, _),
	setup_call_cleanup(
	    open_memory_file(TB, read, Stream),
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
	memory_file_substring(TB, Start, 1, _, FirstChar),
	(   FirstChar == "'"
	->  Type = qatom,
	    Attrs = []
	;   char_type(FirstChar, upper)
	->  Type = uatom,			% var_prefix in effect
	    Attrs = []
	;   Type = atom,
	    (   Len =< 5			% solo characters, neck, etc.
	    ->  memory_file_substring(TB, Start, Len, _, Text),
	        Attrs = [text(Text)]
	    ;   Attrs = []
	    )
	).

json_attributes([], [], _, _, _).
json_attributes([H0|T0], Attrs, TB, Start, Len) :-
	json_attribute(H0, Attrs, T, TB, Start, Len), !,
	json_attributes(T0, T, TB, Start, Len).
json_attributes([_|T0], T, TB, Start, Len) :-
	json_attributes(T0, T, TB, Start, Len).

json_attribute(text, [text(Text)|T], T, TB, Start, Len) :- !,
	memory_file_substring(TB, Start, Len, _, Text).
json_attribute(line(File:Line), [line(Line),file(File)|T], T, _, _, _) :- !.
json_attribute(Term, [Term|T], T, _, _, _).

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
style(goal_term(Class, {_}), brace_term_open-brace_term_close,
      [ name({}), arity(1) | More ]) :-
	goal_type(Class, _Type, More).
style(goal(Class, Goal), Type, [ text, arity(Arity) | More ]) :-
	Goal \= {_},
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
style(delimiter,	 delimiter,			   [text]).
style(identifier,	 identifier,			   [text]).
style(module(_Module),   module,			   [text]).
style(error,		 error,				   [text]).
style(constraint(Set),   constraint,			   [text, set(Set)]).
style(type_error(Expect), error,		      [text,expected(Expect)]).
style(syntax_error(_Msg,_Pos), syntax_error,		   []).
style(instantiation_error, instantiation_error,	           [text]).
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
style(dcg(string),	 string_terminal,		   []).
style(dcg(plain),	 brace_term_open-brace_term_close, []).
style(brace_term,	 brace_term_open-brace_term_close, []).
style(dict_content,	 dict_open-dict_close,             []).
style(expanded,		 expanded,			   [text]).
style(comment_string,	 comment_string,		   []). % up to 7.3.33
style(comment(string),	 comment_string,		   []). % after 7.3.33
style(ext_quant,	 ext_quant,			   []).
style(unused_import,	 unused_import,			   [text]).
style(undefined_import,	 undefined_import,		   [text]).
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

head_type(exported,	 head_exported).
head_type(public(_),	 head_public).
head_type(extern(_),	 head_extern).
head_type(extern(_,_),	 head_extern).
head_type(dynamic,	 head_dynamic).
head_type(multifile,	 head_multifile).
head_type(unreferenced,	 head_unreferenced).
head_type(hook,		 head_hook).
head_type(meta,		 head_meta).
head_type(constraint(_), head_constraint).
head_type(imported,	 head_imported).
head_type(built_in,	 head_built_in).
head_type(iso,		 head_iso).
head_type(def_iso,	 head_def_iso).
head_type(def_swi,	 head_def_swi).
head_type(_,		 head).

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
goal_type(extern(_,_),	      goal_extern,	 []).
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

%%	swish_config:config(-Name, -Styles) is nondet.
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
	x11_color(Name, R, G, B),
	format(atom(RGB), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+', [R,G,B]).
css_style(Style, Style).

%%	x11_color(+Name, -R, -G, -B)
%
%	True if RGB is the color for the named X11 color.

x11_color(Name, R, G, B) :-
	(   x11_color_cache(_,_,_,_)
	->  true
	;   load_x11_colours
	),
	x11_color_cache(Name, R, G, B).

:- dynamic
	x11_color_cache/4.

load_x11_colours :-
	source_file(load_x11_colours, File),
	file_directory_name(File, Dir),
	directory_file_path(Dir, 'rgb.txt', RgbFile),
	setup_call_cleanup(
	    open(RgbFile, read, In),
	    ( lazy_list(lazy_read_lines(In, [as(string)]), List),
	      maplist(assert_colour, List)
	    ),
	    close(In)).

assert_colour(String) :-
	split_string(String, "\s\t\r", "\s\t\r", [RS,GS,BS|NameParts]),
	number_string(R, RS),
	number_string(G, GS),
	number_string(B, BS),
	atomic_list_concat(NameParts, '_', Name0),
	downcase_atom(Name0, Name),
	assertz(x11_color_cache(Name, R, G, B)).

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
	  text_arity_pi(Name, Arity, PI),
	  predicate_info(Module:PI, Info)
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

text_arity_pi('[', 2, consult/1) :- !.
text_arity_pi(']', 2, consult/1) :- !.
text_arity_pi(Name, Arity, Name/Arity).


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
	functor(Head, Name, Arity),
	predicate_property(system:Head, iso), !,
	ignore(Module = system),
	(   catch(once(predicate(Name, Arity, Summary, _, _)), _, fail),
	    Key = summary,
	    Value = Summary
	;   Key = iso,
	    Value = true
	).
predicate_info(_Module:Name/Arity, summary, Summary) :-
	catch(once(predicate(Name, Arity, Summary, _, _)), _, fail), !.
predicate_info(PI, summary, Summary) :-	% PlDoc
	once(prolog:predicate_summary(PI, Summary)).
