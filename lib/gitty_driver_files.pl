/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(gitty_driver_files,
	  [ gitty_close/1,		% +Store
	    gitty_file/3,		% +Store, ?Name, ?Hash

	    gitty_update_head/4,	% +Store, +Name, +OldCommit, +NewCommit
	    delete_head/2,		% +Store, +Name
	    set_head/3,			% +Store, +Name, +Hash
	    store_object/4,		% +Store, +Hash, +Header, +Data
	    delete_object/2,		% +Store, +Hash

	    gitty_hash/2,		% +Store, ?Hash
	    load_plain_commit/3,	% +Store, +Hash, -Meta
	    load_object/5,		% +Store, +Hash, -Data, -Type, -Size

	    gitty_rescan/1		% Store
	  ]).
:- use_module(library(zlib)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

/** <module> Gitty plain files driver

This version of the driver uses plain files  to store the gitty data. It
consists of a nested directory  structure   with  files  named after the
hash. Objects and hash computation is the same as for `git`. The _heads_
(files) are computed on startup by scanning all objects. There is a file
=ref/head= that is updated if a head is updated. Other clients can watch
this file and update their notion  of   the  head. This implies that the
store can handle multiple clients that can  access a shared file system,
optionally shared using NFS from different machines.

The store is simple and robust. The  main disadvantages are long startup
times as the store holds more objects and relatively high disk usage due
to rounding the small objects to disk allocation units.

@bug	Shared access does not work on Windows.
*/

:- dynamic
	head/3,				% Store, Name, Hash
	store/2,			% Store, Updated
	heads_input_stream_cache/2.	% Store, Stream
:- volatile
	head/3,
	store/2,
	heads_input_stream_cache/2.	% Store, Stream

% enable/disable syncing remote servers running on  the same file store.
% This facility requires shared access to files and thus doesn't work on
% Windows.

:- if(current_prolog_flag(windows, true)).
remote_sync(false).
:- else.
remote_sync(true).
:- endif.

%%	gitty_close(+Store) is det.
%
%	Close resources associated with a store.

gitty_close(Store) :-
	(   retract(heads_input_stream_cache(Store, In))
	->  close(In)
	;   true
	),
	retractall(head(Store,_,_)),
	retractall(store(Store,_)).


%%	gitty_file(+Store, ?File, ?Head) is nondet.
%
%	True when File entry in the  gitty   store  and Head is the HEAD
%	revision.

gitty_file(Store, Head, Hash) :-
	gitty_scan(Store),
	head(Store, Head, Hash).

%%	load_plain_commit(+Store, +Hash, -Meta:dict) is semidet.
%
%	Load the commit data as a dict.

load_plain_commit(Store, Hash, Meta) :-
	load_object(Store, Hash, String, _, _),
	term_string(Meta, String, []).

%%	store_object(+Store, +Hash, +Header:string, +Data:string) is det.
%
%	Store the actual object. The store  must associate Hash with the
%	concatenation of Hdr and Data.

store_object(Store, Hash, Hdr, Data) :-
	sub_atom(Hash, 0, 2, _, Dir0),
	sub_atom(Hash, 2, 2, _, Dir1),
	sub_atom(Hash, 4, _, 0, File),
	directory_file_path(Store, Dir0, D0),
	ensure_directory(D0),
	directory_file_path(D0, Dir1, D1),
	ensure_directory(D1),
	directory_file_path(D1, File, Path),
	(   exists_file(Path)
	->  true
	;   setup_call_cleanup(
		gzopen(Path, write, Out, [encoding(utf8)]),
		format(Out, '~s~s', [Hdr, Data]),
		close(Out))
	).

ensure_directory(Dir) :-
	exists_directory(Dir), !.
ensure_directory(Dir) :-
	make_directory(Dir).

%%	load_object(+Store, +Hash, -Data, -Type, -Size) is det.
%
%	Load the given object.

load_object(Store, Hash, Data, Type, Size) :-
	hash_file(Store, Hash, Path),
	setup_call_cleanup(
	    gzopen(Path, read, In, [encoding(utf8)]),
	    read_object(In, Data, Type, Size),
	    close(In)).

read_object(In, Data, Type, Size) :-
	get_code(In, C0),
	read_hdr(C0, In, Hdr),
	phrase((nonblanks(TypeChars), " ", integer(Size)), Hdr),
	atom_codes(Type, TypeChars),
	read_string(In, _, Data).

read_hdr(C, In, [C|T]) :-
	C > 0, !,
	get_code(In, C1),
	read_hdr(C1, In, T).
read_hdr(_, _, []).

%%	gitty_rescan(?Store) is det.
%
%	Update our view of the shared   storage  for all stores matching
%	Store.

gitty_rescan(Store) :-
	retractall(store(Store, _)).

%%	gitty_scan(+Store) is det.
%
%	Scan gitty store for files (entries),   filling  head/3. This is
%	performed lazily at first access to the store.
%
%	@tdb	Possibly we need to maintain a cached version of this
%		index to avoid having to open all objects of the gitty
%		store.

gitty_scan(Store) :-
	store(Store, _), !,
	(   remote_sync(true)
	->  with_mutex(gitty, remote_updates(Store))
	;   true
	).
gitty_scan(Store) :-
	with_mutex(gitty, gitty_scan_sync(Store)).

:- thread_local
	latest/3.

gitty_scan_sync(Store) :-
	store(Store, _), !.
gitty_scan_sync(Store) :-
	remote_sync(true), !,
	restore_heads_from_remote(Store).
gitty_scan_sync(Store) :-
	read_heads_from_objects(Store).

%%	read_heads_from_objects(+Store) is det.
%
%	Establish the head(Store,File,Hash)  relation   by  reading  all
%	objects and adding a fact for the most recent commit.

read_heads_from_objects(Store) :-
	gitty_scan_latest(Store),
	forall(retract(latest(Name, Hash, _Time)),
	       assert(head(Store, Name, Hash))),
	get_time(Now),
	assertz(store(Store, Now)).

%%	gitty_scan_latest(+Store)
%
%	Scans the gitty store, extracting  the   latest  version of each
%	named entry.

gitty_scan_latest(Store) :-
	retractall(head(Store, _, _)),
	retractall(latest(_, _, _)),
	(   gitty_hash(Store, Hash),
	    load_object(Store, Hash, Data, commit, _Size),
	    term_string(Meta, Data, []),
	    _{name:Name, time:Time} :< Meta,
	    (	latest(Name, _, OldTime),
		OldTime > Time
	    ->	true
	    ;	retractall(latest(Name, _, _)),
		assertz(latest(Name, Hash, Time))
	    ),
	    fail
	;   true
	).


%%	gitty_hash(+Store, ?Hash) is nondet.
%
%	True when Hash is an object in the store.

gitty_hash(Store, Hash) :-
	var(Hash), !,
	access_file(Store, exist),
	directory_files(Store, Level0),
	member(E0, Level0),
	E0 \== '..',
	atom_length(E0, 2),
	directory_file_path(Store, E0, Dir0),
	directory_files(Dir0, Level1),
	member(E1, Level1),
	E1 \== '..',
	atom_length(E1, 2),
	directory_file_path(Dir0, E1, Dir),
	directory_files(Dir, Files),
	member(File, Files),
	atom_length(File, 36),
	atomic_list_concat([E0,E1,File], Hash).
gitty_hash(Store, Hash) :-
	hash_file(Store, Hash, File),
	exists_file(File).

%%	delete_object(+Store, +Hash)
%
%	Delete an existing object

delete_object(Store, Hash) :-
	hash_file(Store, Hash, File),
	delete_file(File).

hash_file(Store, Hash, Path) :-
	sub_atom(Hash, 0, 2, _, Dir0),
	sub_atom(Hash, 2, 2, _, Dir1),
	sub_atom(Hash, 4, _, 0, File),
	atomic_list_concat([Store, Dir0, Dir1, File], /, Path).


		 /*******************************
		 *	      SYNCING		*
		 *******************************/

%%	gitty_update_head(+Store, +Name, +OldCommit, +NewCommit) is det.
%
%	Update the head of a gitty  store   for  Name.  OldCommit is the
%	current head and NewCommit is the new  head. If Name is created,
%	and thus there is no head, OldCommit must be `-`.
%
%	This operation can fail because another   writer has updated the
%	head.  This can both be in-process or another process.

gitty_update_head(Store, Name, OldCommit, NewCommit) :-
	with_mutex(gitty,
		   gitty_update_head_sync(Store, Name, OldCommit, NewCommit)).

gitty_update_head_sync(Store, Name, OldCommit, NewCommit) :-
	remote_sync(true), !,
	setup_call_cleanup(
	    heads_output_stream(Store, HeadsOut),
	    gitty_update_head_sync(Store, Name, OldCommit, NewCommit, HeadsOut),
	    close(HeadsOut)).
gitty_update_head_sync(Store, Name, OldCommit, NewCommit) :-
	gitty_update_head_sync2(Store, Name, OldCommit, NewCommit).

gitty_update_head_sync(Store, Name, OldCommit, NewCommit, HeadsOut) :-
	gitty_update_head_sync2(Store, Name, OldCommit, NewCommit),
	format(HeadsOut, '~q.~n', [head(Name, OldCommit, NewCommit)]).

gitty_update_head_sync2(Store, Name, OldCommit, NewCommit) :-
	gitty_scan(Store),		% fetch remote changes
	(   OldCommit == (-)
	->  (   head(Store, Name, _)
	    ->	throw(error(gitty(file_exists(Name),_)))
	    ;	assertz(head(Store, Name, NewCommit))
	    )
	;   (   retract(head(Store, Name, OldCommit))
	    ->	assertz(head(Store, Name, NewCommit))
	    ;	throw(error(gitty(not_at_head(Name, OldCommit)), _))
	    )
	).

remote_updates(Store) :-
	remote_updates(Store, List),
	maplist(update_head(Store), List).

update_head(Store, head(Name, OldCommit, NewCommit)) :-
	(   OldCommit == (-)
	->  \+ head(Store, Name, _)
	;   retract(head(Store, Name, OldCommit))
	), !,
	assert(head(Store, Name, NewCommit)).
update_head(_, _).

%%	remote_updates(+Store, -List) is det.
%
%	Find updates from other gitties  on   the  same filesystem. Note
%	that we have to push/pop the input   context to avoid creating a
%	notion of an  input  context   which  possibly  relate  messages
%	incorrectly to the sync file.

remote_updates(Store, List) :-
	heads_input_stream(Store, Stream),
	setup_call_cleanup(
	    '$push_input_context'(gitty_sync),
	    read_new_terms(Stream, List),
	    '$pop_input_context').

read_new_terms(Stream, Terms) :-
	read(Stream, First),
	read_new_terms(First, Stream, Terms).

read_new_terms(end_of_file, _, List) :- !,
	List = [].
read_new_terms(Term, Stream, [Term|More]) :-
	read(Stream, Term2),
	read_new_terms(Term2, Stream, More).

heads_output_stream(Store, Out) :-
	heads_file(Store, HeadsFile),
	open(HeadsFile, append, Out,
	     [ encoding(utf8),
	       lock(exclusive)
	     ]).

heads_input_stream(Store, Stream) :-
	heads_input_stream_cache(Store, Stream0), !,
	Stream = Stream0.
heads_input_stream(Store, Stream) :-
	heads_file(Store, File),
	between(1, 2, _),
	catch(open(File, read, In,
		   [ encoding(utf8),
		     eof_action(reset)
		   ]),
	      _,
	      create_heads_file(Store)), !,
	assert(heads_input_stream_cache(Store, In)),
	Stream = In.

create_heads_file(Store) :-
	call_cleanup(
	    heads_output_stream(Store, Out),
	    close(Out)),
	fail.					% always fail!

heads_file(Store, HeadsFile) :-
	ensure_directory(Store),
	directory_file_path(Store, ref, RefDir),
	ensure_directory(RefDir),
	directory_file_path(RefDir, head, HeadsFile).

%%	restore_heads_from_remote(Store)
%
%	Restore the known heads by reading the remote sync file.

restore_heads_from_remote(Store) :-
	heads_file(Store, File),
	exists_file(File),
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    restore_heads(Store, In),
	    close(In)), !,
	get_time(Now),
	assertz(store(Store, Now)).
restore_heads_from_remote(Store) :-
	read_heads_from_objects(Store),
	heads_file(Store, File),
	setup_call_cleanup(
	    open(File, write, Out, [encoding(utf8)]),
	    save_heads(Store, Out),
	    close(Out)), !.

restore_heads(Store, In) :-
	read(In, Term0),
	Term0 = epoch(_),
	read(In, Term1),
	restore_heads(Term1, In, Store).

restore_heads(end_of_file, _, _) :- !.
restore_heads(head(File, _, Hash), In, Store) :-
	retractall(head(Store, File, _)),
	assertz(head(Store, File, Hash)),
	read(In, Term),
	restore_heads(Term, In, Store).

save_heads(Store, Out) :-
	get_time(Now),
	format(Out, 'epoch(~0f).~n~n', [Now]),
	forall(head(Store, File, Hash),
	       format(Out, '~q.~n', [head(File, -, Hash)])).


%%	delete_head(+Store, +Head) is det.
%
%	Delete Head from Store. Used  by   gitty_fsck/1  to remove heads
%	that have no commits. Should  we   forward  this  to remotes, or
%	should they do their own thing?

delete_head(Store, Head) :-
	retractall(head(Store, Head, _)).

%%	set_head(+Store, +File, +Hash) is det.
%
%	Set the head of the given File to Hash

set_head(Store, File, Hash) :-
	retractall(head(Store, File, _)),
	asserta(head(Store, File, Hash)).
