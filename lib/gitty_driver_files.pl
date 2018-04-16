/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2017, VU University Amsterdam
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

:- module(gitty_driver_files,
	  [ gitty_close/1,		% +Store
	    gitty_file/4,		% +Store, ?Name, ?Ext, ?Hash

	    gitty_update_head/4,	% +Store, +Name, +OldCommit, +NewCommit
	    delete_head/2,		% +Store, +Name
	    set_head/3,			% +Store, +Name, +Hash
	    store_object/4,		% +Store, +Hash, +Header, +Data
	    delete_object/2,		% +Store, +Hash

	    gitty_hash/2,		% +Store, ?Hash
	    load_plain_commit/3,	% +Store, +Hash, -Meta
	    load_object/5,		% +Store, +Hash, -Data, -Type, -Size
	    gitty_object_file/3,	% +Store, +Hash, -File

	    repack_objects/2,           % +Store, +Options
            pack_objects/6,             % +Store, +Objs, +Packs, +PackDir,
					% -File, +Opts
            unpack_packs/1,             % +Store
            unpack_pack/2,              % +Store, +PackFile

            attach_pack/2,		% +Store, +PackFile
            gitty_fsck/1,               % +Store
            fsck_pack/1,                % +PackFile
            load_object_from_pack/4,	% +Hash, -Data, -Type, -Size

	    gitty_rescan/1		% Store
	  ]).
:- use_module(library(apply)).
:- use_module(library(zlib)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(zlib)).
:- use_module(library(hash_stream)).
:- use_module(library(option)).
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
    head/4,				% Store, Name, Ext, Hash
    store/2,				% Store, Updated
    commit/3,				% Store, Hash, Meta
    heads_input_stream_cache/2,		% Store, Stream
    pack_object/6,                      % Hash, Type, Size, Offset, Store,PackFile
    attached_packs/1,                   % Store
    attached_pack/2.                    % Store, PackFile

:- volatile
    head/4,
    store/2,
    commit/3,
    heads_input_stream_cache/2,
    pack_object/6,
    attached_packs/1,
    attached_pack/2.

% enable/disable syncing remote servers running on  the same file store.
% This facility requires shared access to files and thus doesn't work on
% Windows.

:- if(current_prolog_flag(windows, true)).
remote_sync(false).
:- else.
remote_sync(true).
:- endif.

%!  gitty_close(+Store) is det.
%
%   Close resources associated with a store.

gitty_close(Store) :-
    (   retract(heads_input_stream_cache(Store, In))
    ->  close(In)
    ;   true
    ),
    retractall(head(Store,_,_,_)),
    retractall(store(Store,_)),
    retractall(pack_object(_,_,_,_,Store,_)).


%%	gitty_file(+Store, ?File, ?Ext, ?Head) is nondet.
%
%	True when File entry in the  gitty   store  and Head is the HEAD
%	revision.

gitty_file(Store, Head, Ext, Hash) :-
	gitty_scan(Store),
	head(Store, Head, Ext, Hash).

%%	load_plain_commit(+Store, +Hash, -Meta:dict) is semidet.
%
%	Load the commit data as a  dict.   Loaded  commits are cached in
%	commit/3.  Note  that  only  adding  a  fact  to  the  cache  is
%	synchronized. This means that during  a   race  situation we may
%	load the same object  multiple  times   from  disk,  but this is
%	harmless while a lock  around   the  whole  predicate serializes
%	loading different objects, which is not needed.

load_plain_commit(Store, Hash, Meta) :-
	must_be(atom, Store),
	must_be(atom, Hash),
	commit(Store, Hash, Meta), !.
load_plain_commit(Store, Hash, Meta) :-
	load_object(Store, Hash, String, _, _),
	term_string(Meta0, String, []),
	with_mutex(gitty_commit_cache,
		   assert_cached_commit(Store, Hash, Meta0)),
	Meta = Meta0.

assert_cached_commit(Store, Hash, Meta) :-
	commit(Store, Hash, Meta0), !,
	assertion(Meta0 =@= Meta).
assert_cached_commit(Store, Hash, Meta) :-
	assertz(commit(Store, Hash, Meta)).

%%	store_object(+Store, +Hash, +Header:string, +Data:string) is det.
%
%	Store the actual object. The store  must associate Hash with the
%	concatenation of Hdr and Data.

store_object(Store, Hash, _Hdr, _Data) :-
        pack_object(Hash, _Type, _Size, _Offset, Store, _Pack), !.
store_object(Store, Hash, Hdr, Data) :-
        gitty_object_file(Store, Hash, Path),
        with_mutex(gitty_file, exists_or_create(Path, Out0)),
	(   var(Out0)
	->  true
	;   setup_call_cleanup(
		zopen(Out0, Out, [format(gzip)]),
		format(Out, '~s~s', [Hdr, Data]),
		close(Out))
	).

exists_or_create(Path, _Out) :-
	exists_file(Path), !.
exists_or_create(Path, Out) :-
        file_directory_name(Path, Dir),
        make_directory_path(Dir),
        open(Path, write, Out, [encoding(utf8), lock(write)]).

ensure_directory(Dir) :-
	exists_directory(Dir), !.
ensure_directory(Dir) :-
	make_directory(Dir).

%%	load_object(+Store, +Hash, -Data, -Type, -Size) is det.
%
%	Load the given object.

load_object(_Store, Hash, Data, Type, Size) :-
        load_object_from_pack(Hash, Data0, Type0, Size0), !,
        f(Data0, Type0, Size0) = f(Data, Type, Size).
load_object(Store, Hash, Data, Type, Size) :-
	gitty_object_file(Store, Hash, Path),
        exists_file(Path),
	setup_call_cleanup(
	    gzopen(Path, read, In, [encoding(utf8)]),
	    read_object(In, Data, Type, Size),
	    close(In)).

%!	load_object_header(+Store, +Hash, -Type, -Size) is det.
%
%	Load the header of an object

load_object_header(Store, Hash, Type, Size) :-
	gitty_object_file(Store, Hash, Path),
	setup_call_cleanup(
	    gzopen(Path, read, In, [encoding(utf8)]),
	    read_object_hdr(In, Type, Size),
	    close(In)).

read_object(In, Data, Type, Size) :-
	read_object_hdr(In, Type, Size),
	read_string(In, _, Data).

read_object_hdr(In, Type, Size) :-
	get_code(In, C0),
	read_hdr(C0, In, Hdr),
	phrase((nonblanks(TypeChars), " ", integer(Size)), Hdr),
	atom_codes(Type, TypeChars).

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
	remote_updates(Store).
gitty_scan(Store) :-
	with_mutex(gitty, gitty_scan_sync(Store)).

:- thread_local
	latest/3.

gitty_scan_sync(Store) :-
	store(Store, _), !.
gitty_scan_sync(Store) :-
	remote_sync(true), !,
        gitty_attach_packs(Store),
	restore_heads_from_remote(Store).
gitty_scan_sync(Store) :-
        gitty_attach_packs(Store),
	read_heads_from_objects(Store).

%%	read_heads_from_objects(+Store) is det.
%
%       Establish the head(Store,File,Ext,Hash) relation  by reading all
%       objects and adding a fact for the most recent commit.

read_heads_from_objects(Store) :-
	gitty_scan_latest(Store),
	forall(retract(latest(Name, Hash, _Time)),
	       assert_head(Store, Name, Hash)),
	get_time(Now),
	assertz(store(Store, Now)).

assert_head(Store, Name, Hash) :-
	file_name_extension(_, Ext, Name),
        assertz(head(Store, Name, Ext, Hash)).


%%	gitty_scan_latest(+Store)
%
%	Scans the gitty store, extracting  the   latest  version of each
%	named entry.

gitty_scan_latest(Store) :-
	retractall(head(Store, _, _, _)),
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
        (   gitty_attach_packs(Store),
            pack_object(Hash, _Type, _Size, _Offset, Store, _Pack)
        ;   gitty_file_object(Store, Hash)
        ).
gitty_hash(Store, Hash) :-
        (   gitty_attach_packs(Store),
            pack_object(Hash, _Type, _Size, _Offset, Store, _Pack)
        ->  true
        ;   gitty_object_file(Store, Hash, File),
            exists_file(File)
        ).

gitty_file_object(Store, Hash) :-
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

%%	delete_object(+Store, +Hash)
%
%	Delete an existing object

delete_object(Store, Hash) :-
	gitty_object_file(Store, Hash, File),
	delete_file(File).

%!	gitty_object_file(+Store, +Hash, -Path) is det.
%
%	True when Path is the file  at   which  the  object with Hash is
%	stored.

gitty_object_file(Store, Hash, Path) :-
	sub_string(Hash, 0, 2, _, Dir0),
	sub_string(Hash, 2, 2, _, Dir1),
	sub_string(Hash, 4, _, 0, File),
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
	->  (   head(Store, Name, _, _)
	    ->	throw(error(gitty(file_exists(Name),_)))
	    ;	assert_head(Store, Name, NewCommit)
	    )
	;   (   retract(head(Store, Name, _, OldCommit))
	    ->	assert_head(Store, Name, NewCommit)
	    ;	throw(error(gitty(not_at_head(Name, OldCommit)), _))
	    )
	).

%!	remote_updates(+Store)
%
%	Watch for remote updates to the store. We only do this if we did
%	not do so the last second.

:- dynamic
	last_remote_sync/2.

remote_updates(_) :-
	remote_sync(false), !.
remote_updates(Store) :-
	remote_up_to_data(Store), !.
remote_updates(Store) :-
	with_mutex(gitty, remote_updates_sync(Store)).

remote_updates_sync(Store) :-
	remote_up_to_data(Store), !.
remote_updates_sync(Store) :-
	retractall(last_remote_sync(Store, _)),
	get_time(Now),
	asserta(last_remote_sync(Store, Now)),
	remote_update(Store).

remote_up_to_data(Store) :-
	last_remote_sync(Store, Last),
	get_time(Now),
	Now-Last < 1.

remote_update(Store) :-
	remote_updates(Store, List),
	maplist(update_head(Store), List).

update_head(Store, head(Name, OldCommit, NewCommit)) :-
	(   OldCommit == (-)
	->  \+ head(Store, Name, _, _)
	;   retract(head(Store, Name, _, OldCommit))
	), !,
	assert_head(Store, Name, NewCommit).
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
	retractall(head(Store, File, _, _)),
	assert_head(Store, File, Hash),
	read(In, Term),
	restore_heads(Term, In, Store).

save_heads(Store, Out) :-
	get_time(Now),
	format(Out, 'epoch(~0f).~n~n', [Now]),
	forall(head(Store, File, _, Hash),
	       format(Out, '~q.~n', [head(File, -, Hash)])).


%%	delete_head(+Store, +Head) is det.
%
%	Delete Head from Store. Used  by   gitty_fsck/1  to remove heads
%	that have no commits. Should  we   forward  this  to remotes, or
%	should they do their own thing?

delete_head(Store, Head) :-
	retractall(head(Store, Head, _, _)).

%%	set_head(+Store, +File, +Hash) is det.
%
%	Set the head of the given File to Hash

set_head(Store, File, Hash) :-
	file_name_extension(_, Ext, File),
        (   head(Store, File, _, Hash0)
        ->  (   Hash == Hash0
            ->  true
            ;   asserta(head(Store, File, Ext, Hash)),
                retractall(head(Store, File, _, Hash0))
            )
        ;   asserta(head(Store, File, Ext, Hash))
        ).


		 /*******************************
		 *	      PACKS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

<pack file> := <header>
               <file>*
<header>    := "gitty(Version).\n" <object>* "end_of_header.\n"
<object>    := obj(Hash, Type, Size, FileSize)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pack_version(1).

%!  repack_objects(+Store, +Options) is det.
%
%   Repack  objects  of  Store  for  reduced  disk  usage  and  enhanced
%   performance. By default this picks up all  file objects of the store
%   and all existing small pack files.  Options:
%
%     - small_pack(+Bytes)
%     Consider all packs with less than Bytes as small and repack them.
%     Default 10Mb
%     - min_files(+Count)
%     Do not repack if there are less than Count new files.
%     Default 1,000.

:- debug(gitty(pack)).

repack_objects(Store, Options) :-
    option(min_files(MinFiles), Options, 1_000),
    findall(Object, gitty_file_object(Store, Object), Objects),
    length(Objects, NewFiles),
    debug(gitty(pack), 'Found ~D file objects', [NewFiles]),
    (   NewFiles >= MinFiles
    ->  pack_files(Store, ExistingPacks),
        option(small_pack(MaxSize), Options, 10_000_000),
        include(small_file(MaxSize), ExistingPacks, PackFiles),
        (   debugging(gitty(pack))
        ->  length(PackFiles, PackCount),
            debug(gitty(pack), 'Found ~D small packs', [PackCount])
        ;   true
        ),
        directory_file_path(Store, pack, PackDir),
        make_directory_path(PackDir),
        pack_objects(Store, Objects, PackFiles, PackDir, _PackFile, Options)
    ;   debug(gitty(pack), 'Nothing to do', [])
    ).

small_file(MaxSize, File) :-
    size_file(File, Size),
    Size < MaxSize.

%!  pack_objects(+Store, +Objects, +Packs, +PackDir,
%!               -PackFile, +Options) is det.
%
%   Pack the given objects and pack files into a new pack.

pack_objects(Store, Objects, Packs, PackDir, PackFile, Options) :-
    with_mutex(gitty_pack,
	       pack_objects_sync(Store, Objects, Packs, PackDir,
                                 PackFile, Options)).

pack_objects_sync(_Store, [], [Pack], _, [Pack], _) :-
    !.
pack_objects_sync(Store, Objects, Packs, PackDir, PackFilePath, Options) :-
    length(Objects, ObjCount),
    length(Packs, PackCount),
    debug(gitty(pack), 'Repacking ~D objects and ~D packs',
          [ObjCount, PackCount]),
    maplist(object_info(Store), Objects, FileInfo),
    maplist(pack_info(Store), Packs, PackInfo),
    append([FileInfo|PackInfo], Info0),
    sort(1, @<, Info0, Info),           % remove possible duplicates
    (   debugging(gitty(pack))
    ->  (   PackCount > 0
        ->  length(Info, FinalObjCount),
            debug(gitty(pack), 'Total ~D objects', [FinalObjCount])
        ;   true
        )
    ;   true
    ),
    directory_file_path(PackDir, 'pack-create', TmpPack),
    setup_call_cleanup(
	(   open(TmpPack, write, Out0, [type(binary), lock(write)]),
	    open_hash_stream(Out0, Out, [algorithm(sha1)])
	),
        (   write_signature(Out),
            maplist(write_header(Out), Info),
            format(Out, 'end_of_header.~n', []),
            maplist(add_file(Out, Store), Info),
	    stream_hash(Out, SHA1)
        ),
        close(Out)),
    format(atom(PackFile), 'pack-~w.pack', [SHA1]),
    directory_file_path(PackDir, PackFile, PackFilePath),
    rename_file(TmpPack, PackFilePath),
    debug(gitty(pack), 'Attaching ~p', [PackFilePath]),
    attach_pack(Store, PackFilePath),
    remove_objects_after_pack(Store, Objects, Options),
    delete(Packs, PackFilePath, RmPacks),
    remove_repacked_packs(Store, RmPacks, Options),
    debug(gitty(pack), 'Packing completed', []).

object_info(Store, Object, obj(Object, Type, Size, FileSize)) :-
    gitty_object_file(Store, Object, File),
    load_object_header(Store, Object, Type, Size),
    size_file(File, FileSize).

pack_info(Store, PackFile, Objects) :-
    attach_pack(Store, PackFile),
    pack_read_header(PackFile, _Version, _DataOffset, Objects).

write_signature(Out) :-
    pack_version(Version),
    format(Out, "gitty(~d).~n", [Version]).

write_header(Out, obj(Object, Type, Size, FileSize)) :-
    format(Out, 'obj(~q,~q,~d,~d).~n', [Object, Type, Size, FileSize]).

%!  add_file(+Out, +Store, +Object) is det.
%
%   Add Object from Store to the pack stream Out.

add_file(Out, Store, obj(Object, _Type, _Size, _FileSize)) :-
    gitty_object_file(Store, Object, File),
    exists_file(File),
    !,
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        copy_stream_data(In, Out),
        close(In)).
add_file(Out, Store, obj(Object, Type, Size, FileSize)) :-
    pack_object(Object, Type, Size, Offset, Store, PackFile),
    setup_call_cleanup(
        open(PackFile, read, In, [type(binary)]),
        (   seek(In, Offset, bof, Offset),
            copy_stream_data(In, Out, FileSize)
        ),
        close(In)).


%!  gitty_fsck(+Store) is det.
%
%   Validate all packs associated with Store

gitty_fsck(Store) :-
    pack_files(Store, PackFiles),
    maplist(fsck_pack, PackFiles).

%!  fsck_pack(+File) is det.
%
%   Validate the integrity of the pack file File.

fsck_pack(File) :-
    debug(gitty(pack), 'fsck ~p', [File]),
    check_pack_hash(File),
    debug(gitty(pack), 'fsck ~p: checking objects', [File]),
    check_pack_objects(File),
    debug(gitty(pack), 'fsck ~p: done', [File]).

check_pack_hash(File) :-
    file_base_name(File, Base),
    file_name_extension(Plain, Ext, Base),
    must_be(oneof([pack]), Ext),
    atom_concat('pack-', Hash, Plain),
    setup_call_cleanup(
        (   open(File, read, In0, [type(binary)]),
            open_hash_stream(In0, In, [algorithm(sha1)])
        ),
        (   setup_call_cleanup(
                open_null_stream(Null),
                copy_stream_data(In, Null),
                close(Null)),
            stream_hash(In, SHA1)
        ),
        close(In)),
    assertion(Hash == SHA1).

check_pack_objects(PackFile) :-
    setup_call_cleanup(
        open(PackFile, read, In, [type(binary)]),
        (  read_header(In, Version, DataOffset, Objects),
           set_stream(In, encoding(utf8)),
           foldl(check_object(In, PackFile, Version), Objects, DataOffset, _)
        ),
        close(In)).

check_object(In, PackFile, _Version,
             obj(Object, Type, Size, FileSize),
             Offset0, Offset) :-
    Offset is Offset0+FileSize,
    byte_count(In, Here),
    (   Here == Offset0
    ->  true
    ;   print_message(warning, pack(reposition(Here, Offset0))),
        seek(In, Offset0, bof, Offset0)
    ),
    (   setup_call_cleanup(
            zopen(In, In2, [multi_part(false), close_parent(false)]),
            catch(read_object(In2, Data, _0RType, _0RSize), E,
                  ( print_message(error,
                                  gitty(PackFile, fsck(read_object(Object, E)))),
                    fail)),
            close(In2))
    ->  byte_count(In, End),
        (   End == Offset
        ->  true
        ;   print_message(error,
                          gitty(PackFile, fsck(object_end(Object, End,
                                                          Offset0, Offset,
                                                          Data))))
        ),
        assertion(Type == _0RType),
        assertion(Size == _0RSize),
        gitty:check_object(Object, Data, Type, Size)
    ;   true
    ).


%!  gitty_attach_packs(+Store) is det.
%
%   Attach all packs for Store

gitty_attach_packs(Store) :-
    attached_packs(Store),
    !.
gitty_attach_packs(Store) :-
    with_mutex(gitty_attach_packs,
               gitty_attach_packs_sync(Store)).

gitty_attach_packs_sync(Store) :-
    attached_packs(Store),
    !.
gitty_attach_packs_sync(Store) :-
    pack_files(Store, PackFiles),
    maplist(attach_pack(Store), PackFiles),
    asserta(attached_packs(Store)).

pack_files(Store, Packs) :-
    directory_file_path(Store, pack, PackDir),
    exists_directory(PackDir),
    !,
    directory_files(PackDir, Files),
    convlist(is_pack(PackDir), Files, Packs).
pack_files(_, []).

is_pack(PackDir, File, Path) :-
    file_name_extension(_, pack, File),
    directory_file_path(PackDir, File, Path).

%!  attach_pack(+Store, +PackFile)
%
%   Load the index of Pack into memory.

attach_pack(Store, PackFile) :-
    attached_pack(Store, PackFile),
    !.
attach_pack(Store, PackFile) :-
    retractall(pack_object(_,_,_,_,_,PackFile)),
    pack_read_header(PackFile, Version, DataOffset, Objects),
    foldl(assert_object(Store, PackFile, Version), Objects, DataOffset, _),
    assertz(attached_pack(Store, PackFile)).

pack_read_header(PackFile, Version, DataOffset, Objects) :-
    setup_call_cleanup(
        open(PackFile, read, In, [type(binary)]),
        read_header(In, Version, DataOffset, Objects),
        close(In)).

read_header(In, Version, DataOffset, Objects) :-
    read(In, Signature),
    (   Signature = gitty(Version)
    ->  true
    ;   domain_error(gitty_pack_file, Objects)
    ),
    read(In, Term),
    read_index(Term, In, Objects),
    get_code(In, Code),
    assertion(Code == 0'\n),
    byte_count(In, DataOffset).

read_index(end_of_header, _, []) :-
    !.
read_index(Object, In, [Object|T]) :-
    read(In, Term2),
    read_index(Term2, In, T).

assert_object(Store, Pack, _Version,
              obj(Object, Type, Size, FileSize),
              Offset0, Offset) :-
    Offset is Offset0+FileSize,
    assertz(pack_object(Object, Type, Size, Offset0, Store, Pack)).

%!  detach_pack(+Store, +Pack) is det.
%
%   Remove a pack file from the memory index.

detach_pack(Store, Pack) :-
    retractall(pack_object(_, _, _, _, Store, Pack)),
    retractall(attached_pack(Store, Pack)).

%!  load_object_from_pack(+Hash, -Data, -Type, -Size) is semidet.
%
%   True when Hash is in a pack and can be loaded.

load_object_from_pack(Hash, Data, Type, Size) :-
    pack_object(Hash, Type, Size, Offset, _Store, Pack),
    setup_call_cleanup(
        open(Pack, read, In, [type(binary)]),
        read_object_at(In, Offset, Data, Type, Size),
        close(In)).

read_object_at(In, Offset, Data, Type, Size) :-
    seek(In, Offset, bof, Offset),
    read_object_here(In, Data, Type, Size).

read_object_here(In, Data, Type, Size) :-
    stream_property(In, encoding(Enc)),
    setup_call_cleanup(
        ( set_stream(In, encoding(utf8)),
          zopen(In, In2, [multi_part(false), close_parent(false)])
        ),
        read_object(In2, Data, Type, Size),
        ( close(In2),
          set_stream(In, encoding(Enc))
        )).

%!  unpack_packs(+Store) is det.
%
%   Unpack all packs.

unpack_packs(Store) :-
    absolute_file_name(Store, AbsStore, [file_type(directory),
                                         access(read)]),
    pack_files(AbsStore, Packs),
    maplist(unpack_pack(AbsStore), Packs).

%!  unpack_pack(+Store, +Pack) is det.
%
%   Turn a pack back into a plain object files

unpack_pack(Store, PackFile) :-
    pack_read_header(PackFile, _Version, DataOffset, Objects),
    setup_call_cleanup(
        open(PackFile, read, In, [type(binary)]),
        foldl(create_file(Store, In), Objects, DataOffset, _),
        close(In)),
    detach_pack(Store, PackFile),
    delete_file(PackFile).

create_file(Store, In, obj(Object, _Type, _Size, FileSize), Offset0, Offset) :-
    Offset is Offset0+FileSize,
    gitty_object_file(Store, Object, Path),
    with_mutex(gitty_file, exists_or_recreate(Path, Out)),
	(   var(Out)
	->  true
	;   setup_call_cleanup(
                seek(In, Offset0, bof, Offset0),
                copy_stream_data(In, Out, FileSize),
                close(Out))
	).

exists_or_recreate(Path, _Out) :-
	exists_file(Path), !.
exists_or_recreate(Path, Out) :-
        file_directory_name(Path, Dir),
        make_directory_path(Dir),
        open(Path, write, Out, [type(binary), lock(write)]).


%!  remove_objects_after_pack(+Store, +Objects, +Options) is det.
%
%   Remove the indicated (file) objects from Store.

remove_objects_after_pack(Store, Objects, Options) :-
    debug(gitty(pack), 'Deleting plain files', []),
    maplist(delete_object(Store), Objects),
    (   option(prune_empty_directories(true), Options, true)
    ->  debug(gitty(pack), 'Pruning empty directories', []),
        prune_empty_directories(Store)
    ;   true
    ).

%!  remove_repacked_packs(+Store, +Packs, +Options)
%
%   Remove packs that have been repacked.

remove_repacked_packs(Store, Packs, Options) :-
    maplist(remove_pack(Store, Options), Packs).

remove_pack(Store, _Options, Pack) :-
    detach_pack(Store, Pack),
    delete_file(Pack).

%!  prune_empty_directories(+Dir) is det.
%
%   Prune directories that are  empty  below   Dir.  Dir  itself  is not
%   removed, even if it is empty.

prune_empty_directories(Dir) :-
    prune_empty_directories(Dir, 0).

prune_empty_directories(Dir, Level) :-
    directory_files(Dir, AllFiles),
    exclude(hidden, AllFiles, Files),
    (   Files == [],
        Level > 0
    ->  delete_directory_async(Dir)
    ;   convlist(prune_empty_directories(Dir, Level), Files, Left),
        (   Left == [],
            Level > 0
        ->  delete_directory_async(Dir)
        ;   true
        )
    ).

hidden(.).
hidden(..).

prune_empty_directories(Parent, Level0, File, _) :-
    directory_file_path(Parent, File, Path),
    exists_directory(Path),
    !,
    Level is Level0 + 1,
    prune_empty_directories(Path, Level),
    fail.
prune_empty_directories(_, _, File, File).

delete_directory_async(Dir) :-
    with_mutex(gitty_file, delete_directory_async2(Dir)).

delete_directory_async2(Dir) :-
    catch(delete_directory(Dir), E,
          (   \+ exists_directory(Dir)
          ->  true
          ;   \+ empty_directory(Dir)
          ->  true
          ;   throw(E)
          )).

empty_directory(Dir) :-
    directory_files(Dir, AllFiles),
    exclude(hidden, AllFiles, []).
