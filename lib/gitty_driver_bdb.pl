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

:- module(gitty_driver_bdb,
	  [ gitty_close/1,		% +Store
	    gitty_file/3,		% +Store, ?Name, ?Hash

	    gitty_update_head/4,	% +Store, +Name, +OldCommit, +NewCommit
	    delete_head/2,		% +Store, +Name
	    set_head/3,			% +Store, +Name, +Hash
	    store_object/4,		% +Store, +Hash, +Header, +Data
	    delete_object/2,		% +Store, +Hash

	    gitty_hash/2,		% +Store, ?Hash
	    load_plain_commit/3,	% +Store, +Hash, -Meta
	    load_object/5		% +Store, +Hash, -Data, -Type, -Size
	  ]).
:- use_module(library(zlib)).
:- use_module(library(dcg/basics)).
:- use_module(library(memfile)).
:- use_module(library(bdb)).

/** <module> Gitty BDB driver

This version of the driver  uses   library(bdb),  the BerkeyDB database.
This driver is particularly suited for large numbers of files. The store
uses less disk space and starts much   faster on large numbers of files.

The BDB database file contains two databases:

  - =heads= maps a file name to the hash of the last object
  - =objects= contains the object blobs.
*/


:- dynamic
	bdb_env/2,			% Store, Env
	bdb_db/3.			% Store, Database, Handle
:- volatile
	bdb_env/2,
	bdb_db/3.


bdb_handle(Store, Database, Handle) :-
	bdb_db(Store, Database, Handle), !.
bdb_handle(Store, Database, Handle) :-
	with_mutex(gitty_bdb, bdb_handle_sync(Store, Database, Handle)).

bdb_handle_sync(Store, Database, Handle) :-
	bdb_db(Store, Database, Handle), !.
bdb_handle_sync(Store, Database, Handle) :-
	bdb_store(Store, Env),
	db_types(Database, KeyType, ValueType),
	bdb_open(Database, update, Handle,
		 [ environment(Env),
		   key(KeyType),
		   value(ValueType)
		 ]),
	asserta(bdb_db(Store, Database, Handle)).

db_types(heads,   atom, atom).		% Name --> Hash
db_types(objects, atom, c_blob).	% Hash --> Blob

%%	bdb_store(+Store, -Env) is det.
%
%	Get the BDB environment for Store.

bdb_store(Store, Env) :-
	bdb_env(Store, Env), !.
bdb_store(Store, Env) :-
	with_mutex(gitty_bdb, bdb_store_sync(Store, Env)).

bdb_store_sync(Store, Env) :-
	bdb_env(Store, Env), !.
bdb_store_sync(Store, Env) :-
	ensure_directory(Store),
	bdb_init(Env,
		 [ home(Store),
		   create(true),
		   thread(true),
		   init_txn(true),
		   recover(true),
		   register(true)
		 ]),
	asserta(bdb_env(Store, Env)).

ensure_directory(Dir) :-
	exists_directory(Dir), !.
ensure_directory(Dir) :-
	make_directory(Dir).

%%	gitty_close(+Store) is det.
%
%	Close the BDB environment associated with a gitty store

gitty_close(Store) :-
	with_mutex(gitty_bdb, gitty_close_sync(Store)).

gitty_close_sync(Store) :-
	(   retract(bdb_env(Store, Env))
	->  bdb_close_environment(Env)
	;   true
	).


%%	gitty_file(+Store, ?File, ?Head) is nondet.
%
%	True when File entry in the  gitty   store  and Head is the HEAD
%	revision.

gitty_file(Store, Head, Hash) :-
	bdb_handle(Store, heads, H),
	(   nonvar(Head)
	->  bdb_get(H, Head, Hash)
	;   bdb_enum(H, Head, Hash)
	).

%%	gitty_update_head(+Store, +Name, +OldCommit, +NewCommit) is det.
%
%	Update the head of a gitty  store   for  Name.  OldCommit is the
%	current head and NewCommit is the new  head. If Name is created,
%	and thus there is no head, OldCommit must be `-`.
%
%	This operation can fail because another   writer has updated the
%	head.  This can both be in-process or another process.
%
%	@error gitty(file_exists(Name) if the file already exists
%	@error gitty(not_at_head(Name, OldCommit) if the head was moved
%	       by someone else.

gitty_update_head(Store, Name, OldCommit, NewCommit) :-
	bdb_store(Store, Env),
	bdb_transaction(
	    Env,
	    gitty_update_head_sync(Store, Name, OldCommit, NewCommit)).

gitty_update_head_sync(Store, Name, OldCommit, NewCommit) :-
	bdb_handle(Store, heads, BDB),
	(   OldCommit == (-)
	->  (   bdb_get(BDB, Name, _)
	    ->	throw(error(gitty(file_exists(Name),_)))
	    ;	bdb_put(BDB, Name, NewCommit)
	    )
	;   (   bdb_get(BDB, Name, OldCommit)
	    ->	bdb_put(BDB, Name, NewCommit)
	    ;	throw(error(gitty(not_at_head(Name, OldCommit)), _))
	    )
	).

%%	delete_head(+Store, +Name) is det.
%
%	Delete the named head.

delete_head(Store, Name) :-
	bdb_handle(Store, heads, BDB),
	bdb_del(BDB, Name, _Old).

%%	set_head(+Store, +File, +Hash) is det.
%
%	Set the head of the given File to Hash

set_head(Store, File, Hash) :-
	bdb_handle(Store, heads, BDB),
	bdb_put(BDB, File, Hash).

%%	load_plain_commit(+Store, +Hash, -Meta:dict) is semidet.
%
%	Load the commit data as a dict. Fails  if Hash does not exist or
%	is not a commit.

load_plain_commit(Store, Hash, Meta) :-
	load_object(Store, Hash, String, commit, _Size),
	term_string(Meta, String, []).

%%	store_object(+Store, +Hash, +Header:string, +Data:string) is det.
%
%	Store the actual object. The store  must associate Hash with the
%	concatenation of Hdr and Data.

store_object(Store, Hash, Hdr, Data) :-
	compress_string(Hdr, Data, Object),
	bdb_handle(Store, objects, BDB),
	bdb_put(BDB, Hash, Object).

compress_string(Header, Data, String) :-
	setup_call_cleanup(
	    new_memory_file(MF),
	    ( setup_call_cleanup(
		  open_memory_file(MF, write, Out, [encoding(utf8)]),
		  setup_call_cleanup(
		      zopen(Out, OutZ, [ format(gzip),
					 close_parent(false)
				       ]),
		      format(OutZ, '~s~s', [Header, Data]),
		    close(OutZ)),
		  close(Out)),
	      memory_file_to_string(MF, String, octet)
	    ),
	    free_memory_file(MF)).

%%	load_object(+Store, +Hash, -Data, -Type, -Size) is det.
%
%	Load an object given its  Hash.  Data   holds  the  content as a
%	string, Type is the object type (an   atom) and Size is the size
%	of the object in bytes.

load_object(Store, Hash, Data, Type, Size) :-
	bdb_handle(Store, objects, BDB),
	bdb_get(BDB, Hash, Blob),
	setup_call_cleanup(
	    open_string(Blob, In),
	    setup_call_cleanup(
		zopen(In, InZ, [ format(gzip),
				 close_parent(false)
			       ]),
		( set_stream(InZ, encoding(utf8)),
		  read_object(InZ, Data, Type, Size)
		),
		close(InZ)),
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

%%	gitty_hash(+Store, ?Hash) is nondet.
%
%	True when Hash is an object in the store.

gitty_hash(Store, Hash) :-
	bdb_handle(Store, objects, BDB),
	(   nonvar(Hash)
	->  bdb_get(BDB, Hash, _)
	;   bdb_enum(BDB, Hash, _)
	).

%%	delete_object(+Store, +Hash)
%
%	Delete an existing object

delete_object(Store, Hash) :-
	bdb_handle(Store, objects, BDB),
	bdb_del(BDB, Hash, _).
