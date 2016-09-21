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

:- module(gitty_bdb,
	  [ gitty_file/3,		% +Store, ?Name, ?Hash
	    gitty_create/5,		% +Store, +Name, +Data, +Meta, -Commit
	    gitty_update/5,		% +Store, +Name, +Data, +Meta, -Commit
	    gitty_commit/3,		% +Store, +Name, -Meta
	    gitty_data/4,		% +Store, +Name, -Data, -Meta
	    gitty_history/4,		% +Store, +Name, -History, +Options
	    gitty_hash/2,		% +Store, ?Hash
	    gitty_reserved_meta/1,	% ?Key
	    gitty_close/1,		% +Store

	    gitty_diff/4,		% +Store, ?Start, +End, -Diff

	    data_diff/3,		% +String1, +String2, -Diff
	    udiff_string/2		% +Diff, -String
	  ]).
:- use_module(library(zlib)).
:- use_module(library(sha)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(debug)).
:- use_module(library(dcg/basics)).
:- use_module(library(memfile)).
:- use_module(library(bdb)).

/** <module> Single-file GIT like version system, BDB version

This library provides a first implementation  of a lightweight versioned
file store with dynamic meta-data. The   store  is partly modelled after
GIT. Like GIT, it uses  a  content-based   store.  In  fact,  the stored
objects are compatible  with  GIT.  Unlike   GIT  though,  there  are no
_trees_.  Each  entry  (file)  has  its  own  history.  Each  commit  is
associated  with  a  dict  that  can  carry  aribitrary  meta-data.  The
following fields are reserved for gitties bookkeeping:

  - name:Name
  Name of the entry (file)
  - time:TimeStamp
  Float representing when the object was added to the store
  - data:Hash
  Object hash of the contents
  - previous:Hash
  Hash of the previous commit.

The key =commit= is reserved and returned   as  part of the meta-data of
the newly created (gitty_create/5) or updated object (gitty_update/5).
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The BDB database file contains two databases:

  - =heads= maps a file name to the hash of the last object
  - =objects= contains the object blobs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


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
		   init_txn(true)
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

%%	gitty_create(+Store, +Name, +Data, +Meta, -Commit) is det.
%
%	Create a new object Name from Data and meta information.
%
%	@arg Commit is a dit describing the new Commit

gitty_create(Store, Name, _Data, _Meta, _) :-
	gitty_file(Store, Name, _Hash), !,
	throw(error(gitty(file_exists(Name)),_)).
gitty_create(Store, Name, Data, Meta, CommitRet) :-
	save_object(Store, Data, blob, Hash),
	get_time(Now),
	Commit = gitty{time:Now}.put(Meta)
		                .put(_{ name:Name,
					data:Hash
				      }),
	format(string(CommitString), '~q.~n', [Commit]),
	save_object(Store, CommitString, commit, CommitHash),
	CommitRet = Commit.put(commit, CommitHash),
	catch(gitty_update_head(Store, Name, -, CommitHash),
	      E,
	      ( delete_object(Store, CommitHash),
		throw(E))).

%%	gitty_update(+Store, +Name, +Data, +Meta, -Commit) is det.
%
%	Update document Name using Data and the given meta information

gitty_update(Store, Name, Data, Meta, CommitRet) :-
	gitty_file(Store, Name, OldHead),
	(   _{previous:OldHead} >:< Meta
	->  true
	;   throw(error(gitty(commit_version(OldHead, Meta.previous)), _))
	),
	load_plain_commit(Store, OldHead, OldMeta),
	get_time(Now),
	save_object(Store, Data, blob, Hash),
	Commit = gitty{}.put(OldMeta)
			.put(_{time:Now})
		        .put(Meta)
		        .put(_{ name:Name,
				data:Hash,
				previous:OldHead
			      }),
	format(string(CommitString), '~q.~n', [Commit]),
	save_object(Store, CommitString, commit, CommitHash),
	CommitRet = Commit.put(commit, CommitHash),
	catch(gitty_update_head(Store, Name, OldHead, CommitHash),
	      E,
	      ( delete_object(Store, CommitHash),
		throw(E))).

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

%%	gitty_data(+Store, +NameOrHash, -Data, -Meta) is semidet.
%
%	Get the data in object Name and its meta-data

gitty_data(Store, Name, Data, Meta) :-
	gitty_commit(Store, Name, Meta),
	load_object(Store, Meta.data, Data).

%%	gitty_commit(+Store, +NameOrHash, -Meta) is semidet.
%
%	True if Meta holds the commit data of NameOrHash. A key =commit=
%	is added to the meta-data to specify the commit hash.

gitty_commit(Store, Name, Meta) :-
	must_be(atom, Name),
	gitty_file(Store, Name, Head), !,
	load_commit(Store, Head, Meta).
gitty_commit(Store, Hash, Meta) :-
	load_commit(Store, Hash, Meta).

load_commit(Store, Hash, Meta) :-
	load_plain_commit(Store, Hash, Meta0),
	Meta1 = Meta0.put(commit, Hash),
	(   gitty_file(Store, Meta0.name, Hash)
	->  Meta = Meta1.put(symbolic, "HEAD")
	;   Meta = Meta1
	).

load_plain_commit(Store, Hash, Meta) :-
	load_object(Store, Hash, String),
	term_string(Meta, String, []).

%%	gitty_history(+Store, +NameOrHash, -History, +Options) is det.
%
%	History is a list of dicts representating the history of Name in
%	Store.  Options:
%
%	  - depth(+Depth)
%	  Number of entries in the history.  If not present, defaults
%	  to 5.
%	  - includes(+HASH)
%	  Ensure Hash is included in the history.  This means that the
%	  history includes the entry with HASH an (depth+1)//2 entries
%	  after the requested HASH.

gitty_history(Store, Name, History, Options) :-
	history_hash_start(Store, Name, Hash0),
	option(depth(Depth), Options, 5),
	(   option(includes(Hash), Options)
	->  read_history_to_hash(Store, Hash0, Hash, History0),
	    length(History0, Before),
	    After is max(Depth-Before, (Depth+1)//2),
	    read_history_depth(Store, Hash, After, History1),
	    append(History0, History1, History2),
	    list_prefix(Depth, History2, History)
	;   read_history_depth(Store, Hash0, Depth, History)
	).

history_hash_start(Store, Name, Hash) :-
	gitty_file(Store, Name, Head), !,
	Hash = Head.
history_hash_start(_, Hash, Hash).


read_history_depth(_, _, 0, []) :- !.
read_history_depth(Store, Hash, Left, [H|T]) :-
	load_commit(Store, Hash, H), !,
	Left1 is Left-1,
	(   read_history_depth(Store, H.get(previous), Left1, T)
	->  true
	;   T = []
	).
read_history_depth(_, _, _, []).

%%	read_history_to_hash(+Store, +Start, +Upto, -History)
%
%	Read the history upto, but NOT including Upto.

read_history_to_hash(Store, Hash, Upto, [H|T]) :-
	Upto \== Hash,
	load_commit(Store, Hash, H),
	(   read_history_to_hash(Store, H.get(previous), Upto, T)
	->  true
	;   T = []
	).
read_history_to_hash(_, _, _, []).

list_prefix(0, _, []) :- !.
list_prefix(_, [], []) :- !.
list_prefix(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	list_prefix(N2, T0, T).


%%	save_object(+Store, +Data:string, +Type, -Hash) is det.
%
%	Save an object in a git compatible   way. Data provides the data
%	as a string.
%
%	@see http://www.gitguys.com/topics/what-is-the-format-of-a-git-blob/
%	@bug We currently delete objects if the head cannot be moved.
%	This can lead to a race condition.   We need to leave that
%	to GC.

save_object(Store, Data, Type, Hash) :-
	sha_new_ctx(Ctx0, []),
	size_in_bytes(Data, Size),
	format(string(Hdr), '~w ~d\u0000', [Type, Size]),
	sha_hash_ctx(Ctx0, Hdr, Ctx1, _),
	sha_hash_ctx(Ctx1, Data, _, HashBin),
	hash_atom(HashBin, Hash),
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
	    free_memory_file(MF)),
	asserta(data(Header, Data, String)).


size_in_bytes(Data, Size) :-
	setup_call_cleanup(
	    open_null_stream(Out),
	    ( format(Out, '~s', [Data]),
	      byte_count(Out, Size)
	    ),
	    close(Out)).

%%	load_object(+Store, +Hash, -Data) is det.
%%	load_object(+Store, +Hash, -Data, -Type, -Size) is det.
%
%	Load the given object.

load_object(Store, Hash, Data) :-
	load_object(Store, Hash, Data, _, _).
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

%%	gitty_reserved_meta(?Key) is nondet.
%
%	True when Key is a gitty reserved key for the commit meta-data

gitty_reserved_meta(name).
gitty_reserved_meta(time).
gitty_reserved_meta(data).
gitty_reserved_meta(previous).


:- multifile
	prolog:error_message//1.

prolog:error_message(gitty(not_at_head(Name, _OldCommit))) -->
	[ 'Gitty: cannot update head for "~w" because it was \c
	   updated by someone else'-[Name] ].


		 /*******************************
		 *	       DIFF		*
		 *******************************/

%%	gitty_diff(+Store, ?Hash1, +FileOrHash2, -Dict) is det.
%
%	True if Dict representeds the changes   in Hash1 to FileOrHash2.
%	If Hash1 is unbound,  it  is   unified  with  the  `previous` of
%	FileOrHash2. Returns _{initial:true} if  Hash1   is  unbound and
%	FileOrHash2 is the initial commit.  Dict contains:
%
%	  - from:Meta1
%	  - to:Meta2
%	  Meta-data for the two diffed versions
%	  - data:UDiff
%	  String holding unified diff representation of changes to the
%	  data.  Only present of data has changed
%	  - tags:_{added:AddedTags, deleted:DeletedTags}
%	  If tags have changed, the added and deleted ones.

gitty_diff(Store, C1, C2, Dict) :-
	gitty_data(Store, C2, Data2, Meta2),
	(   var(C1)
	->  C1 = Meta2.get(previous)
	;   true
	), !,
	gitty_data(Store, C1, Data1, Meta1),
	Pairs = [ from-Meta1, to-Meta2|_],
	(   Data1 \== Data2
	->  udiff_string(Data1, Data2, UDIFF),
	    memberchk(data-UDIFF, Pairs)
	;   true
	),
	meta_tag_set(Meta1, Tags1),
	meta_tag_set(Meta2, Tags2),
	(   Tags1 \== Tags2
	->  ord_subtract(Tags1, Tags2, Deleted),
	    ord_subtract(Tags2, Tags1, Added),
	    memberchk(tags-_{added:Added, deleted:Deleted}, Pairs)
	;   true
	),
	once(length(Pairs,_)),			% close list
	dict_pairs(Dict, json, Pairs).
gitty_diff(_Store, '0000000000000000000000000000000000000000', _C2,
	   json{initial:true}).


meta_tag_set(Meta, Tags) :-
	sort(Meta.get(tags), Tags), !.
meta_tag_set(_, []).

%%	udiff_string(+Data1, +Data2, -UDIFF) is det.
%
%	Produce a unified difference between two   strings. Note that we
%	can avoid one temporary file using diff's `-` arg and the second
%	by    passing    =/dev/fd/NNN=    on    Linux    systems.    See
%	http://stackoverflow.com/questions/3800202

:- if(true).

udiff_string(Data1, Data2, UDIFF) :-
	setup_call_cleanup(
	    save_string(Data1, File1),
	    setup_call_cleanup(
		save_string(Data2, File2),
		process_diff(File1, File2, UDIFF),
		delete_file(File2)),
	    delete_file(File1)).

save_string(String, File) :-
	tmp_file_stream(utf8, File, TmpOut),
	format(TmpOut, '~s', [String]),
	close(TmpOut).

process_diff(File1, File2, String) :-
	setup_call_cleanup(
	    process_create(path(diff),
			   ['-u', file(File1), file(File2)],
			   [ stdout(pipe(Out)),
			     process(PID)
			   ]),
	    read_string(Out, _, String),
	    ( close(Out),
	      process_wait(PID, Status)
	    )),
	assertion(normal_diff_exit(Status)).

normal_diff_exit(exit(0)).		% equal
normal_diff_exit(exit(1)).		% different

:- else.

udiff_string(Data1, Data2, UDIFF) :-
	data_diff(Data1, Data2, Diffs),
	maplist(udiff_string, Diffs, Strings),
	atomics_to_string(Strings, UDIFF).

:- endif.


		 /*******************************
		 *	   PROLOG DIFF		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attempt at a built-in diff utility. Doing   it in Prolog may seem weird,
but is good for tasting  ones  own   dog  food.  In  addition, it avoids
temporary files and relatively expensive fork()  calls. As it turns out,
implementing an efficient LCS (Longest  Common   Sequence)  in Prolog is
rather hard. We'll leave the  code  for   reference,  but  might  seek a
different solution for the real thing.  Options are:

  - Use external diff after all
  - Add a proper Prolog implementation of LCS
  - Add LCS in C.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%%	data_diff(+Data1, +Data2, -UDiff) is det.
%
%	Diff two data strings line-by-line. UDiff is  a list of terms of
%	the form below, where `L1` and `L2` provide the starting line in
%	Data1 and Data2 and `S1` and `S2` provide the number of affected
%	lines.
%
%	  ==
%	  udiff(L1,S1,L2,S2,Diff)
%	  ==
%
%	`Diff` is a list holding
%
%	  - +(Line)
%	  Line was added to Data1 to get Data2
%	  - -(Line)
%	  Line was deleted from Data1 to get Data2
%	  - -(Line1,Line2)
%	  Line was replaced
%	  - =(Line)
%	  Line is identical (context line).

data_diff(Data, Data, UDiff) :- !,
	UDiff = [].
data_diff(Data1, Data2, Diff) :-
	split_string(Data1, "\n", "", List1),
	split_string(Data2, "\n", "", List2),
	list_diff(List1, List2, Diff).

list_diff(List1, List2, UDiff) :-
	list_lcs(List1, List2, Lcs),
	make_diff(List1, List2, Lcs, c(), 1, 1, Diff),
	join_diff(Diff, UDiff).

%%	make_diff(+List1, +List2, +Lcs, +Context0, +Line1, +Line2, -Diff)

make_diff([], [], [], _, _, _, []) :- !.
make_diff([H|T1], [H|T2], [H|C], c(_,C0,C1), L1, L2, Diff) :- !,
	L11 is L1+1,
	L21 is L2+1,
	make_diff(T1, T2, C, c(C0,C1,H), L11, L21, Diff).
make_diff([H|T1], [H|T2], [H|C], C0, L1, L2, Diff) :- !,
	L11 is L1+1,
	L21 is L2+1,
	add_context(C0, H, C1),
	(   compound_name_arity(C1, _, L1)
	->  Diff = Diff1
	;   Diff = [=(H)|Diff1]
	),
	make_diff(T1, T2, C, C1, L11, L21, Diff1).
make_diff([H|T1], [H2|T2], [H|C], C0, L1, L2, [d(L1,L2,C0,+H2)|Diff]) :- !,
	L21 is L2+1,
	make_diff([H|T1], T2, [H|C], c(), L1, L21, Diff).
make_diff([], [H2|T2], [], C0, L1, L2, [d(L1,L2,C0,+H2)|Diff]) :- !,
	L21 is L2+1,
	make_diff([], T2, [], c(), L1, L21, Diff).
make_diff([H1|T1], [H|T2], [H|C], C0, L1, L2, [d(L1,L2,C0,-H1)|Diff]) :- !,
	L11 is L1+1,
	make_diff(T1, [H|T2], [H|C], c(), L11, L2, Diff).
make_diff([H1|T1], [], [], C0, L1, L2, [d(L1,L2,C0,-H1)|Diff]) :- !,
	L11 is L1+1,
	make_diff(T1, [], [], c(), L11, L2, Diff).
make_diff([H1|T1], [H2|T2], C, C0, L1, L2, [d(L1,L2,C0,H1-H2)|Diff]) :- !,
	L11 is L1+1,
	L21 is L2+1,
	make_diff(T1, T2, C, c(), L11, L21, Diff).

add_context(c(_,B,C),N,c(B,C,N)).
add_context(c(A,B),  N,c(A,B,N)).
add_context(c(A),    N,c(A,N)).
add_context(c(),     N,c(N)).

%%	join_diff(+Diff, -UDiff) is det.

join_diff([], []).
join_diff([d(L10,L20,C,L)|T0], [udiff(L1,S1,L2,S2,Diff)|T]) :-
	pre_context(C, S0, Diff, [L|DiffT]),
	L1 is L10-S0,
	L2 is L20-S0,
	diff_affected(L,S10,S20),
	S11 is S10+S0,
	S21 is S20+S0,
	collect_diff(T0,S11,S21,S1,S2,0,DiffT,T1),
	join_diff(T1, T).

pre_context(c(),      0, L, L).
pre_context(c(A),     1, [=(A)|L], L).
pre_context(c(A,B),   2, [=(A),=(B)|L], L).
pre_context(c(A,B,C), 3, [=(A),=(B),=(C)|L], L).

collect_diff([d(_,_,_,L)|T0], S10,S20,S1,S2,C,[L|Diff],T) :-
	C < 3, !,
	diff_affected(L,S1x,S2x),
	S11 is S10+S1x,
	S21 is S20+S2x,
	collect_diff(T0,S11,S21,S1,S2,0,Diff,T).
collect_diff([=(L)|T0], S10,S20,S1,S2,C0,[=(L)|Diff],T) :- !,
	S11 is S10+1,
	S21 is S20+1,
	C1 is C0+1,
	collect_diff(T0,S11,S21,S1,S2,C1,Diff,T).
collect_diff(T,S1,S2,S1,S2,_,[],T).

diff_affected(+(_),   0, 1).
diff_affected(-(_),   0, 1).
diff_affected(-(_,_), 1, 1).

%%	udiff_string(+UDiff, -String) is det.
%
%	True when String is the string representation of UDiff.

udiff_string(udiff(L1,S1,L2,S2,Diff), Final) :-
	format(string(Hdr), '@@ -~d,~d +~d,~d @@', [L1,S1,L2,S2]),
	udiff_blocks(Diff, Blocks),
	maplist(block_lines, Blocks, LineSets),
	append(LineSets, Lines),
	atomics_to_string([Hdr|Lines], "\n", Final).

block_lines(=(U), Lines) :- maplist(string_concat(' '), U, Lines).
block_lines(+(U), Lines) :- maplist(string_concat('+'), U, Lines).
block_lines(-(U), Lines) :- maplist(string_concat('-'), U, Lines).

udiff_blocks([], []) :- !.
udiff_blocks([=(H)|T0], [=([H|E])|T]) :- !,
	udiff_cp(T0, E, T1),
	udiff_blocks(T1, T).
udiff_blocks(U, List) :-
	udiff_block(U, D, A, T1),
	udiff_add(D,A,List,ListT),
	udiff_blocks(T1, ListT).

udiff_add([],A,[+A|T],T) :- !.
udiff_add(D,[],[-D|T],T) :- !.
udiff_add(D,A,[-D,+A|T],T).

udiff_cp([=(H)|T0], [H|E], T) :- !,
	udiff_cp(T0, E, T).
udiff_cp(L, [], L).

udiff_block([-L|T], [L|D], A, Rest) :- !,
	udiff_block(T, D, A, Rest).
udiff_block([+L|T], D, [L|A], Rest) :- !,
	udiff_block(T, D, A, Rest).
udiff_block([L1-L2|T], [L1|D], [L2|A], Rest) :- !,
	udiff_block(T, D, A, Rest).
udiff_block(T, [], [], T).

%%	list_lcs(+List1, +List2, -Lcs) is det.
%
%	@tbd	Too slow.  See http://wordaligned.org/articles/longest-common-subsequence

:- thread_local lcs_db/2.

list_lcs([], [], []) :- !.
list_lcs([H|L1], [H|L2], [H|Lcs]) :- !,
	list_lcs(L1, L2, Lcs).
list_lcs(List1, List2, Lcs) :-
	reverse(List1, Rev1),
	reverse(List2, Rev2),
	copy_prefix(Rev1, Rev2, RevDiff1, RevDiff2, RevLcs, RevT),
	list_lcs2(RevDiff1, RevDiff2, RevT),
	reverse(RevLcs, Lcs).

list_lcs2(List1, List2, Lcs) :-
	variant_sha1(List1+List2, Hash),
	call_cleanup(
	    lcs(List1, List2, Hash, Lcs),
	    retractall(lcs_db(_,_))).

copy_prefix([H|T1], [H|T2], L1, L2, [H|L], LT) :- !,
	copy_prefix(T1, T2, L1, L2, L, LT).
copy_prefix(R1, R2, R1, R2, L, L).


lcs(_,_,Hash,Lcs) :-
	lcs_db(Hash,Lcs), !.
lcs([H|L1], [H|L2], _, [H|Lcs]) :- !,
	variant_sha1(L1+L2,Hash),
	lcs(L1, L2, Hash, Lcs).
lcs(List1, List2, Hash, Lcs) :-
	List1 = [H1|L1],
	List2 = [H2|L2],
	variant_sha1(L1+[H2|L2],Hash1),
	variant_sha1([H1|L1]+L2,Hash2),
	lcs(    L1 , [H2|L2], Hash1, Lcs1),
	lcs([H1|L1],     L2 , Hash2, Lcs2),
	longest(Lcs1, Lcs2, Lcs),!,
	asserta(lcs_db(Hash, Lcs)).
lcs(_,_,_,[]).

longest(L1, L2, Longest) :-
	length(L1, Length1),
	length(L2, Length2),
	(   Length1 > Length2
	->  Longest = L1
	;   Longest = L2
	).
