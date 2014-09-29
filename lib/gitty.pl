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

:- module(gitty,
	  [ gitty_file/3,		% +Store, ?Name, ?Hash
	    gitty_create/5,		% +Store, +Name, +Data, +Meta, -Commit
	    gitty_update/5,		% +Store, +Name, +Data, +Meta, -Commit
	    gitty_commit/3,		% +Store, +Name, -Meta
	    gitty_data/4,		% +Store, +Name, -Data, -Meta
	    gitty_history/4,		% +Store, +Name, +Max, -History
	    gitty_scan/1,		% +Store
	    gitty_hash/2,		% +Store, ?Hash

	    data_diff/3,		% +String1, +String2, -Diff
	    udiff_string/2		% +Diff, -String
	  ]).
:- use_module(library(zlib)).
:- use_module(library(filesex)).
:- use_module(library(sha)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

/** <module> Single-file GIT like version system

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

:- dynamic
	head/3,				% Store, Name, Hash
	store/1.			% Store
:- volatile
	head/3,
	store/1.

%%	gitty_file(+Store, ?File, ?Head) is nondet.
%
%	True when File entry in the  gitty   store  and Head is the HEAD
%	revision.

gitty_file(Store, Head, Hash) :-
	gitty_scan(Store),
	head(Store, Head, Hash).

%%	gitty_create(+Store, +Name, +Data, +Meta, -Commit) is det.
%
%	Create a new object Name from Data and meta information.

gitty_create(Store, Name, _Data, _Meta, _) :-
	gitty_scan(Store),
	head(Store, Name, _), !,
	throw(error(gitty(file_exists(Name)),_)).
gitty_create(Store, Name, Data, Meta, CommitRet) :-
	save_object(Store, Data, blob, Hash),
	get_time(Now),
	Commit = gitty{}.put(Meta)
		        .put(_{ name:Name,
				time:Now,
				data:Hash
			      }),
	format(string(CommitString), '~q.~n', [Commit]),
	save_object(Store, CommitString, commit, CommitHash),
	CommitRet = Commit.put(commit, CommitHash),
	with_mutex(gitty,
		   (   head(Store, Name, _)
		   ->  delete_object(Store, CommitHash),
		       throw(error(gitty(file_exists(Name),_)))
		   ;   assertz(head(Store, Name, CommitHash))
		   )).

%%	gitty_update(+Store, +Name, +Data, +Meta, -Commit) is det.
%
%	Update document Name using Data and the given meta information

gitty_update(Store, Name, Data, Meta, CommitRet) :-
	gitty_scan(Store),
	head(Store, Name, OldHead),
	(   _{previous:OldHead} >:< Meta
	->  true
	;   throw(error(gitty(commit_version(OldHead, Meta.previous)), _))
	),
	load_commit(Store, OldHead, OldMeta),
	get_time(Now),
	save_object(Store, Data, blob, Hash),
	Commit = gitty{}.put(OldMeta)
		        .put(Meta)
		        .put(_{ name:Name,
				time:Now,
				data:Hash,
				previous:OldHead
			      }),
	format(string(CommitString), '~q.~n', [Commit]),
	save_object(Store, CommitString, commit, CommitHash),
	CommitRet = Commit.put(commit, CommitHash),
	with_mutex(gitty,
		   (   retract(head(Store, Name, OldHead))
		   ->  assertz(head(Store, Name, CommitHash))
		   ;   delete_object(Store, CommitHash),
		       throw(error(gitty(not_at_head(OldHead)), _))
		   )).

%%	gitty_data(+Store, +NameOrHash, -Data, -Meta) is semidet.
%
%	Get the data in object Name and its meta-data

gitty_data(Store, Name, Data, Meta) :-
	gitty_scan(Store),
	head(Store, Name, Head), !,
	load_commit(Store, Head, Meta),
	load_object(Store, Meta.data, Data).
gitty_data(Store, Hash, Data, Meta) :-
	load_commit(Store, Hash, Meta),
	load_object(Store, Meta.data, Data).

%%	gitty_commit(+Store, +NameOrHash, -Meta) is semidet.
%
%	True if Meta holds the commit data of NameOrHash.

gitty_commit(Store, Name, Meta) :-
	gitty_scan(Store),
	head(Store, Name, Head), !,
	load_commit(Store, Head, Meta).
gitty_commit(Store, Hash, Meta) :-
	load_commit(Store, Hash, Meta).


load_commit(Store, Head, Meta) :-
	load_object(Store, Head, String),
	term_string(Meta, String, []).

%%	gitty_history(+Store, +NameOrHash, +Max, -History) is det.
%
%	History is a list of dicts representating the history of Name in
%	Store.

gitty_history(Store, Name, Max, [Meta|History]) :-
	gitty_scan(Store),
	head(Store, Name, Head), !,
	load_commit(Store, Head, Meta),
	history(Store, Meta, Max, History).
gitty_history(Store, Hash, Max, [Meta|History]) :-
	load_commit(Store, Hash, Meta),
	history(Store, Meta, Max, History).


history(Store, Meta, Max, [Prev|History]) :-
	succ(Max1, Max),
	load_commit(Store, Meta.get(previous), Prev), !,
	history(Store, Prev, Max1, History).
history(_, _, _, []).


%%	save_object(+Store, +Data, +Type, -Hash)
%
%	Save an object in a git compatible   way. Data provides the data
%	as a string.
%
%	@see http://www.gitguys.com/topics/what-is-the-format-of-a-git-blob/

save_object(Store, Data, Type, Hash) :-
	sha_new_ctx(Ctx0, []),
	size_in_bytes(Data, Size),
	format(string(Hdr), '~w ~d\u0000', [Type, Size]),
	sha_hash_ctx(Ctx0, Hdr, Ctx1, _),
	sha_hash_ctx(Ctx1, Data, _, HashBin),
	hash_atom(HashBin, Hash),
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

size_in_bytes(Data, Size) :-
	setup_call_cleanup(
	    open_null_stream(Out),
	    ( format(Out, '~s', [Data]),
	      byte_count(Out, Size)
	    ),
	    close(Out)).

ensure_directory(Dir) :-
	exists_directory(Dir), !.
ensure_directory(Dir) :-
	make_directory(Dir).

%%	load_object(+Store, +Hash, -Data) is det.
%%	load_object(+Store, +Hash, -Data, -Type, -Size) is det.
%
%	Load the given object.

load_object(Store, Hash, Data) :-
	load_object(Store, Hash, Data, _, _).
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

%%	gitty_scan(+Store) is det.
%
%	Scan gitty store for files (entries),   filling  head/3. This is
%	performed lazily at first access to the store.
%
%	@tdb	Possibly we need to maintain a cached version of this
%		index to avoid having to open all objects of the gitty
%		store.

gitty_scan(Store) :-
	store(Store), !.
gitty_scan(Store) :-
	with_mutex(gitty, gitty_scan_sync(Store)).

gitty_scan_sync(Store) :-
	store(Store), !.
gitty_scan_sync(Store) :-
	(   gitty_hash(Store, Hash),
	    load_object(Store, Hash, Data, commit, _Size),
	    term_string(Meta, Data, []),
	    (	head(Store, Meta.name, OldHash)
	    ->	(   OldHash == Meta.get(previous)
		->  retract(head(Store, Meta.name, OldHash)),
		    assertz(head(Store, Meta.name, Hash))
		;   true
		)
	    ;	assertz(head(Store, Meta.name, Hash))
	    ),
	    fail
	;   assertz(store(Store))
	).


%%	gitty_hash(+Store, ?Hash) is nondet.
%
%	True when Hash is an object in the store.

gitty_hash(Store, Hash) :-
	var(Hash), !,
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
		 *	       DIFF		*
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
%	  Line as added to Data1 to get Data2
%	  - -(Line)
%	  Line was deleted from Data1 to get Data2
%	  - -(Line1,Line2)
%	  Line as replaced
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

udiff_blocks([], []).
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
