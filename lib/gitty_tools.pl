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

:- module(gitty_tools,
	  [ gitty_copy_store/3,		% StoreIn, StoreOut, Driver
	    gitty_compare_stores/2	% Store1, Store2
	  ]).
:- use_module(gitty).
:- use_module(library(apply)).
:- use_module(library(option)).

/** <module> Gitty maintenance tools

This file contains some maintenance  predicates   for  gitty  stores. It
notably allows for copying, synchronizing and comparing stores.
*/

%%	gitty_copy_store(+StoreIn, +StoreOut, +Driver) is det.
%
%	Copy a gitty store, using Driver   for the target StoreOut. This
%	copies the entire history  and  data   of  StoreIn  to StoreOut,
%	possibly transcribing to another driver.   Note  that the hashes
%	are independent from the driver.
%
%	Note that gitty_copy_store/3 can also be used to migrate updates
%	from StoreIn to an older copy StoreOut.

gitty_copy_store(StoreIn, StoreOut, Driver) :-
	gitty_open(StoreIn, []),
	gitty_open(StoreOut, [driver(Driver)]),
	State = state(0),
	(   gitty_file(StoreIn, File, _),
	    State = state(N0),
	    N is N0+1,
	    nb_setarg(1, State, N),
	    format(user_error, '~N~D ~`.t ~q ~50|', [N, File]),
	    (	copy_file(File, StoreIn, StoreOut)
	    ->	fail
	    ;	format('Failed to copy ~q~n', [File])
	    )
	;   true
	).

copy_file(File, StoreIn, StoreOut) :-
	gitty_history(StoreIn, File, History, [depth(1000000)]),
	reverse(History, LastToFirst),
	maplist(copy_commit(StoreIn, StoreOut), LastToFirst).

copy_commit(_StoreIn, StoreOut, Commit) :-
	gitty_hash(StoreOut, Commit.commit), !,
	put_char(user_error, '+').
copy_commit(StoreIn, StoreOut, Commit) :-
	gitty_data(StoreIn, Commit.commit, Data, Meta0),
	del_keys([commit, symbolic], Meta0, Meta),
	(   Prev = Meta.get(previous),
	    gitty_commit(StoreIn, Prev, PrevCommit),
	    PrevCommit.name == Meta.name
	->  gitty_update(StoreOut, Meta.name, Data, Meta, _)
	;   gitty_create(StoreOut, Meta.name, Data, Meta, _)
	),
	put_char(user_error, '.').

del_keys([], Dict, Dict).
del_keys([H|T], Dict0, Dict) :-
	del_dict(H, Dict0, _, Dict1), !,
	del_keys(T, Dict1, Dict).
del_keys([_|T], Dict0, Dict) :-
	del_keys(T, Dict0, Dict).

%%	gitty_compare_stores(+Store1, +Store2) is semidet.
%
%	True if both stores are exactly the same.
%
%	@bug	Should (optionally) describe the differences

gitty_compare_stores(Store1, Store2) :-
	gitty_open(Store1, []),
	gitty_open(Store2, []),
	gitty_full_history(Store1, History1),
	gitty_full_history(Store2, History2),
	History1 == History2.

gitty_full_history(Store, History) :-
	setof(File, Hash^gitty_file(Store, File, Hash), Files),
	maplist(gitty_full_history(Store), Files, History).

gitty_full_history(Store, File, History) :-
	gitty_history(Store, File, History, [depth(1000000)]).

%%	gitty_fsck(+Store, +File, +Options)
%
%	Check integrity of the store.  Requires the following step:
%
%	  - Validate each object
%	    - Does hash match content?
%	  - Validate each commit
%	    - Does the data exists?
%	    - Does previous exist?
%	  - Reconstruct heads

gitty_fsck(Store) :-
	check_objects(Store).

check_objects(Store) :-
	aggregate_all(count,
		      ( gitty_hash(Store, Hash),
			check_object(Store, Hash)
		      ), Objects),
	progress(checked_objects(Objects)).

check_object(Store, Hash) :-
	gitty:fsck_object(Store, Hash), !.
check_object(Store, Hash) :-
	gripe(bad_object(Store, Hash)).



gitty_fsck(Store, File, Options) :-
	gitty_file(Store, File, Head),
	check_commit(Store, File, Head, Options).

check_commit(Store, File, Head, Options) :-
	(   gitty_commit(Store, Head, Commit)
	->  (   gitty_hash(Store, Commit.data)
	    ->	true
	    ;	fix(gitty:delete_object(Store, Head), Options),
	        gripe(no_data(File, Commit.data))
	    ),
	    (   Prev = Commit.get(previous)
	    ->  check_commit(Store, Commit.name, Prev, Options)
	    ;   true
	    )
	;   fix(gitty:delete_head(Store, Head), Options),
	    gripe(no_commit(Store, File, Head))
	).

:- meta_predicate
	fix(0, +).

fix(Goal, Options) :-
	option(fix(true), Options), !,
	call(Goal).
fix(_, _).


gripe(Term) :-
	print_message(error, gitty(Term)).
progress(Term) :-
	print_message(informational, gitty(Term)).

:- multifile prolog:message//1.

prolog:message(gitty(Term)) -->
	gitty_message(Term).

gitty_message(no_commit(Store, File, Head)) -->
	[ '~p: file ~p: missing commit object ~p'-[Store, File, Head] ].
gitty_message(bad_object(Store, Hash)) -->
	[ '~p: ~p: corrupt object'-[Store, Hash] ].
gitty_message(checked_objects(Count)) -->
	[ 'Checked ~D objects'-[Count] ].
