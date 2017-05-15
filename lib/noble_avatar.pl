/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam
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

:- module(noble_avatar,
	  [ noble_avatar/2,			% ?Gender, -File
	    noble_avatar/3,			% ?Gender, -File, ?New
	    create_avatar/2,			% +PNG, -File

	    existing_noble_avatar/2		% -Gender, -File
	  ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(filesex)).
:- use_module(library(process)).

/** <module> Noble Avatar generator

This library generates random avatar images   from  components. The file
locations of the library are  defined   by  file_search_path/2 rules for
`noble_avatar_components` (the components) and   `noble_avatar`  for the
generated avatars.

The probalities for the various components are specified in part/1.


## Credits and license

The component images can  be  downloaded   from  the  address below. The
images are licensed under CC-BY-3.0.  If you use this library with these
images, please include the following acknowledgements:

  1. Credit Noble Master Games as follows (linking is optional):
     "Avatar graphics created by Noble Master Games" and link to
     http://www.noblemaster.com
  2. Credit the artist "Liea" as follows (optional):
     "Avatar graphics designed by Mei-Li Nieuwland" and link to
     http://liea.deviantart.com

@see http://opengameart.org/content/avatar-generator-with-15-trillion-combinations
*/


:- multifile
	user:file_search_path/2.

user:file_search_path(noble,		       icons(noble)).
user:file_search_path(noble_avatar_components, noble(components)).
user:file_search_path(noble_avatar,	       data(avatars)).

:- dynamic
	noble_dir/1,				% Directory
	noble/4.				% Part, Gender, I, File

%%	noble_avatar(?Gender, -File) is det.
%%	noble_avatar(?Gender, -File, ?New) is det.
%
%	True when File is the image file  name for a generated avatar of
%	Gender. If Gender is unspecified, it is generated randomly.
%
%	@arg	New is a boolean that indicates whether the avatar is
%		newly generated (`true`) or we re-generated an existing
%		one (`false`).  It may be specified as `true` to force
%		generating a new avatar.  Previously generated avatars
%		can be queried using existing_noble_avatar/2.

noble_avatar(Gender, Image) :-
	noble_avatar(Gender, Image, _).

noble_avatar(Gender, Image, New) :-
	var(New), !,
	noble_index_components,
	avatar_components(Gender, IDs, Components),
	maplist(plus(0'a), IDs, Codes),
	atom_codes(Base, Codes),
	file_name_extension(Base, png, PNG),
	with_mutex(noble_avatar,
		   create_avatar_sync(Components,
				      noble_avatar, PNG, Image, New)).
noble_avatar(Gender, Image, true) :- !,
	repeat,
	    noble_avatar(Gender, Image, New),
	    New == true, !.

%!	create_avatar(+PNG, -Image) is det.
%
%	(Re-)create avatar with basename PNG.

create_avatar(PNG, Image) :-
	file_name_extension(Base, png, PNG),
	atom_codes(Base, Codes),
	maplist(plus(0'a), IDs, Codes),
	noble_index_components,
	avatar_components(_Gender, IDs, Components),
	with_mutex(noble_avatar,
		   create_avatar_sync(Components,
				      noble_avatar, PNG, Image, _New)).

%%	existing_noble_avatar(-Gender, -Image) is nondet.
%
%	True when Image is the image file of a previously generated
%	avatar of Gender.

existing_noble_avatar(Gender, Image) :-
	absolute_file_name(noble_avatar(.), Dir,
			   [ file_type(directory),
			     solutions(all)
			   ]),
	directory_files(Dir, Files),
	member(Image, Files),
	file_name_extension(Base, png, Image),
	sub_atom(Base, 0, 1, _, First),
	char_code(First, Code),
	Index is Code-0'a,
	gender_id(Gender, Index).


create_avatar_sync(Components, DirAlias, File, Image, New) :-
	Location =.. [DirAlias,File],
	(   absolute_file_name(Location, Image,
			       [ access(read),
				 file_errors(fail)
			       ])
	->  New = false
	;   absolute_file_name(Location, Image,
			       [ access(write), file_errors(fail) ])
	->  composite(Components, Image),
	    New	= true
	;   Dir =.. [DirAlias,.],
	    absolute_file_name(Dir, DirPath, [solutions(all)]),
	    file_directory_name(DirPath, Parent),
	    exists_directory(Parent),
	    \+ exists_directory(DirPath)
	->  make_directory(DirPath),
	    absolute_file_name(Location, Image, [access(write)]),
	    composite(Components, Image)
	).

composite(Components, Image) :-
	noble_dir(Dir),
	phrase(composite(Components, Dir), Argv, [file(Image)]),
	process_create(path(convert), Argv, []).

composite([], _) -->
	[ '-background', 'none', '-flatten' ].
composite([File|T], Dir) -->
	{ directory_file_path(Dir, File, AbsFile)
	},
	[ '-page', '+0+0', file(AbsFile) ],
	composite(T, Dir).

avatar_components(Gender, [GID|IDs], Files) :-
	gender_id(Gender, GID),
	parts(Parts),
	files(Parts, Gender, IDs, Files).

files([], _, [], []).
files([P:H-Gender|T], Gender, [I|IDs], [File|Files]) :-
	(   var(I), I \== 0
	->  maybe(P)
	;   true
	),
	file(H, Gender, I, File), !,
	files(T, Gender, IDs, Files).
files([_|T], Gender, [0|IDs], Files) :-
	files(T, Gender, IDs, Files).

file(Part, Gender, I, File) :-
	findall(I, noble(Part, Gender, I, _), IL),
	random_member(I, IL),
	noble(Part, Gender, I, File).

gender_id(Var, ID) :-
	var(Var), var(ID),
	ID is 1+random(2),
	gender_id(Var, ID), !.
gender_id(male, 1).
gender_id(female, 2).


%%	parts(-Parts:list) is det.
%
%	True when Parts is the list  of part specifications for creating
%	a new avatar. Each specification is a term
%
%	    Probability:Part-Gender
%
%	Part is included with Probability and only of Gender matches
%	the target Gender.

parts([ 0.5:pattern - _,
	1.0:head - _,
	1.0:mouth - _,
	1.0:eye - _,
	0.5:eyepatch - _,
	0.3:glasses - _,
	0.3:mustache - male,
	0.5:beard - male,
	0.8:hair - _,
	0.2:accessory - _,
	0.5:necklace - _,
	0.3:boa - _,
	0.2:scar - _,
	0.1:sideburn - _
      ]).

%%	noble_index_components
%
%	Create an index for the Noble  Avatar components. The components
%	are searched for in the directory noble_avatar_components(.).

noble_index_components :-
	noble_dir(_), !.
noble_index_components :-
	with_mutex(noble_avatar, noble_index_components_sync).

noble_index_components_sync :-
	noble_dir(_), !.
noble_index_components_sync :-
	retractall(noble_dir(_)),
	retractall(noble(_,_,_,_)),
	absolute_file_name(noble_avatar_components(.), Dir,
			   [ file_type(directory)
			   ]),
	directory_files(Dir, Files),
	maplist(noble_file, Files),
	assertz(noble_dir(Dir)).

noble_file(File) :-
	file_name_extension(Base, png, File),
	atomic_list_concat([avatar,Part,V], '_', Base),
	(   atom_concat(f, NA, V),
	    atom_number(NA, N)
	->  Gender = female
	;   atom_concat(m, NA, V),
	    atom_number(NA, N)
	->  Gender = male
	;   atom_number(V, N)
	), !,
	assert(noble(Part, Gender, N, File)).
noble_file(_).


