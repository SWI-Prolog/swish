/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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

:- module(avatar,
	  [ email_gravatar/2,			% +Email, -AvatarURL
	    valid_gravatar/1,			% +AvatarURL
	    random_avatar/1,			% -AvatarURL
	    release_avatar/1,			% +AvatarURL

	    clean_avatar_cache/0
	  ]).
:- use_module(library(uri)).
:- use_module(library(md5)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_open)).
:- use_module(library(error)).

/** <module> Avatar management

This module provides access to avatar handling.
*/

%%	email_avatar(+Email, -AvatarImageLink) is det.
%
%	@see https://en.gravatar.com/site/implement/hash/
%	@see https://en.gravatar.com/site/implement/images/

email_gravatar(Email, AvatarURL) :-
	downcase_atom(Email, CanonicalEmail),
	md5_hash(CanonicalEmail, Hash, []),
	atom_concat('/avatar/', Hash, Path),
	uri_data(scheme,    Components, http),
	uri_data(authority, Components, 'www.gravatar.com'),
	uri_data(path,      Components, Path),
	uri_components(AvatarURL, Components).


%%	valid_gravatar(+URL) is semidet.
%
%	True if URL is a real gavatar.

valid_gravatar(URL) :-
	string_concat(URL, "?d=404", URL2),
	catch(http_open(URL2, In, [method(head)]),
	      error(existence_error(_,_),_),
	      fail),
	close(In).


%%	random_avatar(-AvatarURL) is det.
%
%	Generate a random avatar image url. This uses an arbitrary image
%	from  the  virtual  path  icons(avatar).  This  predicate  never
%	replies with the same URL.
%
%	@arg AvatarURL is a relative URL (does not include the host)
%	@error resource_error(avatars) if no more avatars are available

random_avatar(AvatarURL) :-
	avatar_cache(_Size),
	repeat,
	findall(I, free_avatar(I), L),
	    (	L == []
	    ->	resource_error(avatars)
	    ;	random_member(A, L),
		avatar(A, AvatarURL),
		with_mutex(avatar, claim_avatar(A)),
		!
	    ).

free_avatar(I) :-
	avatar(I, _),
	\+ used_avatar(I).

claim_avatar(I) :-
	used_avatar(I), !, fail.
claim_avatar(I) :-
	assertz(used_avatar(I)).

%!	release_avatar(+URL) is det.
%
%	Release the avatar to the pool of free avatars.

release_avatar(URL0) :-
	atom_string(URL, URL0),
	forall(avatar(I, URL),
	       retractall(used_avatar(I))).

clean_avatar_cache :-
	retractall(avatar_cache_size(_)),
	retractall(avatar(_,_)).

:- dynamic
	used_avatar/1,
	avatar_cache_size/1,
	avatar/2.
:- volatile
	used_avatar/1,
	avatar_cache_size/1,
	avatar/2.

avatar_cache(Size) :-
	avatar_cache_size(Size), !.
avatar_cache(Size) :-
	findall(Path, avatar_path(Path), Paths),
	foldl(assert_avatar, Paths, 0, Size0),
	assertz(avatar_cache_size(Size0)),
	Size = Size0.

avatar_path(icons(avatar/File)) :-
	absolute_file_name(icons(avatar), Dir,
			   [ file_type(directory),
			     solutions(all)
			   ]),
	directory_files(Dir, Files),
	member(File, Files),
	file_name_extension(_, Ext, File),
	downcase_atom(Ext, LwrExt),
	image_extension(LwrExt).

image_extension(png).
image_extension(jpg).
image_extension(jpeg).
image_extension(gif).

assert_avatar(Path, N, N2) :-
	http_absolute_location(Path, HREF, []),
	assertz(avatar(N, HREF)),
	N2 is N+1.
