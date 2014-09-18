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

:- module(swish_authenticate,
	  [ swish_add_user/3		% +User, +Passwd, +Fields
	  ]).
:- use_module(library(pengines), []).
:- use_module(library(lists)).
:- use_module(library(crypt)).
:- use_module(library(http/http_authenticate)).

/** <module> SWISH login management

This module provides basic login and  password management facilities for
SWISH.
*/

:- dynamic
	password_file_cache/1.

password_file(File) :-
	password_file_cache(File), !.
password_file(File) :-
	absolute_file_name(swish(passwd), File, [access(read)]),
	asserta(password_file_cache(File)).

pengines:authentication_hook(Request, _Application, User) :-
	password_file(File),
	http_authenticate(basic(File), Request, [User|_Fields]), !.
pengines:authentication_hook(_Request, _Application, _User) :-
	throw(http_reply(authorise(basic('SWISH user')))).

%%	swish_add_user(+User, +Passwd, +Fields) is det.
%
%	Add a new user to the SWISH password file.

swish_add_user(User, Passwd, Fields) :-
	phrase("$1$", E, _),
	crypt(Passwd, E),
	string_codes(Hash, E),

	Entry = passwd(User, Hash, Fields),

	absolute_file_name(swish(passwd), File,
			   [access(write)]),
	(   exists_file(File)
	->  http_read_passwd_file(File, Data)
	;   Data = []
	),
	(   selectchk(passwd(User, _, _), Data, Entry, NewData)
	->  true
	;   append(Data, [Entry], NewData)
	),
	http_write_passwd_file(File, NewData).

