/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

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
:- use_module(library(debug)).
:- use_module(library(crypt)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(settings)).

:- use_module(config).
:- use_module(page).

:- if(exists_source(library(http/http_digest))).
:- use_module(library(http/http_digest)).
:- setting(method, oneof([basic,digest]), digest,
	   "HTTP authentication method used").
:- else.
:- setting(method, oneof([basic]), basic,
	   "HTTP authentication method used").
:- endif.

:- setting(realm, atom, 'SWISH user',
	   "HTTP authentication realm").

:- multifile
	swish_config:config/2,
	swish_config:authenticate/2,
	swish_config:verify_write_access/3.

/** <module> SWISH login management

This module provides basic login and  password management facilities for
SWISH.  You can create an authenticated SWISH server by

  1. Loading this library
  2. Add one or more users to the passwd file using swish_add_user/3

     ==
     ?- swish_add_user("Bob", "Bob's secret", []).
     ==

As a result, trying to create the  first pengine (e.g., using _|Run!|_),
the server will challenge the user.  The   logged  in  user is available
through pengine_user/1.
*/

:- dynamic
	password_file_cache/1.

password_file(File) :-
	password_file_cache(File), !.
password_file(File) :-
	absolute_file_name(swish(passwd), File, [access(read)]),
	update_auth_type(File),
	asserta(password_file_cache(File)).

logged_in(Request, User) :-
	setting(method, digest), !,
	setting(realm, Realm),
	password_file(File),
	http:authenticate(digest(File, Realm), Request, [user(User)|_Details]).
logged_in(Request, User) :-
	password_file(File),
	http_authenticate(basic(File), Request, [User|_Fields]), !,
	debug(authenticate, 'Logged in as ~p', [User]).
logged_in(_Request, _User) :-
	throw(http_reply(authorise(basic('SWISH user')))).

%%	pengines:authentication_hook(+Request, +Application, -User)
%
%	Is called from the  /pengine/create   request  to  establish the
%	logged in user.

pengines:authentication_hook(Request, _Application, User) :-
	logged_in(Request, User), !.

pengines:not_sandboxed(_User, _Application).


%%	swish_config:verify_write_access(+Request, +File, +Options)

swish_config:verify_write_access(Request, _File, _Options) :-
	logged_in(Request, _User), !.

%%	swish_config:authenticate(+Request, -User)
%
%	Called for all SWISH  actions.  May   be  used  to  restrict all
%	access. Access can only be denied by throwing an exception.

swish_config:authenticate(Request, User) :-
	\+ swish_config(public_access, true),
	logged_in(Request, User).


%%	update_auth_type(+File)
%
%	Update the authentication type to match the password hashes

update_auth_type(File) :-
	digest_password_file(File), !,
	setting(method, Method),
	(   Method == digest
	->  true
	;   print_message(warning, http_auth_type(Method, digest)),
	    set_setting(method, digest)
	).
update_auth_type(_) :-
	setting(method, Method),
	(   Method == basic
	->  true
	;   print_message(warning, http_auth_type(Method, basic)),
	    set_setting(method, basic)
	).

digest_password_file(File) :-
	http_read_passwd_file(File, [passwd(_User, Hash, _Fields)|_]),
	is_sha1(Hash).

is_sha1(Hash) :-
	atom_length(Hash, 32),
	forall(sub_atom(Hash, _, 1, _, Char),
	       char_type(Char, xdigit(_))).

%%	swish_add_user(+User, +Passwd, +Fields) is det.
%
%	Add a new user to the SWISH password file.

:- if(current_predicate(http_digest_password_hash/4)).
swish_add_user(User, Passwd, Fields) :-
	setting(method, digest), !,
	setting(realm, Realm), !,
	http_digest_password_hash(User, Realm, Passwd, Hash),
	update_password(passwd(User, Hash, Fields)).
:- endif.
swish_add_user(User, Passwd, Fields) :-
	phrase("$1$", E, _),		% use Unix MD5 hashes
	crypt(Passwd, E),
	string_codes(Hash, E),
	update_password(passwd(User, Hash, Fields)).

update_password(Entry) :-
	arg(1, Entry, User),
	(   catch(password_file(File), _, fail)
	->  true
	;   absolute_file_name(swish(passwd), File,
			       [access(write)])
	),
	(   exists_file(File)
	->  http_read_passwd_file(File, Data)
	;   Data = []
	),
	(   selectchk(passwd(User, _, _), Data, Entry, NewData)
	->  true
	;   append(Data, [Entry], NewData)
	),
	http_write_passwd_file(File, NewData).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(http_auth_type(ReqMethod, Method)) -->
	[ 'Using HTTP authentication ~q instead of ~q due to password file format'
	  -[Method, ReqMethod]
	].
