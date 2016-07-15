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
	  [ swish_add_user/3,		% +User, +Passwd, +Fields
	    swish_add_user/1,		% +Dict
	    swish_add_user/0,
	    swish_logged_in/3,		% +Request, -User, -Data
	    swish_current_user/2	% ?User, ?Data
	  ]).
:- use_module(library(pengines), []).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(crypt)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(form).

:- use_module(config).
:- use_module(page, []).

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
:- setting(password_file, callable, passwd,
	   "Location of the password file").

:- multifile
	swish_config:config/2,
	swish_config:config/3,
	swish_config:authenticate/2,
	swish_config:verify_write_access/3.

/** <module> SWISH login management

This module provides basic login and  password management facilities for
SWISH.  You can create an authenticated SWISH server by

  1. Loading this library
  2. Add one or more users to the passwd file using swish_add_user/0

     ==
     ?- swish_add_user.
     User name: Bob
     Real name: Bob de Bouwer
     Group:     user
     E-mail:	bob@bouwer.com
     Password:
     (again):
     true.
     ==

Authentication is by default based on  HTTP digest authentication, which
uses a challenge-response to avoid  exchanging   the  plain password and
uses  sequence  numbers  to  avoid  replaying  actions.  This  basically
protects non-authorized users  from  entering   commands,  but  does not
encrypt the communication.
*/

:- dynamic
	password_file_cache/1.

password_file(File) :-
	password_file_cache(File), !.
password_file(File) :-
	setting(password_file, Spec),
	absolute_file_name(Spec, File, [access(read)]),
	update_auth_type(File),
	asserta(password_file_cache(File)).

%%	logged_in(+Request, -User) is det.
%
%	True when User is  logged  in.   Throws  an  HTTP  authorization
%	exception if the user is not authenticated.
%
%	@throw http_reply(authorise(Authorise))

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

%%	swish_config:config(?Key, ?Value, +Options) is nondet.
%
%	Make the user available as config.swish.user.

swish_config:config(user, Dict, Options) :-
	option(user(User), Options),
	password_file(File),
	(   http_current_user(File, User, [_Hash,Group,RealName,Email])
	->  Dict = u{user:User, group:Group, realname:RealName, email:Email}
	;   Dict = u{user:User}
	).


%%	pengines:authentication_hook(+Request, +Application, -User)
%
%	Is called from the  /pengine/create   request  to  establish the
%	logged in user.

pengines:authentication_hook(_Request, _Application, anonymous) :- !.
	%logged_in(Request, User), !.

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

%%	swish_current_user(?User, -Dict) is nondet.
%
%	True if User is a user with properties.

swish_current_user(User,
		   u{user:User, group:Group, realname:RealName, email:Email}) :-
	password_file(File),
	http_current_user(File, User, [_Hash,Group,RealName,Email]).

%%	swish_logged_in(+Request, -User, -UserData) is det.
%
%	True when Request is associated with User.

swish_logged_in(Request, User, UserData) :-
	logged_in(Request, User),
	swish_current_user(User, UserData).


%%	swish_add_user(+User, +Passwd, +Fields) is det.
%
%	Add a new user to the SWISH   password  file. Defined Fields are
%	(in this order):
%
%	  - _Group_ identifies the user group (not used)
%	  - _Real name_ is the common name of the user
%	  - _EMail_ is the user's e-mail address (not used)

:- if(current_predicate(http_digest_password_hash/4)).
swish_add_user(User, Passwd, Fields) :-
	setting(method, digest), !,
	setting(realm, Realm),
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
	writeable_passwd_file(File),
	(   exists_file(File)
	->  http_read_passwd_file(File, Data)
	;   Data = []
	),
	(   selectchk(passwd(User, _, _), Data, Entry, NewData)
	->  true
	;   append(Data, [Entry], NewData)
	),
	http_write_passwd_file(File, NewData).

writeable_passwd_file(File) :-
	(   catch(password_file(File), _, fail)
	->  true
	;   setting(password_file, Spec),
	    absolute_file_name(Spec, File, [access(write)])
	).


%%	swish_add_user
%
%	Interactively add a user to the SWISH password file.

swish_add_user :-
	writeable_passwd_file(File),
	(   exists_file(File)
	->  Action = update
	;   Action = create
	),
	print_message(informational, password_file(Action, File)),
	read_string("User name: ", User),
	read_string("Real name: ", RealName),
	read_string("Group:     ", Group),
	read_string("E-Mail:    ", Email),
	between(1, 3, _),
	read_passwd("Password:  ", Passwd1),
	read_passwd("(again):   ", Passwd2),
	(   Passwd1 == Passwd2
	->  !, swish_add_user(User, Passwd1, [Group,RealName,Email])
	;   print_message(warning, password_mismatch),
	    fail
	).

read_string(Prompt, String) :-
	format(user_error, '~w', [Prompt]),
	read_line_to_string(user_input, String).
read_passwd(Prompt, Passwd) :-
	format(user_error, '~w', [Prompt]),
	read_pwd([], Codes),
	string_codes(Passwd, Codes).

read_pwd(P0, P) :-
	get_single_char(C),
	read_pwd(C, P0, P).

read_pwd(0'\r, P, P) :- !, nl(user_error).
read_pwd(0'\n, P, P) :- !, nl(user_error).
read_pwd(0'\b, P0, P) :-
	(   append(P1, [_], P0)
	->  true
	;   P1 = []
	),
	read_pwd(P1, P).
read_pwd(21, _, P) :-			% Control-U
	read_pwd([], P).
read_pwd(C, P0, P) :-
	append(P0, [C], P1),
	read_pwd(P1, P).


%%	swish_add_user(+Data:dict) is det.
%
%	Add a user from Data.

swish_add_user(Data) :-
	Groups = [user,administrator],
	validate_form(
	    Data,
	    [ field(user,     User,     [alnum, atom, length >= 2]),
	      field(realname, RealName, [strip, alnum_and_spaces]),
	      field(email,    Email,    [email]),
	      field(group,    Group,    [downcase, atom,oneof(Groups)]),
	      field(pwd1,     Pwd1,     [password]),
	      field(pwd2,     Pwd2,     [password])
	    ]),
	new_user(User),
	(   Pwd1 == Pwd2
	->  true
	;   input_error(pwd2, matching_password)
	),
	swish_add_user(User, Pwd1, [Group,RealName,Email]).

new_user(User) :-
	writeable_passwd_file(File),
	exists_file(File),
	http_current_user(File, User, _Fields), !,
	input_error(user, new_user).
new_user(_).


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(http_auth_type(ReqMethod, Method)) -->
	[ 'Using HTTP authentication ~q instead of ~q due to password file format'
	  -[Method, ReqMethod]
	].
prolog:message(password_mismatch) -->
	[ 'Password mismatch'-[] ].
prolog:message(password_file(Action, File)) -->
	[ 'Password file: ~w (~w)'-[File, Action] ].
