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

:- module(web_storage, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_client)).
:- use_module(library(settings)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(filesex)).

/** <module> Store files on behalve of web clients

*/

:- setting(directory, atom, storage, 'The directory for storing files.').

user:file_search_path(web_storage, Dir) :-
	setting(directory, Dir).
user:file_search_path(web_storage, swish_examples(.)).

:- http_handler(root(storage), web_storage, [ id(web_storage), prefix ]).

%%	web_storage(+Request) is det.
%
%	Restfull HTTP handler to store data on behalf of the client in a
%	hard-to-guess location. Returns a JSON  object that provides the
%	URL for the data and the plain   file name. Understands the HTTP
%	methods =GET=, =POST=, =PUT= and =DELETE=.

web_storage(Request) :-
	option(method(Method), Request),
	storage(Method, Request).

storage(get, Request) :-
	request_file(Request, _File, Path),
	http_reply_file(Path, [unsafe(true)], Request).
storage(post, Request) :-
	http_parameters(Request,
			[   data(Data, [default(''),
					description('Data to be saved')]),
			    type(Type, [default(pl)])
			]),
	setting(directory, Dir),
	make_directory_path(Dir),
	random_filename(Base),
	file_name_extension(Base, Type, File),
	directory_file_path(Dir, File, RelPath),
	storage_url(File, URL),
	save_string(RelPath, Data),
	reply_json_dict(json{url:URL, file:File}).
storage(put, Request) :-
	http_read_data(Request, Form, []),
	option(data(Data), Form, ''),
	request_file(Request, File, Path),
	storage_url(File, URL),
	save_string(Path, Data),
	reply_json_dict(json{url:URL, file:File}).
storage(delete, Request) :-
	request_file(Request, _File, Path),
	delete_file(Path),
	reply_json_dict(true).

request_file(Request, File, Path) :-
	option(path_info(PathInfo), Request),
	atom_concat(/, File, PathInfo),
	http_safe_file(File, []),
	absolute_file_name(web_storage(File), Path, [access(read)]).

storage_url(File, HREF) :-
	http_absolute_uri(root(storage/File), HREF).

save_string(File, Data) :-
	setup_call_cleanup(
	    open(File, write, S, [encoding(utf8)]),
	    write(S, Data),
	    close(S)).

%%	random_filename(-Name) is det.
%
%	Return a random file name from plain nice ASCII characters.

random_filename(Name) :-
	length(Chars, 8),
	maplist(random_char, Chars),
	atom_chars(Name, Chars).

from('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ').

random_char(Char) :-
	from(From),
	atom_length(From, Len),
	Max is Len - 1,
	random_between(0, Max, I),
	sub_atom(From, I, 1, _, Char).
