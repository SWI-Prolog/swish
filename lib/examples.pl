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

:- module(swish_examples, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- use_module(library(lists)).

/** <module> Serve example files
*/

:- multifile
	user:file_search_path/2,
	swish_config:source_alias/1.

% make example(File) find the example data
user:file_search_path(example, swish(examples)).
% make SWISH serve /example/File as example(File).
swish_config:source_alias(example).

:- http_handler(swish(list_examples),
		list_examples, [id(swish_examples)]).


%%	list_examples(+Request)
%
%	Get a list of registered example code. Examples are described in
%	a file swish_examples('index.json').

list_examples(_Request) :-
	http_absolute_location(swish(example), HREF, []),
	findall(Index,
		absolute_file_name(example(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs),
	maplist(index_json(HREF), ExDirs, JSON),
	append(JSON, AllExamples),
	reply_json(AllExamples).

index_json(HREF, Dir, JSON) :-
	directory_file_path(Dir, 'index.json', File),
	access_file(File, read), !,
	read_file_to_json(File, JSON0),
	maplist(add_href(HREF), JSON0, JSON).
index_json(HREF, Dir, JSON) :-
	string_concat(Dir, "/*.pl", Pattern),
	expand_file_name(Pattern, Files),
	maplist(ex_file_json(HREF), Files, JSON).

read_file_to_json(File, JSON) :-
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    json_read_dict(In, JSON),
	    close(In)).

add_href(HREF0, Dict, Dict.put(href, HREF)) :-
	directory_file_path(HREF0, Dict.file, HREF).

%%	ex_file_json(+ExampleBase, +Path, -JSON) is det.
%
%	@tbd	Beautify title from file-name (_ --> space, start
%		with capital, etc).

ex_file_json(HREF0, Path, json{file:File, href:HREF, title:Base}) :-
	file_base_name(Path, File),
	file_name_extension(Base, _, File),
	directory_file_path(HREF0, File, HREF).
