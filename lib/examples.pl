/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2017, VU University Amsterdam
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

:- module(swish_examples, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_path)).
:- use_module(library(filesex)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(storage).

/** <module> Serve example files

Locate and serve files for the _Examples_   menu. The examples come from
two sources:

  - Prolog files in the file search path `examples`
  - Gitty files marked as `example`.
*/

:- multifile
	user:file_search_path/2,
	swish_config:config/2,
	swish_config:source_alias/2.

% make example(File) find the example data
user:file_search_path(example, swish(examples)).
% make SWISH serve /example/File as example(File).
swish_config:source_alias(example, [access(read), search('*.{pl,swinb}')]).

:- http_handler(swish(list_examples),
		list_examples, [id(swish_examples)]).


%%	list_examples(+Request)
%
%	Get a list of registered example code. Examples are described in
%	a file swish_examples('index.json').

list_examples(_Request) :-
	example_files(FileExamples),
	storage_examples(StorageExamples),
	append(FileExamples, StorageExamples, AllExamples),
	reply_json(AllExamples).

%%	example_files(JSON:list) is det.
%
%	JSON is a list of JSON dicts containing the keys below. The list
%	is composed from all *.pl files in the search path `example`.
%
%	  - file:File
%	  - href:URL
%	  - title:String

example_files(AllExamples) :-
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
	append(JSON, AllExamples).

index_json(HREF, Dir, JSON) :-
	directory_file_path(Dir, 'index.json', File),
	access_file(File, read), !,
	read_file_to_json(File, JSON0),
	maplist(add_href(HREF), JSON0, JSON).
index_json(HREF, Dir, JSON) :-
	string_concat(Dir, "/*.{pl,swinb}", Pattern),
	expand_file_name(Pattern, Files),
	maplist(ex_file_json(HREF), Files, JSON).

read_file_to_json(File, JSON) :-
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    json_read_dict(In, JSON),
	    close(In)).

add_href(HREF0, Dict, Dict2) :-
	is_dict(Dict),
	directory_file_path(HREF0, Dict.get(file), HREF), !,
	Dict2 = Dict.put(href, HREF).
add_href(_, Dict, Dict).

%%	ex_file_json(+ExampleBase, +Path, -JSON) is det.
%
%	@tbd	Beautify title from file-name (_ --> space, start
%		with capital, etc).

ex_file_json(HREF0, Path, json{file:File, href:HREF, title:Base}) :-
	file_base_name(Path, File),
	file_name_extension(Base, _, File),
	directory_file_path(HREF0, File, HREF).


		 /*******************************
		 *	      STORAGE		*
		 *******************************/

%%	storage_examples(-List) is det.
%
%	Extract examples from the gitty store.

storage_examples(List) :-
	swish_config:config(community_examples, true),
	findall(Ex, gitty_example(Ex), List1),
	List1 \== [], !,
	List = [json{type:"divider"}|List1].
storage_examples([]).

gitty_example(json{title:Title, file:File, type:"store"}) :-
	storage_file(File),
	storage_meta_data(File, Meta),
	Meta.get(example) == true,
	(   Title = Meta.get(title), Title \== ""
	->  true
	;   Title = File
	).
