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

:- module(swish_help, []).
:- use_module(library(lists)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

/** <module> SWISH help system

This module serves help information for SWISH.

@tbd	Serve SWI-Prolog Markdown files.
*/

:- http_handler(swish(help), serve_files_in_directory(swish_help),
		[id(help),prefix]).
:- http_handler(swish(help_index),
		help_index, [id(swish_help_index)]).

user:file_search_path(swish_help, swish(web/help)).

%%	help_index(+Request)
%
%       Get a list of registered help  topics. Help topics are described
%       in a file swish_help('index.json').

help_index(_Request) :-
	help_files(HelpIndex),
	reply_json(HelpIndex).

%%	help_files(JSON:list) is det.
%
%       JSON is a list of JSON dicts containing the keys below. The list
%       is  composed  from  all  *.html  files    in   the  search  path
%       `swish_help`.
%
%	  - file:File
%	  - title:String

help_files(AllExamples) :-
	findall(Index,
		absolute_file_name(swish_help(.), Index,
				   [ access(read),
				     file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ]),
		ExDirs),
	maplist(index_json, ExDirs, JSON),
	append(JSON, AllExamples).

index_json(Dir, JSON) :-
	directory_file_path(Dir, 'index.json', File),
	access_file(File, read), !,
	read_file_to_json(File, JSON).
index_json(Dir, JSON) :-
	string_concat(Dir, "/*.{html}", Pattern),
	expand_file_name(Pattern, Files),
	maplist(help_file_json, Files, JSON).

read_file_to_json(File, JSON) :-
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    json_read_dict(In, JSON),
	    close(In)).

%%	help_file_json(+Path, -JSON) is det.
%
%	@tbd	Beautify title from file-name (_ --> space, start
%		with capital, etc).

help_file_json(Path, json{file:File, title:Base}) :-
	file_base_name(Path, File),
	file_name_extension(Base, _, File).
