/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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

:- module(swish_include,
	  [
	  ]).
:- use_module(gitty).
:- use_module(library(sandbox), []).
:- use_module(library(debug)).
:- use_module(library(settings)).

/** <module> Support :- include(File) from SWISH

This module allows SWISH programs  to   include  other programs from the
shared gitty store. It realises this using the following steps:

  - Use term_expansion/2 to rewrite the include to fetch the data from
    the gitty store.
  - Declare this specific version of include safe.
  - Adjust the colourization to indicate the shared file as existing.
  - Hook the Prolog cross-referencer to process the included file.

We allow for hierarchical and circular includes.
*/


swish:term_expansion(:- include(FileIn), Expansion) :-
	atomic(FileIn),
	atom_string(File, FileIn),
	(   prolog_load_context(module, Module),
	    clause(Module:'swish included'(File), true)
	->  Expansion = []
	;   Expansion = [ (:- discontiguous('swish included'/1)),
		          'swish included'(File),
		          (:- include(stream(Id, Stream, [close(true)])))
			],
	    '$push_input_context'(swish_include),
	    setting(web_storage:directory, Store),
	    add_extension(File, FileExt),
	    catch(gitty_data(Store, FileExt, Data, _Meta), _, fail),
	    atom_concat('swish://', FileExt, Id),
	    open_string(Data, Stream),
	    '$pop_input_context'
	).

add_extension(File, FileExt) :-
	file_name_extension(_, Ext, File),
	Ext \== '', !,
	FileExt = File.
add_extension(Hash, Hash) :-
	is_hash(Hash), !.
add_extension(File, FileExt) :-
	file_name_extension(File, pl, FileExt).

is_hash(Name) :-
	atom_length(Name, 40),
	split_string(Name, ":", "0123456789abcdef", [""]).


		 /*******************************
		 *	      SANDBOX		*
		 *******************************/

:- multifile
	sandbox:safe_directive/1.

sandbox:safe_directive(M:include(stream(Id, Stream, [close(true)]))) :-
	is_stream(Stream),
	sub_atom(Id, 0, _, _, 'swish://'),
	prolog_load_context(module, M).


		 /*******************************
		 *	      COLOUR		*
		 *******************************/

:- multifile
	prolog_colour:term_colours/2.

prolog_colour:term_colours((:- include(File)),
			   neck(directive) -
			   [ goal(built_in,include(File)) -
			     [ FileClass
			     ]
			   ]) :-
	debug(include, 'Classifying ~p', [File]),
	(   atomic(File),
	    setting(web_storage:directory, Store),
	    add_extension(File, FileExt),
	    catch(gitty_commit(Store, FileExt, _Meta), _, fail)
	->  atom_concat('swish://', FileExt, Id),
	    FileClass = file(Id)
	;   FileClass = nofile
	),
	debug(include, 'Class ~p', [FileClass]).


		 /*******************************
		 *	      XREF		*
		 *******************************/

:- multifile
	prolog:xref_open_source/2,
	prolog:xref_source_file/3,
	prolog:xref_source_identifier/2,
	prolog:xref_source_time/2.

%%	prolog:xref_source_identifier(+Src, -Id) is semidet.
%%	prolog:xref_open_source(+File, -Stream) is det.
%%	prolog:xref_source_time(+File, -Modified) is det.
%
%	Map swish://file to a file from the gitty store.

prolog:xref_source_identifier(Src, Id) :-
	atom(Src),
	sub_atom(Src, 0, _, _, 'swish://'), !,
	Id = Src.

prolog:xref_open_source(File, Stream) :-
	atom(File),
	atom_concat('swish://', Name, File),
	setting(web_storage:directory, Store),
	catch(gitty_data(Store, Name, Data, _Meta), _, fail),
	open_string(Data, Stream).

prolog:xref_source_time(File, Modified) :-
	atom(File),
	atom_concat('swish://', Name, File),
	setting(web_storage:directory, Store),
	catch(gitty_commit(Store, Name, Meta), _, fail),
	Modified = Meta.get(time).

%%	prolog:xref_source_file(+Term, -Path, +Options)
%
%	Deal with the above expansion for :- include(program) to support
%	the cross-referencer.

prolog:xref_source_file(stream(Id, _Stream, [close(true)]), Id, _).
prolog:xref_source_file(File, Id, Options) :-
	atom(File),
	option(relative_to(Src), Options),
	atom(Src),
	sub_atom(Src, 0, _, _, 'swish://'),
	add_extension(File, FileExt),
	atom_concat('swish://', FileExt, Id).

