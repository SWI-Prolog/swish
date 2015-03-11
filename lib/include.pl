/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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
*/


swish:term_expansion(:- include(File),
		     :- include(stream(Id, Stream, [close(true)]))) :-
	setting(web_storage:directory, Store),
	atomic(File),
	add_extension(File, FileExt),
	catch(gitty_data(Store, FileExt, Data, _Meta), _, fail),
	atom_concat('swish://', FileExt, Id),
	open_string(Data, Stream).

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
	(   setting(web_storage:directory, Store),
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
	prolog:xref_source_file/3.

prolog:xref_open_source(File, Stream) :-
	atom_concat('swish://', Name, File),
	setting(web_storage:directory, Store),
	catch(gitty_data(Store, Name, Data, _Meta), _, fail),
	open_string(Data, Stream).

prolog:xref_source_file(stream(Id, _Stream, [close(true)]), Id, _).

