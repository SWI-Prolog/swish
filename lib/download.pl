/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
		         CWI, Amsterdam
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

:- module(download,
	  [ download_button/2			% +Data, +Options
	  ]).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(apply)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

/** <module> Provide a button for downloading data

This module allows a button to be  inserted into the Pengine output that
allows for downloading data. Originally this   used the `data` type URL.
This has been disabled in recent   browsers. Also considering the length
limitations on URLs on some browsers we   now store the data server-side
and make the link simply download the  data.   The  data  is kept on the
server for `keep_downloads_time` seconds, default 24 hours.
*/

:- setting(keep_downloads_time, number, 86400,
	   "Seconds to keep a downloaded file").

%!	download_button(+Data:string, +Options)
%
%	Emit a button in the SWISH   output window for downloading Data.
%	The provided data  is  stored  on   the  server.
%
%	Options:
%
%	  - filename(+Name)
%	    (Base-)Name of the file created (default:
%	    `swish-download.dat`),
%	  - content_type(+Type)
%	    Full content type.  By default this is derived from the
%	    extension of the filename and the encoding.
%	  - encoding(+Enc)
%	    Encoding to use. One of `utf8` or `octet`. default is
%	    `utf8`.
%
%	@see https://en.wikipedia.org/wiki/Data_URI_scheme

download_button(Data, Options) :-
	option(filename(FileName), Options, 'swish-download.dat'),
	option(encoding(Enc), Options, utf8),
	(   option(content_type(ContentType), Options)
	->  true
	;   file_mime_type(FileName, Major/Minor),
	    atomics_to_string([Major, Minor], /, ContentType0),
	    add_charset(Enc, ContentType0, ContentType)
	),
	save_download_data(Data, UUID, Enc),
	pengine_output(
	    json{action:downloadButton,
		 content_type:ContentType,
		 encoding: Enc,
		 uuid:UUID,
		 filename:FileName
		}).

add_charset(utf8, Enc0, Enc) :- !,
	atom_concat(Enc0, '; charset=UTF-8', Enc).
add_charset(_, Enc, Enc).


		 /*******************************
		 *	      SERVER		*
		 *******************************/

:- http_handler(swish(download), download, [id(download), prefix, method(get)]).

%!	download(+Request)
%
%	Handle a download request.

download(Request) :-
	http_parameters(Request,
			[ uuid(UUID, []),
			  content_type(Type, [])
			]),
	download_file(UUID, File),
	http_reply_file(File,
			[ mime_type(Type),
			  unsafe(true)
			],
			Request).


		 /*******************************
		 *	       STORE		*
		 *******************************/

%!	save_download_data(+Data, -UUID, +Encoding) is det.
%
%	Save the string Data in the download store and return a UUID to
%	retreive it.

save_download_data(Data, UUID, Encoding) :-
	download_file(UUID, Path),
	ensure_parents(Path),
	setup_call_cleanup(
	    open(Path, write, Out, [encoding(Encoding)]),
	    write(Out, Data),
	    close(Out)),
	prune_downloads.


%!	download_file(?UUID, -Path)
%
%	Path is the full file from which to download Name.
%
%	@tbd We could use the SHA1 of the  data. In that case we need to
%	_touch_ the file if it exists and we   need  a way to ensure the
%	file is completely saved by a   concurrent  thread that may save
%	the same file.

download_file(UUID, Path) :-
	(   var(UUID)
	->  uuid(UUID)
	;   true
	),
	variant_sha1(UUID, SHA1),
	sub_atom(SHA1, 0, 2, _, Dir0),
	sub_atom(SHA1, 2, 2, _, Dir1),
	sub_atom(SHA1, 4, _, 0, File),
	download_dir(Dir),
	atomic_list_concat([Dir, Dir0, Dir1, File], /, Path).


%!	download_dir(-Dir) is det.
%
%	Find the download base directory.

:- dynamic download_dir_cache/1.
:- volatile download_dir_cache/1.

download_dir(Dir) :-
	download_dir_cache(Dir),
	!.
download_dir(Dir) :-
	absolute_file_name(data(download), Dir,
			   [ file_type(directory),
			     access(write),
			     file_errors(fail)
			   ]),
	!,
	asserta(download_dir_cache(Dir)).
download_dir(Dir) :-
	absolute_file_name(data(download), Dir,
			   [ solutions(all)
			   ]),
	catch(make_directory(Dir), error(_,_), fail),
	!,
	asserta(download_dir_cache(Dir)).

ensure_parents(Path) :-
	file_directory_name(Path, Dir1),
	file_directory_name(Dir1, Dir0),
	ensure_directory(Dir0),
	ensure_directory(Dir1).

ensure_directory(Dir) :-
	exists_directory(Dir),
	!.
ensure_directory(Dir) :-
	make_directory(Dir).


%!	prune_downloads
%
%	Prune old download files. This is actually executed every 1/4th
%	of the time we keep the files.  This makes this call fast.

:- dynamic pruned_at/1.
:- volatile pruned_at/1.

prune_downloads :-
	E = error(_,_),
	with_mutex(download,
		   catch(prune_downloads_sync, E,
			 print_message(warning, E))).

prune_downloads_sync :-
	pruned_at(Last),
	setting(keep_downloads_time, Time),
	get_time(Now),
	Now < Last + Time/4,
	!.
prune_downloads_sync :-
	thread_create(do_prune_downloads, _,
		      [ alias(prune_downloads),
			detached(true)
		      ]),
	get_time(Now),
	retractall(pruned_at(_)),
	asserta(pruned_at(Now)).

do_prune_downloads :-
	get_time(Now),
	setting(keep_downloads_time, Time),
	Before is Now - Time,
	download_dir(Dir),
	prune_dir(Dir, Before, false).

%!	prune_dir(+Dir, +Time, +PruneDir) is det.
%
%	Find all files older than Time and  delete them as well as empty
%	directories.

prune_dir(Dir, Time, PruneDir) :-
	directory_files(Dir, Files0),
	exclude(reserved, Files0, Files),
	exclude(clean_entry(Dir, Time), Files, Rest),
	(   Rest == [],
	    PruneDir == true
	->  E = error(_,_),
	    catch(delete_directory(Dir), E,
		  print_message(warning, E))
	;   true
	).

reserved(.).
reserved(..).

%!	clean_entry(+Dir, +Time, +File) is semidet.
%
%	True when Dir/File has been cleaned and is removed.

clean_entry(Dir, Time, File) :-
	directory_file_path(Dir, File, Path),
	(   exists_directory(Path)
	->  prune_dir(Path, Time, true),
	    \+ exists_directory(Path)
	;   time_file(Path, FTime),
	    FTime < Time
	->  E = error(_,_),
	    catch(delete_file(Path), E,
		  ( print_message(warning, E),
		    fail
		  ))
	).

