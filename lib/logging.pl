/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(swish_logging,
	  [ create_log_dir/0
	  ]).
:- use_module(library(http/http_log)).
:- use_module(library(broadcast)).
:- use_module(library(settings)).
:- use_module(library(apply)).

/** <module> Add SWISH query execution to the HTTP log file

Add a line of the format below to  the HTTP log file. The src_text(Text)
option of Options is replaced by   `Hash-Text`  for the first occurrence
and just `Hash` for subsequent occurrences.

  ==
  pengine(Time, create(Pengine, Application, Options))
  ==
*/

:- setting(swish:logging, boolean, true,
	   "Enable/disable logging of SWISH query execution").

:- listen(pengine(Action), swish_log(Action)).

swish_log(create(Pengine, Application, Options0)) :-
	maplist(hash_option, Options0, Options),
	get_time(Now),
	format_time(string(HDate), '%+', Now),
	http_log('/*~s*/ pengine(~3f, ~q).~n',
		 [HDate, Now, create(Pengine, Application, Options)]).
swish_log(send(Pengine, Event)) :-
	get_time(Now),
	format_time(string(HDate), '%+', Now),
	http_log('/*~s*/ pengine(~3f, ~q).~n',
		 [HDate, Now, send(Pengine, Event)]).

:- dynamic
	text_hash/3,
	gc_text_hash_time/1.

hash_option(src_text(Text), src_text(Result)) :- !,
	(   text_hash(Text, _, Hash)
	->  Result = Hash
	;   variant_sha1(Text, Hash),
	    get_time(Now),
	    assert(text_hash(Text, Now, Hash)),
	    gc_text_hash,
	    Result = Hash-Text
	).
hash_option(Option, Option).

gc_text_hash :-
	gc_text_hash_time(Last),
	get_time(Now),
	Now - Last < 900, !.
gc_text_hash :-
	get_time(Now),
	retractall(gc_text_hash_time(_)),
	asserta(gc_text_hash_time(Now)),
	Before is Now - 3600,
	(   text_hash(Text, Time, Hash),
	    Time < Before,
	    retractall(text_hash(Text, Time, Hash)),
	    fail
	;   true
	).

%!	create_log_dir
%
%	Create the directory for holding the log files

create_log_dir :-
	setting(http:logfile, Term),
	directory_spec(Term, DirSpec),
	(   absolute_file_name(DirSpec, _,
			       [ file_type(directory),
				 access(write),
				 file_errors(fail)
			       ])
	->  true
	;   absolute_file_name(DirSpec, Dir,
			       [ solutions(all)
			       ]),
	    catch(make_directory(Dir), _, fail)
	->  true
	).

directory_spec(Atom, Dir) :-
	atomic(Atom), !,
	file_directory_name(Atom, Dir).
directory_spec(Term, DirTerm) :-
	Term =.. [Alias,Atom],
	file_directory_name(Atom, Dir),
	DirTerm =.. [Alias,Dir].
