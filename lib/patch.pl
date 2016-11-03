/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
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

:- module(patch,
	  [ patch/4				% +Data, +Diff, -Merged, +Options
	  ]).
:- use_module(library(process)).
:- use_module(library(option)).

/** <module> Run patch program

This library uses the GNU patch(1) program to merge changes.
*/

:- predicate_options(patch/4, 4,
		     [ status(-compound),
		       stderr(-string)
		     ]).

%%	patch(+Orig:string, +Diff:string, -Merged:string, +Options)
%
%	Patch the string Orig using Diff.  Options:
%
%	  - status(-Status)
%	  Unify Status with the completion status of patch.  This
%	  is exit(0) for smooth completion and exit(1) if there are
%	  merge conflicts.
%	  - stderr(-String)
%	  Unify String with the data patch(1) sent to standard error.

patch(Orig, Diff, Merged, Options) :-
	setup_call_cleanup(
	    tmp_file_stream(utf8, TmpFile, Out),
	    (	call_cleanup(format(Out, '~s', [Orig]),
			     close(Out)),
		run_patch(TmpFile, Diff, Merged, Options)
	    ),
	    delete_file(TmpFile)).

run_patch(File, Diff, Merged, Options) :-
	thread_self(Me),
	setup_call_cleanup(
	    process_create(path(patch),
			   [ '--force', '--merge', '--silent',
			     '--output=-', file(File) ],
			   [ stdin(pipe(In)),
			     stdout(pipe(Out)),
			     stderr(pipe(Err)),
			     process(PID)
			   ]),
	    ( set_stream(In, encoding(utf8)),
	      set_stream(Out, encoding(utf8)),
	      set_stream(Err, encoding(utf8)),
	      thread_create(copy_diff(Diff, In), _, [detached(true)]),
	      thread_create(read_stderr(Me, Err), ErrThread, []),
	      read_string(Out, _, Merged)
	    ),
	    ( close(Out),
	      process_wait(PID, Status)
	    )),
	get_errors(ErrThread, Options),
	(   option(status(VarStat), Options)
	->  VarStat = Status
	;   true
	).

copy_diff(Diff, To) :-
	call_cleanup(
	    format(To, '~s', [Diff]),
	    close(To)).

read_stderr(Master, Stderr) :-
	read_string(Stderr, _, Errors),
	thread_send_message(Master, patch_errors(Errors)).

get_errors(Thread, Options) :-
	thread_join(Thread, Status),
	(   Status == true
	->  thread_get_message(patch_errors(Errors))
	;   Status == exception(Error)
	->  throw(Error)
	),
	(   option(stderr(Var), Options)
	->  Var = Errors
	;   true
	).
