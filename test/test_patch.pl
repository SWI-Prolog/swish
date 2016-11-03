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

:- module(test_patch, [ test_patch/0 ]).
:- use_module(library(plunit)).
:- use_module(library(process)).
:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(readutil)).
:- use_module('../lib/patch').

test_patch :-
	run_tests([patch]).

:- begin_tests(patch).

test(simple, Merged+Status == Final+exit(0)) :-
	run(diff('-u', file('diff/f0.txt'), file('diff/f1.txt')), Diff, _),
	read_file_to_string('diff/f0.txt', Orig, []),
	read_file_to_string('diff/f1.txt', Final, []),
	patch(Orig, Diff, Merged, [status(Status)]).
test(conflict, Status+Failures == exit(1)+1) :-
	run(diff('-u', file('diff/f0.txt'), file('diff/f1.txt')), Diff, _),
	read_file_to_string('diff/f2.txt', Orig, []),
	patch(Orig, Diff, Merged, [status(Status), stderr(Errors)]),
	aggregate_all(count, sub_string(Errors, _,_,_, 'Hunk #'), Failures),
	assertion(sub_string(Merged, _, _, _, "<<<<<")),
	assertion(sub_string(Merged, _, _, _, ">>>>>")),
	debug(patch,
	      'Diff:~n~s~nMerged:~n~s~nStatus: ~p', [Diff, Merged, Status]).

:- end_tests(patch).

run(Command, Output, Status) :-
	Command =.. [Exe|Argv],
	setup_call_cleanup(
	    process_create(path(Exe), Argv, [stdout(pipe(Out)), process(Pid)]),
	    read_string(Out, _, Output),
	    close(Out)),
	process_wait(Pid, Status).
