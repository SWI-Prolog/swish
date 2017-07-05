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

:- module(replay,
	  [ load_log/1,			% +File
	    replay/0,
	    replay_after/1,		% +Time
	    replay/1,			% +Pengine
	    replay/2,			% +Pengine, +ServerURL
	    concurrent_replay/1,	% +Count
	    skip_pengine/1		% +Pengine
	  ]).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(thread)).

/** <module> Replay SWISH sessions from a log file

This module can be used if the server is configured for extended logging
by loading logging.pl. The typical scenario is to use `grep` to select a
range of log messages from the log   file  during which the SWISH server
behaved suspicious, e.g., appears to leak memory or crash. To replay the
log, perform the following steps:

  - Start a pengine server at http://localhost:3050
  - Load this file using =|swipl lib/replay.pl|=
  - Load the log file to be examined using load_log/1.
  - Replay the first pengine interaction using replay/0. Hit
    SPACE to replay the next pengine interaction.

@bug	If the pengine requires a lot of time to execute you need to
	wait long.  Eventually we want to background the processing
	of such pengines.  This doesn't work yet.  A work around is
	to run the thread monitor (=|?- prolog_ide(thread_monitor).|=)
	in the pengine server and abort the long running pengine.

	Alternatively, set a short time limit on the server.  This may
	of course fail to reproduce the problem.  As the server never
	has to wait long for the user anyway, it is alsu wise to reduce
	the idle limit:

	    set_setting(swish:time_limit, 2).
	    set_setting(swish:idle_limit, 5).
*/

:- debug(playback(event)).
:- debug(playback(create)).
:- debug(playback(source)).
:- debug(playback(query)).
:- debug(playback(background)).
:- debug(playback(concurrent)).
%:- debug(playback(timing)).		% Do real timing

:- dynamic
	pengine/2,
	skip_pengine_store/1.

%%	replay is nondet.
%
%	Find the first Pengine in  the  log   data  and  execute  it. On
%	backtracking,  find  the  next  Pengine.  You  can  execute  all
%	pengines using:
%
%	  ==
%	  ?- forall(replay, true).
%	  ==

replay :-
	pengine_in_log(Pengine, _StartTime, Src),
	\+ skip_pengine_store(Pengine),
	Src \== (-),
	catch(replay(Pengine), E,
	      print_message(warning, E)).

replay_after(Time) :-
	pengine_in_log(Pengine, StartTime, Src),
	StartTime >= Time,
	\+ skip_pengine_store(Pengine),
	Src \== (-),
	replay(Pengine).


%%	concurrent_replay(+Count) is det.
%
%	Perform a concurrent replay over Count threads.

concurrent_replay(Count) :-
	findall(Pengine, pengine_in_log(Pengine, _StartTime, _Src), Pengines),
	length(Pengines, Len),
	debug(playback(concurrent), 'Replaying ~D pengines', Len),
	maplist([P, replay1(P)]>>true, Pengines, Goals),
	concurrent(Count, Goals, []).

replay1(P) :-
	catch(replay(P), E, print_message(warning, E)), !.
replay1(P) :-
	print_message(error, replay(P, failed)).


%%	replay(+Pengine) is det.
%
%	Same as replay(Pengine, 'http://localhost:3050/').

replay(Pengine) :-
	replay(Pengine, 'http://localhost:3050/').

%%	replay(+Pengine, +ServerURL) is det.
%
%	Replay pengine with id Pengine on server ServerURL.

replay(Pengine, URL) :-
	pengine_interaction(Pengine, StartTime, CreateOptions, Messages),
	format_time(string(D), '%+', StartTime),
	debug(playback(create), '*** ~q at ~s', [Pengine, D]),
	show_source(CreateOptions),
	pengine_create([ server(URL),
			 id(Id)
		       | CreateOptions
		       ]),
	debug(playback(create), '*** ~q', [Id]),
	get_time(Now),
	run(Messages, Now, Id, []).

pengine_interaction(Pengine, StartTime, CreateOptions, Messages) :-
	once(pengine(StartTime, create(Pengine, swish, Options0))),
	maplist(fix_option, Options0, CreateOptions),
	findall(Time-Message,
		(   pengine(Time0, send(Pengine, Message)),
		    Time is Time0-StartTime
		),
		Messages).

fix_option(src_text(_Hash-Text), src_text(Text)) :- !.
fix_option(src_text(Hash), src_text(Text)) :- !,
	pengine(_Time, create(_, swish, Options)),
	memberchk(src_text(Hash-Text), Options), !.
fix_option(Option, Option).

show_source(Options) :-
	option(src_text(Source), Options, ""),
	(   debugging(playback(source))
	->  format(user_error, '~N==~n~s~N==~n', [Source])
	;   true
	).

run([], _, _, _) :- !.
run(Messages, StartTime, Id, Options) :-
	pengine_event(Event, [listen(Id)]),
	reply(Event, Id, StartTime, Messages, Messages1),
	run(Messages1, StartTime, Id, Options).

reply(output(_Id, Prompt), Pengine, StartTime, [Time-pull_response|T], T) :- !,
	debug(playback(event), 'Output ~p (pull_response)', [Prompt]),
	sync_time(StartTime, Time),
	pengine_pull_response(Pengine, []).
reply(error(Id, time_limit_exceeded), Pengine, _, Msgs, []) :- !,
	catch(pengine_destroy(Id), _, true),
	(   Msgs == []
	->  true
	;   print_message(error, replay(Pengine, timeout(Msgs)))
	).
reply(destroy(Id, error(Id, time_limit_exceeded)),
      Pengine, _, Msgs, []) :- !,
	(   Msgs == []
	->  true
	;   print_message(error, replay(Pengine, timeout(Msgs)))
	).
reply(Event, Pengine, StartTime, [Time-H|T], T) :-
	debug(playback(event), 'Received ~p, reply: ~p', [Event, H]),
	sync_time(StartTime, Time),
	(   catch(pengine_send(H, Pengine), E,
		  print_message(error, E))
	->  true
	;   print_message(error, replay(Pengine, failed(H)))
	).

sync_time(StartTime, Time) :-
	debugging(playback(timing)), !,
	get_time(Now),
	Sleep is (StartTime+Time) - Now,
	debug(playback(event), '  sleep: ~3f ...', [Sleep]),
	sleep(Sleep).
sync_time(_, _).

pengine_send(ask(Question,Options0), Id) :-
	maplist(fix_ask_option(Id), Options0, Options),
	debug(playback(query), 'ask ~p', [Question]),
	pengine_ask(Id, Question, Options).
pengine_send(next, Id) :-
	debug(playback(query), 'next', []),
	pengine_next(Id, []).
pengine_send(next(N), Id) :-
	debug(playback(query), 'next ~p', [N]),
	pengine_next(Id, [chunk(N)]).
pengine_send(stop, Id) :-
	debug(playback(query), 'stop', []),
	pengine_stop(Id, []).
pengine_send(input(Input), Id) :-
	pengine_respond(Id, Input, []).
pengine_send(output(_Prompt), Id) :-
	pengine_pull_response(Id, []).
pengine_send(destroy, Id) :-
	pengine_destroy(Id).
pengine_send(pull_response, Id) :-
	pengine_pull_response(Id, []).

%%	fix_ask_option(+Pengine, +AskOption, -NewAskOption) is det.
%
%	Fixed breakpoint options to refer to the new pengine.

fix_ask_option(Id, breakpoints(List0), breakpoints(List)) :- !,
	maplist(fix_breakpoint(Id), List0, List).
fix_ask_option(_, Option, Option).

fix_breakpoint(Id, BP0, BP) :-
	fix_file(BP0.file, Id, NewFile), !,
	BP = BP0.put(file, NewFile).
fix_breakpoint(_, BP, BP).

fix_file(OldFile, Pengine, NewFile) :-
	split_string(OldFile, "/", "", Parts),
	select(OldPengine, Parts, Pengine, NewParts),
	is_uuid_string(OldPengine), !,
	atomics_to_string(NewParts, "/", NewFile).

is_uuid_string(String) :-
	split_string(String, "-", "", Parts),
	maplist(string_length, Parts, [8,4,4,4,12]).

%%	pengine_in_log(-Id, -StartTime, -Src)
%
%	True if Pengine was created in the session

pengine_in_log(Pengine, StartTime, Src) :-
	pengine(StartTime, create(Pengine, swish, Options0)),
	(   maplist(fix_option, Options0, Options)
	->  option(src_text(Src), Options)
	;   Src = (-)
	),
	\+ for_source_only(Pengine, Options0).

for_source_only(Pengine, Options) :-
	option(src_text(_Hash-_Text), Options),
	\+ pengine(_, send(Pengine, _)).

%%	load_log(+Log)
%
%	Load a log file.

load_log(Log) :-
	retractall(pengine(_,_)),
	absolute_file_name(Log, Path,
			   [ access(read),
			     extensions(['', log])
			   ]),
	setup_call_cleanup(
	    open(Path, read, In, [encoding(utf8)]),
	    read_log(In),
	    close(In)).

read_log(In) :-
	read_term(In, Term,
		  [ syntax_errors(dec10)
		  ]),
	read_log(Term, In).

read_log(end_of_file, _) :- !.
read_log(Term, In) :-
	assert_event(Term),
	read_log(In).

assert_event(pengine(Time, Action)) :- !,
	assertz(pengine(Time, Action)).
assert_event(request(_Id, Time, Request)) :-
	memberchk(path('/pengine/pull_response'), Request),
	memberchk(search(Fields), Request),
	memberchk(id=Pengine, Fields), !,
	assertz(pengine(Time, send(Pengine, pull_response))).
assert_event(_).

skip_pengine(Pengine) :-
	assertz(skip_pengine_store(Pengine)).

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message//1.

prolog:message(replay(Pengine, timeout(Msgs))) -->
	{ length(Msgs, Len) },
	[ 'Terminated ~q on timeout (~D messages left)'-[ Pengine, Len ] ].
prolog:message(replay(Pengine, failed(H))) -->
	[ 'Replay on ~q for ~q failed'-[Pengine, H] ].
prolog:message(replay(Pengine, failed)) -->
	[ 'Replay of ~q failed'-[Pengine] ].
