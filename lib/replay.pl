/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2026, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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
	    load_log/2,			% +File, +Options
	    replay/0,
	    replay_after/1,		% +Time
	    replay/1,			% +Pengine
	    replay/2,			% +Pengine, +ServerURL
	    concurrent_replay/1,	% +Count
	    concurrent_replay/2,	% +Count, +Options
	    skip_pengine/1,		% +Pengine
	    log_pengines/1,		% -Count

	    pengine_source/2		% ?Pengine, ?Source
	  ]).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(thread)).
:- use_module(library(readutil)).
:- use_module(library(aggregate)).

/** <module> Replay SWISH sessions from a log file

This module can be used if the server is configured for extended logging
by loading logging.pl. The typical scenario is to select a range of log
messages from the log file during which the SWISH server behaved
suspicious, e.g., appears to leak memory or crash. To replay the log,
perform the following steps:

  - Start a pengine server at http://localhost:3050
  - Load this file using =|swipl lib/replay.pl|=
  - Load the log file to be examined using load_log/1 or load_log/2.
    The latter can select a part of the log, e.g.,

        ?- load_log('httpd-1.log', [max_pengines(1000)]).

  - Replay the first pengine interaction using replay/0. Hit
    SPACE to replay the next pengine interaction.  Alternatively,
    replay all pengines concurrently using

        ?- concurrent_replay(8, [progress(500)]).

@bug	If the pengine requires a lot of time to execute you need to
	wait long.  A work around is to set a short time limit on the
	server.  This may of course fail to reproduce the problem.  As the
	server never has to wait long for the user anyway, it is also wise
	to reduce the idle limit:

	    set_setting(swish:time_limit, 2).
	    set_setting(swish:idle_limit, 5).

	This is what =|run_replay.pl|= does.
*/

:- debug(playback(event)).
:- debug(playback(create)).
:- debug(playback(source)).
:- debug(playback(query)).
:- debug(playback(background)).
:- debug(playback(concurrent)).
%:- debug(playback(timing)).		% Do real timing

%   The log is stored indexed on the pengine:
%
%     - log_create(Pengine, Time, Application, Options)
%     - log_send(Pengine, Time, Message)
%     - log_source(Hash, Text)
%
%   The src_text(Hash-Text) options of log_create/4 are stored as
%   src_text(Hash) and the text is stored as log_source(Hash, Text).

:- dynamic
	log_create/4,
	log_send/3,
	log_source/2,
	skip_pengine_store/1,
	replay_start/2.

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
	catch(replay(Pengine), E, true),
	(   var(E)
	->  true
	;   E = error(socket_error(econnrefused, _), _)
	->  print_message(warning, E),
	    !					% server seems dead
	;   print_message(warning, E)
	).

replay_after(Time) :-
	pengine_in_log(Pengine, StartTime, Src),
	StartTime >= Time,
	\+ skip_pengine_store(Pengine),
	Src \== (-),
	replay(Pengine).


%%	concurrent_replay(+Count) is det.
%%	concurrent_replay(+Count, +Options) is det.
%
%	Replay the pengines of the log using Count threads.  Options:
%
%	  - server(+URL)
%	    Server to replay on.  Default http://localhost:3050/
%	  - limit(+Max)
%	    Replay at most Max pengines.
%	  - after(+Time)
%	    Only replay pengines created at or after Time.
%	  - progress(+N)
%	    Report progress after every N pengines.  Default 1000.
%	  - verbose(+Bool)
%	    If `false` (default), disable the playback(_) debug
%	    messages during the replay.
%	  - server_stats(+Bool)
%	    If `true` (default), add statistics of the server to the
%	    progress reports.  These are obtained using pengine_rpc/3
%	    on the `pengine_sandbox` application.
%
%	Prints a summary of the number of pengines that completed,
%	failed, raised an error and were terminated on a timeout.

concurrent_replay(Count) :-
	concurrent_replay(Count, []).

concurrent_replay(Count, Options) :-
	option(server(URL), Options, 'http://localhost:3050/'),
	option(progress(Every), Options, 1000),
	findall(Pengine, replay_candidate(Pengine, Options), Pengines0),
	(   option(limit(Max), Options)
	->  length(Pengines0, Len0),
	    Keep is min(Max, Len0),
	    length(Pengines, Keep),
	    append(Pengines, _, Pengines0)
	;   Pengines = Pengines0
	),
	length(Pengines, Len),
	debug(playback(concurrent), 'Replaying ~D pengines using ~D threads',
	      [Len, Count]),
	reset_counters,
	get_time(T0),
	retractall(replay_start(_,_)),
	option(server_stats(Stats), Options, true),
	assertz(replay_start(T0, Stats-URL)),
	verbose_debug(Options, Debug),
	call_cleanup(
	    concurrent_forall(member(P, Pengines),
			      replay1(P, URL, Every),
			      [threads(Count)]),
	    restore_debug(Debug)),
	get_time(T1),
	Time is T1-T0,
	print_message(informational, replay(summary(Len, Time))),
	server_stats(Stats-URL).

replay_candidate(Pengine, Options) :-
	pengine_in_log(Pengine, StartTime, Src),
	Src \== (-),
	\+ skip_pengine_store(Pengine),
	(   option(after(After), Options)
	->  StartTime >= After
	;   true
	).

replay1(P, URL, Every) :-
	catch(replay(P, URL), E, true),
	!,
	(   var(E)
	->  count(ok)
	;   count(error),
	    print_message(warning, E)
	),
	progress(Every).
replay1(P, _, Every) :-
	count(failed),
	print_message(error, replay(P, failed)),
	progress(Every).

counter(ok).
counter(failed).
counter(error).
counter(timeout).

reset_counters :-
	forall(counter(C),
	       ( atom_concat(replay_, C, Key),
		 flag(Key, _, 0)
	       )).

count(C) :-
	atom_concat(replay_, C, Key),
	flag(Key, N, N+1).

counts(Counts) :-
	findall(C-N,
		( counter(C),
		  atom_concat(replay_, C, Key),
		  flag(Key, N, N)
		),
		Counts).

progress(Every) :-
	flag(replay_done, N0, N0+1),
	N is N0+1,
	(   N mod Every =:= 0
	->  replay_start(T0, Server),
	    get_time(Now),
	    Time is Now-T0,
	    counts(Counts),
	    print_message(informational, replay(progress(N, Time, Counts))),
	    server_stats(Server)
	;   true
	).

server_stats(true-URL) :- !,
	catch(findall(K=V,
		      ( member(K, [modules, predicates, heapused, threads]),
			pengine_rpc(URL, statistics(K, V),
				    [ application(pengine_sandbox)
				    ])
		      ),
		      Stats),
	      E, (print_message(warning, E), fail)),
	!,
	print_message(informational, replay(server(Stats))).
server_stats(_).

verbose_debug(Options, Debug) :-
	findall(Topic, (debugging(playback(Topic), true)), Debug),
	(   option(verbose(true), Options)
	->  true
	;   forall(member(Topic, Debug), nodebug(playback(Topic)))
	),
	flag(replay_done, _, 0).

restore_debug(Debug) :-
	forall(member(Topic, Debug), debug(playback(Topic))).


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
	once(log_create(Pengine, StartTime, swish, Options0)),
	maplist(fix_option, Options0, CreateOptions),
	findall(Time-Message,
		(   log_send(Pengine, Time0, Message),
		    Time is Time0-StartTime
		),
		Messages).

fix_option(src_text(Hash), src_text(Text)) :- !,
	log_source(Hash, Text), !.
fix_option(Option, Option).

show_source(Options) :-
	option(src_text(Source), Options, ""),
	(   debugging(playback(source))
	->  format(user_error, '~N==~n~s~N==~n', [Source])
	;   true
	).

run([], _, Id, Options) :- !,
	finish(Id, Options).
run(Messages, StartTime, Id, Options) :-
	pengine_event(Event, [listen(Id), timeout(10)]),
	(   Event == timeout
	->  count(timeout),
	    print_message(error, replay(Id, timeout(Messages)))
	;   reply(Event, Id, StartTime, Messages, Messages1),
	    (   Messages = [_-destroy|Messages1]
	    ->  Options1 = [destroyed|Options]
	    ;   Event = destroy(_, _)
	    ->  Options1 = [destroyed|Options]
	    ;   Options1 = Options
	    ),
	    run(Messages1, StartTime, Id, Options1)
	).

%   finish(+Pengine, +Options)
%
%   All messages are sent.  Wait for the answer to the last one and
%   destroy the pengine, unless the last message destroyed it.

finish(_, Options) :-
	memberchk(destroyed, Options), !.
finish(Id, _) :-
	pengine_event(Event, [listen(Id), timeout(10)]),
	(   Event = destroy(_, _)
	->  true
	;   Event = destroy(_)
	->  true
	;   Event == timeout
	->  count(timeout),
	    catch(pengine_destroy(Id), _, true)
	;   debug(playback(event), 'Final ~p', [Event]),
	    catch(pengine_destroy(Id), _, true)
	).

reply(output(_Id, Prompt), Pengine, StartTime, [Time-pull_response|T], T) :- !,
	debug(playback(event), 'Output ~p (pull_response)', [Prompt]),
	sync_time(StartTime, Time),
	pengine_pull_response(Pengine, []).
reply(error(Id, time_limit_exceeded), Pengine, _, Msgs, []) :- !,
	catch(pengine_destroy(Id), _, true),
	(   Msgs == []
	->  true
	;   count(timeout),
	    print_message(error, replay(Pengine, timeout(Msgs)))
	).
reply(destroy(Id, error(Id, time_limit_exceeded)),
      Pengine, _, Msgs, []) :- !,
	(   Msgs == []
	->  true
	;   count(timeout),
	    print_message(error, replay(Pengine, timeout(Msgs)))
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
%	True if Pengine was created in the session.  Src is `-` if the
%	source is not in the log.

pengine_in_log(Pengine, StartTime, Src) :-
	log_create(Pengine, StartTime, swish, Options0),
	(   maplist(fix_option, Options0, Options)
	->  option(src_text(Src), Options)
	;   Src = (-)
	),
	\+ for_source_only(Pengine, Options0).

%   A pengine that only provides a source has no interaction.

for_source_only(Pengine, Options) :-
	option(src_text(_Hash), Options),
	\+ log_send(Pengine, _, _).

%%	log_pengines(-Count) is det.
%
%	Count is the number of replayable pengines in the loaded log.

log_pengines(Count) :-
	aggregate_all(count, pengine_in_log(_,_,_), Count).

%%	load_log(+Log) is det.
%%	load_log(+Log, +Options) is det.
%
%	Load a log file.  As the log may be large, we only parse lines
%	that hold pengine events or pull_response requests.  Options
%	select a part of the log:
%
%	  - lines(+From-To)
%	    Only process lines From..To (1-based, inclusive).  To may
%	    be `inf`.
%	  - from(+Time)
%	  - to(+Time)
%	    Only process events in this time range (Unix time stamps).
%	  - max_pengines(+Max)
%	    Ignore pengines created after the first Max.  After the
%	    Max-th pengine is created, read at most `tail` more lines
%	    for the remaining interactions of the loaded pengines.
%	  - tail(+Lines)
%	    See max_pengines.  Default 100,000.

load_log(Log) :-
	load_log(Log, []).

load_log(Log, Options) :-
	retractall(log_create(_,_,_,_)),
	retractall(log_send(_,_,_)),
	retractall(log_source(_,_)),
	absolute_file_name(Log, Path,
			   [ access(read),
			     extensions(['', log])
			   ]),
	option(lines(From-To), Options, 1-inf),
	option(max_pengines(Max), Options, inf),
	option(tail(Tail), Options, 100 000),
	State = state(0, 0, inf),		% line, pengines, last line
	statistics(cputime, T0),
	setup_call_cleanup(
	    open(Path, read, In, [encoding(utf8)]),
	    read_log(In, From, To, Max-Tail, Options, State),
	    close(In)),
	statistics(cputime, T1),
	T is T1-T0,
	arg(1, State, Lines),
	aggregate_all(count, log_create(_,_,_,_), Creates),
	aggregate_all(count, log_send(_,_,_), Sends),
	print_message(informational,
		      replay(loaded(Path, Lines, Creates, Sends, T))).

read_log(In, From, To, MaxTail, Options, State) :-
	repeat,
	  read_line_to_string(In, Line),
	  arg(1, State, N0),
	  N is N0+1,
	  nb_setarg(1, State, N),
	  (   Line == end_of_file
	  ->  !
	  ;   N > To
	  ->  !
	  ;   arg(3, State, Last),
	      N > Last
	  ->  !
	  ;   N < From
	  ->  fail
	  ;   log_line(Line, N, MaxTail, Options, State),
	      fail
	  ).

log_line(Line, N, MaxTail, Options, State) :-
	(   sub_string(Line, _, _, _, "*/ pengine(")
	->  true
	;   sub_string(Line, _, _, _, "/pengine/pull_response")
	),
	!,
	catch(term_string(Term, Line), E,
	      ( print_message(warning, replay(line(N), E)),
		fail )),
	assert_event(Term, MaxTail, Options, State).
log_line(_, _, _, _, _).

assert_event(pengine(Time, Action), MaxTail, Options, State) :- !,
	in_time_range(Time, Options),
	assert_action(Action, Time, MaxTail, State).
assert_event(request(_Id, Time, Request), _, Options, _State) :-
	in_time_range(Time, Options),
	memberchk(path('/pengine/pull_response'), Request),
	memberchk(search(Fields), Request),
	memberchk(id=Pengine, Fields),
	log_create(Pengine, _, _, _), !,
	assertz(log_send(Pengine, Time, pull_response)).
assert_event(_, _, _, _).

assert_action(create(Pengine, App, Options0), Time, Max-Tail, State) :- !,
	maplist(store_source, Options0, Options),
	(   arg(2, State, C0),
	    C0 < Max
	->  assertz(log_create(Pengine, Time, App, Options)),
	    (   App == swish
	    ->  C is C0+1,
		nb_setarg(2, State, C),
		(   C =:= Max
		->  arg(1, State, Line),
		    Last is Line+Tail,
		    nb_setarg(3, State, Last)
		;   true
		)
	    ;   true
	    )
	;   true				% keep sources
	).
assert_action(send(Pengine, Message), Time, _, _) :-
	log_create(Pengine, _, _, _), !,	% created in the selected part
	assertz(log_send(Pengine, Time, Message)).
assert_action(_, _, _, _).

store_source(src_text(Hash-Text), src_text(Hash)) :- !,
	(   log_source(Hash, _)
	->  true
	;   assertz(log_source(Hash, Text))
	).
store_source(Option, Option).

in_time_range(Time, Options) :-
	(   option(from(From), Options)
	->  Time >= From
	;   true
	),
	(   option(to(To), Options)
	->  Time =< To
	;   true
	).

skip_pengine(Pengine) :-
	assertz(skip_pengine_store(Pengine)).

		 /*******************************
		 *	       QUERY		*
		 *******************************/

pengine_source(Pengine, Src) :-
	log_create(Pengine, _Time, _App, Options),
	memberchk(src_text(Hash), Options),
	log_source(Hash, Src).



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
prolog:message(replay(line(N), E)) -->
	[ 'Line ~D: '-[N] ],
	'$messages':translate_message(E).
prolog:message(replay(loaded(Path, Lines, Creates, Sends, Time))) -->
	[ 'Loaded ~w: ~D lines, ~D pengines, ~D messages in ~2f sec'-
	  [Path, Lines, Creates, Sends, Time] ].
prolog:message(replay(progress(N, Time, Counts))) -->
	{ Rate is N/max(Time, 0.001) },
	[ 'Replayed ~D pengines in ~1f sec (~1f/sec)'-[N, Time, Rate] ],
	counts(Counts).
prolog:message(replay(summary(N, Time))) -->
	{ counts(Counts) },
	[ 'Replay of ~D pengines completed in ~1f sec'-[N, Time] ],
	counts(Counts).

prolog:message(replay(server(Stats))) -->
	[ '  server:' ],
	server_stats(Stats).

server_stats([]) --> [].
server_stats([K=V|T]) --> [ ' ~w: ~D'-[K, V] ], server_stats(T).

counts([]) --> [].
counts([C-N|T]) --> [ ', ~w: ~D'-[C, N] ], counts(T).
