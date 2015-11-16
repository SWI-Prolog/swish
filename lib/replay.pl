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

:- module(replay,
	  [ load_log/1,				% +File
	    replay/0,
	    replay/1,				% +Pengine
	    replay/2				% +Pengine, +ServerURL
	  ]).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(settings)).
:- use_module(library(option)).

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
*/

:- debug(playback(event)).
:- debug(playback(source)).
:- debug(playback(query)).
:- debug(playback(background)).

:- setting(background, number, 10,
	   "Background if it takes longer that seconds to reply").

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
	pengine_in_log(Pengine, StartTime, Src),
	Src \== (-),
	format_time(string(D), '%+', StartTime),
	format(user_error, '*** ~q at ~s ***~n', [Pengine, D]),
	replay(Pengine).

%%	replay(+Pengine) is det.
%
%	Same as replay(Pengine, 'http://localhost:3050/').

replay(Pengine) :-
	replay(Pengine, 'http://localhost:3050/').

%%	replay(+Pengine, +ServerURL) is det.
%
%	Replay pengine with id Pengine on server ServerURL.

replay(Pengine, URL) :-
	once(pengine(StartTime, create(Pengine, swish, Options0))),
	maplist(fix_option, Options0, Options),
	findall(Time-Message,
		(   pengine(Time0, send(Pengine, Message)),
		    Time is Time0-StartTime
		),
		Messages),
	option(src_text(Source), Options, ""),
	(   debugging(playback(source))
	->  format(user_error, '~N==~n~s~N==~n', [Source])
	;   true
	),
	pengine_create([ server(URL),
			 id(Id)
		       | Options
		       ]),
	get_time(Now),
	setting(background, BgTime),
	run(Messages, Now, Id, [backround(BgTime)]).

fix_option(src_text(_Hash-Text), src_text(Text)) :- !.
fix_option(src_text(Hash), src_text(Text)) :- !,
	pengine(_Time, create(_, swish, Options)),
	memberchk(src_text(Hash-Text), Options), !.
fix_option(Option, Option).

run([], _, _, _) :- !.
run(Messages, StartTime, Id, Options) :-
	(   option(backround(BgTime), Options, 0),
	    BgTime > 0
	->  pengine_event(Event, [listen(Id), timeout(BgTime)]),
	    (	Event == timeout
	    ->	background(Messages, StartTime, Id),
		Bg = true
	    ;	true
	    )
	;   pengine_event(Event, [listen(Id)])
	),
	(   Bg == true
	->  true
	;   reply(Event, Id, StartTime, Messages, Messages1),
	    run(Messages1, StartTime, Id, Options)
	).

reply(output(_Id, Prompt), Pengine, _, Msgs, Msgs) :- !,
	debug(playback(event), 'Output ~p (pull_response)', [Prompt]),
	pengine_pull_response(Pengine, []).
reply(Event, Pengine, StartTime, [Time-H|T], T) :-
	get_time(Now),
	Sleep is (StartTime+Time) - Now,
	debug(playback(event), 'Received ~p, reply: ~p (sleep: ~3f)',
	      [Event, H, Sleep]),
%	sleep(Sleep),
	debug(playback(reply), 'Reply: ~p', [H]),
	(   pengine_send(H, Pengine)
	->  true
	;   print_message(error, replay(Pengine, failed(H)))
	).

background(Messages, StartTime, Id) :-
	debug(background, 'Backgrounding ~q', [Id]),
	thread_create(run(Messages, StartTime, Id, [backround(0)]),
		      _, [detached(true)]).

pengine_send(ask(Question,Options), Id) :-
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

%%	pengine_in_log(-Id, -StartTime, -Src)
%
%	True if Pengine was created in the session

pengine_in_log(Pengine, StartTime, Src) :-
	pengine(StartTime, create(Pengine, swish, Options0)),
	(   maplist(fix_option, Options0, Options)
	->  option(src_text(Src), Options)
	;   Src = (-)
	).

%%	load_log(+Log)
%
%	Load a log file.

load_log(Log) :-
	absolute_file_name(Log, Path,
			   [ access(read),
			     extensions(['', log])
			   ]),
	setup_call_cleanup(
	    ( style_check(-discontiguous),
	      style_check(-singleton)
	    ),
	    consult(Path),
	    ( style_check(+discontiguous),
	      style_check(+singleton)
	    )).
