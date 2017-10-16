/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2017, VU University Amsterdam
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

:- module(swish_debug,
	  [ pengine_stale_module/1,	% -Module
	    pengine_stale_module/2,	% -Module, -State
	    swish_statistics/1,		% -Statistics
	    start_swish_stat_collector/0,
	    swish_stats/2,		% ?Period, ?Dicts
	    swish_died_thread/2		% ?Thread, ?State
	  ]).
:- use_module(library(pengines)).
:- use_module(library(broadcast)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(aggregate)).
:- use_module(procps).
:- use_module(highlight).
:- if(exists_source(library(mallocinfo))).
:- use_module(library(mallocinfo)).
:- endif.

%%	pengine_stale_module(-M, -State) is nondet.
%
%	True if M seems to  be  a   pengine  module  with  no associated
%	pengine. State is a dict that describes   what we know about the
%	module.

pengine_stale_module(M) :-
	current_module(M),
	is_uuid(M),
	\+ live_module(M),
	\+ current_highlight_state(M, _).

pengine_stale_module(M, State) :-
	pengine_stale_module(M),
	stale_module_state(M, State).

live_module(M) :-
	pengine_property(Pengine, module(M)),
	pengine_property(Pengine, thread(Thread)),
	catch(thread_property(Thread, status(running)), _, fail).

stale_module_state(M, State) :-
	findall(N-V, stale_module_property(M, N, V), Properties),
	dict_create(State, stale, Properties).

stale_module_property(M, pengine, Pengine) :-
	pengine_property(Pengine, module(M)).
stale_module_property(M, pengine_queue, Queue) :-
	pengine_property(Pengine, module(M)),
	member(G, pengines:pengine_queue(Pengine, Queue, _TimeOut, _Time)),
	call(G).		% fool ClioPatria cpack xref
stale_module_property(M, pengine_pending_queue, Queue) :-
	pengine_property(Pengine, module(M)),
	member(G, [pengines:output_queue(Pengine, Queue, _Time)]),
	call(G).		% fool ClioPatria cpack xref
stale_module_property(M, thread, Thread) :-
	pengine_property(Pengine, module(M)),
	member(G, [pengines:pengine_property(Pengine, thread(Thread))]),
	call(G).		% fool ClioPatria cpack xref
stale_module_property(M, thread_status, Status) :-
	pengine_property(Pengine, module(M)),
	pengine_property(Pengine, thread(Thread)),
	catch(thread_property(Thread, status(Status)), _, fail).
stale_module_property(M, module_class, Class) :-
	module_property(M, class(Class)).
stale_module_property(M, program_space, Space) :-
	module_property(M, program_space(Space)).
stale_module_property(M, program_size, Size) :-
	module_property(M, program_size(Size)).
stale_module_property(M, predicates, List) :-
	current_module(M),
	findall(PI, pi_in_module(M, PI), List).
stale_module_property(UUID, highlight_state, State) :-
	current_highlight_state(UUID, State).

pi_in_module(M, Name/Arity) :-
	'$c_current_predicate'(_, M:Head),
	functor(Head, Name, Arity).

%%	swish_statistics(?State)
%
%	True if State is a statistics about SWISH

swish_statistics(highlight_states(Count)) :-
	aggregate_all(count, current_highlight_state(_,_), Count).
swish_statistics(pengines(Count)) :-
	aggregate_all(count, pengine_property(_,thread(_)), Count).
swish_statistics(remote_pengines(Count)) :-
	aggregate_all(count, pengine_property(_,remote(_)), Count).
swish_statistics(pengines_created(Count)) :-
	(   flag(pengines_created, Old, Old)
	->  Count = Old
	;   Count = 0
	).

:- listen(pengine(Action), swish_update_stats(Action)).

swish_update_stats(create(_Pengine, _Application, _Options0)) :-
	flag(pengines_created, Old, Old+1).
swish_update_stats(send(_Pengine, _Event)).


%%	is_uuid(@UUID)
%
%	True if UUID looks like a UUID

is_uuid(M) :-
	atom(M),
	atom_length(M, 36),
	forall(sub_atom(M, S, 1, _, C),
	       uuid_code(S, C)).

uuid_sep(8).
uuid_sep(13).
uuid_sep(18).
uuid_sep(23).

uuid_code(S, -) :- !, uuid_sep(S).
uuid_code(_, X) :- char_type(X, xdigit(_)).

		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

:- if(current_predicate(http_unix_daemon:http_daemon/0)).
:- use_module(library(broadcast)).
:- listen(http(post_server_start), start_swish_stat_collector).
:- else.
:- initialization
	start_swish_stat_collector.
:- endif.

%%	start_swish_stat_collector
%
%	Start collecting statistical  performance   information  for the
%	running SWISH server.

start_swish_stat_collector :-
	thread_property(_, alias(swish_stats)), !.
start_swish_stat_collector :-
	swish_stat_collector(swish_stats,
			     [ 60,	% collect a minute
			       60,	% collect an hour
			       24,	% collect a day
			       7,	% collect a week
			       52	% collect a year
			     ],
			     1).

swish_stat_collector(Name, Dims, Interval) :-
	atom(Name), !,
	thread_create(stat_collect(Dims, Interval), _, [alias(Name)]).
swish_stat_collector(Thread, Dims, Interval) :-
	thread_create(stat_collect(Dims, Interval), Thread, []).

%%	swish_stats(?Period, ?Stats:list(dict)) is nondet.
%
%	Get the collected statistics for the given Period. Period is one
%	of =minute=, =hour=, =day=, =week= or =year=. Stats is a list of
%	statistics structures, last  one  first.   The  =minute=  period
%	contains 60 second measurements, the hour 60 minutes, the day 24
%	hours, etc.  Each dict constains the following keys:
%
%	  * cpu
%	  Total process CPU time
%	  * d_cpu
%	  Differential CPU (is avg CPU per second)
%	  * pengines
%	  Number of running pengines
%	  * pengines_created
%	  Total number of pengines created
%	  * d_pengines_created
%	  Pengines created per second
%	  * rss
%	  Total resident memory
%	  * stack
%	  Memory in all Prolog stacks.

swish_stats(Name, Stats) :-
	stats_ring(Name, Ring),
	swish_stats(swish_stats, Ring, Stats).

stats_ring(minute, 1).
stats_ring(hour,   2).
stats_ring(day,	   3).
stats_ring(week,   4).
stats_ring(year,   5).

swish_stats(Name, Ring, Stats) :-
	thread_self(Me),
	catch(thread_send_message(Name, Me-get_stats(Ring)), E,
	      stats_died(Name, E)),
	thread_get_message(get_stats(Ring, Stats)).

stats_died(Alias, E) :-
	print_message(error, E),
	thread_join(Alias, Status),
	print_message(error, swish_stats(died, Status)),
	start_swish_stat_collector,
	fail.

stat_collect(Dims, Interval) :-
	new_sliding_stats(Dims, SlidingStat),
	get_time(Now),
	ITime is floor(Now),
	stat_loop(SlidingStat, _{}, ITime, Interval, [true]).

stat_loop(SlidingStat, Stat0, StatTime, Interval, Wrap) :-
	(   thread_self(Me),
	    thread_get_message(Me, Request,
			       [ deadline(StatTime)
			       ])
	->  (   reply_stats_request(Request, SlidingStat)
	    ->	true
	    ;	debug(swish_stats, 'Failed to process ~p', [Request])
	    ),
	    stat_loop(SlidingStat, Stat0, StatTime, Interval, Wrap)
	;   get_stats(Wrap, Stat1),
	    dif_stat(Stat1, Stat0, Stat),
	    push_sliding_stats(SlidingStat, Stat, Wrap1),
	    NextTime is StatTime+Interval,
	    stat_loop(SlidingStat, Stat1, NextTime, Interval, Wrap1)
	).

dif_stat(Stat1, Stat0, Stat) :-
	maplist(dif_field(Stat1, Stat0),
		[ cpu - d_cpu,
		  pengines_created - d_pengines_created
		],
		Fields), !,
	dict_pairs(Extra, _, Fields),
	put_dict(Extra, Stat1, Stat).
dif_stat(Stat, _, Stat).

dif_field(Stat1, Stat0, Key-DKey, DKey-DValue) :-
	DValue is Stat1.get(Key) - Stat0.get(Key).

reply_stats_request(Client-get_stats(Period), SlidingStat) :-
	arg(Period, SlidingStat, Ring),
	ring_values(Ring, Values),
	thread_send_message(Client, get_stats(Period, Values)).

%%	get_stats(+Wrap, -Stats:dict) is det.
%
%	Request elementary statistics.

get_stats(Wrap, Stats) :-
	Stats0 = stats{ cpu:CPU,
			rss:RSS,
			stack:Stack,
			pengines:Pengines,
			threads:Threads,
			pengines_created:PenginesCreated,
			time:Time
		      },
	get_time(Now),
	Time is floor(Now),
	statistics(process_cputime, PCPU),
	statistics(cputime, MyCPU),
	CPU is PCPU-MyCPU,
	statistics(stack, Stack),
	statistics(threads, Threads),
	catch(procps_stat(Stat), _,
	      Stat = stat{rss:0}),
	RSS = Stat.rss,
	swish_statistics(pengines(Pengines)),
	swish_statistics(pengines_created(PenginesCreated)),
	add_fordblks(Wrap, Stats0, Stats1),
	add_visitors(Stats1, Stats).

:- if(current_predicate(mallinfo/1)).
add_fordblks(Wrap, Stats0, Stats) :-
	(   Wrap = [true|_]
	->  member(G, [mallinfo(MallInfo)]),
	    call(G),			% fool ClioPatria xref
	    FordBlks = MallInfo.get(fordblks),
	    b_setval(fordblks, FordBlks)
	;   nb_current(fordblks, FordBlks)
	), !,
	Stats = Stats0.put(fordblks, FordBlks).
:- endif.
add_fordblks(_, Stats, Stats).

add_visitors(Stats0, Stats) :-
	broadcast_request(swish(visitor_count(C))), !,
	Stats = Stats0.put(visitors, C).
add_visitors(Stats, Stats).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Maintain sliding statistics. The statistics are maintained in a ring. If
the ring wraps around, the average is pushed to the next ring.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

new_sliding_stats(Dims, Stats) :-
	maplist(new_ring, Dims, Rings),
	compound_name_arguments(Stats, sliding_stats, Rings).

push_sliding_stats(Stats, Values, Wrap) :-
	push_sliding_stats(1, Stats, Values, Wrap).

push_sliding_stats(I, Stats, Values, [Wrap|WrapT]) :-
	arg(I, Stats, Ring),
	push_ring(Ring, Values, Wrap),
	(   Wrap == true
	->  average_ring(Ring, Avg),
	    I2 is I+1,
	    (	push_sliding_stats(I2, Stats, Avg, WrapT)
	    ->	true
	    ;	true
	    )
	;   WrapT = []
	).

new_ring(Dim, ring(0, Ring)) :-
	compound_name_arity(Ring, [], Dim).

push_ring(Ring, Value, Wrap) :-
	Ring = ring(Here0, Data),
	Here is Here0+1,
	compound_name_arity(Data, _, Size),
	Arg is (Here0 mod Size)+1,
	(   Arg == Size
	->  Wrap = true
	;   Wrap = false
	),
	nb_setarg(Arg, Data, Value),
	nb_setarg(1, Ring, Here).

ring_values(Ring, Values) :-
	Ring = ring(Here, Data),
	compound_name_arity(Data, _, Size),
	Start is Here - 1,
	End is Start - min(Here,Size),
	read_ring(Start, End, Size, Data, Values).

read_ring(End, End, _, _, []) :- !.
read_ring(Here0, End, Size, Data, [H|T]) :-
	A is (Here0 mod Size)+1,
	arg(A, Data, H),
	Here1 is Here0-1,
	read_ring(Here1, End, Size, Data, T).

average_ring(ring(_,Data), Avg) :-
	compound_name_arguments(Data, _, Dicts),
	average_dicts(Dicts, Avg).

average_dicts(Dicts, Avg) :-
	dicts_to_same_keys(Dicts, dict_fill(0), Dicts1),
	Dicts1 = [H|_],
	is_dict(H, Tag),
	dict_keys(H, Keys),
	length(Dicts1, Len),
	maplist(avg_key(Dicts1, Len), Keys, Pairs),
	dict_pairs(Avg, Tag, Pairs).

avg_key(Dicts, Len, Key, Key-Avg) :-
	maplist(get_dict(Key), Dicts, Values),
	sum_list(Values, Sum),
	Avg is Sum/Len.


%!	swish_died_thread(TID, Status) is nondet.
%
%	True if Id is a thread that died   with Status and has not (yet)
%	been joined. Note that such threads may exist for a short while.

swish_died_thread(TID, Status) :-
	findall(TID-Stat, (thread_property(Thread, status(Stat)),
			   Stat \== running,
			   thread_property(Thread, id(TID))), Pairs),
	member(TID-Stat, Pairs),
	status_message(Stat, Status).

status_message(exception(Ex), Message) :- !,
	message_to_string(Ex, Message0),
	string_concat('ERROR: ', Message0, Message).
status_message(Status, Status).


		 /*******************************
		 *	     SANDBOX		*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_debug:pengine_stale_module(_)).
sandbox:safe_primitive(swish_debug:pengine_stale_module(_,_)).
sandbox:safe_primitive(swish_debug:swish_statistics(_)).
sandbox:safe_primitive(swish_debug:swish_stats(_, _)).
sandbox:safe_primitive(swish_debug:swish_died_thread(_, _)).
