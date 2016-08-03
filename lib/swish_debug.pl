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

:- module(swish_debug,
	  [ pengine_stale_module/1,	% -Module, -State
	    pengine_stale_module/2,	% -Module, -State
	    swish_statistics/1,		% -Statistics
	    start_swish_stat_collector/0,
	    swish_stats/2		% ?Period, ?Dicts
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
stale_module_property(M, program_space, Space) :-
	module_property(M, program_space(Space)).


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
			pengines_created:PenginesCreated,
			time:Time
		      },
	get_time(Now),
	Time is floor(Now),
	statistics(process_cputime, PCPU),
	statistics(cputime, MyCPU),
	CPU is PCPU-MyCPU,
	statistics(stack, Stack),
	catch(procps_stat(Stat), _,
	      Stat = stat{rss:0}),
	RSS = Stat.rss,
	swish_statistics(pengines(Pengines)),
	swish_statistics(pengines_created(PenginesCreated)),
	add_fordblks(Wrap, Stats0, Stats).

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


		 /*******************************
		 *	     SANDBOX		*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_debug:pengine_stale_module(_)).
sandbox:safe_primitive(swish_debug:pengine_stale_module(_,_)).
sandbox:safe_primitive(swish_debug:swish_statistics(_)).
sandbox:safe_primitive(swish_debug:swish_stats(_, _)).
