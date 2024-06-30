/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2024, VU University Amsterdam
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

:- module(procps,
	  [ procps_stat/1,		% -Stat
	    procps_stat/2,		% +Pid, -Stat
	    procps_thread_stat/2,	% ?Thread, -Stat
	    procps_status/1,		% -Status
	    procps_status/2		% +Pid, -Status
	  ]).
:- if(exists_source(library(unix))).
:- use_module(library(unix)).
:- endif.
:- use_module(library(lists)).

/** <module> Get process statistics from Linux /proc
*/

		 /*******************************
		 *	  /proc/[pid]/stat	*
		 *******************************/

%!	procps_stat(-Stat:dict) is det.
%!	procps_stat(+PID, -Stat:dict) is det.
%!	procps_thread_stat(+Thread, -Stat:dict) is det.
%
%	Get data from the  `stat`  file   of  the  current  process, the
%	process identified by `PID` or the   Prolog thread identified by
%	`Thread`. In all cases, this returns   a dict tagged `stat` with
%	the field values as defined by ``man 5 proc``.
%
%	@error existence_error(source_sink, _) if the system does not
%	provide the ``/proc`` filesystem.

procps_stat(Stat) :-
	stat_file_dict('/proc/self/stat', Stat).
procps_stat(Pid, Stat) :-
	atomic_list_concat(['/proc/', Pid, '/stat'], StatFile),
	stat_file_dict(StatFile, Stat).

procps_thread_stat(Thread, Stat) :-
	thread_property(Thread, system_thread_id(TID)),
	atomic_list_concat(['/proc/self/task/', TID, '/stat'], StatFile),
	stat_file_dict(StatFile, Stat).

stat_file_dict(StatFile, Stat) :-
	setup_call_cleanup(
	    open(StatFile, read, In),
	    read_string(In, _, String),
	    close(In)),
	split_string(String, " ", " \n", Parts),
	parts_pairs(Parts, 1, Pairs),
	dict_pairs(Stat, stat, Pairs).

parts_pairs([], _, []).
parts_pairs([H0|T0], I0, [H|T]) :-
	part_pair(H0, I0, H),
	I is I0+1,
	parts_pairs(T0, I, T).

part_pair(String, I, Key-Value) :-
	stat_field(Key, I), !,
	stat_field_value(Key, String, Value).
part_pair(String, I, I-String).

stat_field_value(comm, String, Command) :- !,
	sub_string(String, 1, _, 1, Command).
stat_field_value(state, String, Atom) :- !,
	atom_string(Atom, String).
stat_field_value(Field, String, Seconds) :-
	time_field(Field), !,
	number_string(ClockTicks, String),
	clockticks(TicksPerSec),
	Seconds is ClockTicks/TicksPerSec.
stat_field_value(Field, String, Bytes) :-
	page_field(Field), !,
	number_string(Pages, String),
	pagesize(BytesPerPage),
	Bytes is Pages*BytesPerPage.
stat_field_value(_, String, Number) :-
	number_string(Number, String).

:- if(current_predicate(sysconf/1)).
% the weird way to call sysconf confuses ClioPatria's cpack code
% analysis enough to accept this ...
term_expansion(clockticks(sysconf), Expansion) :-
	(   member(Sysconf, [sysconf(clk_tck(TicksPerSec))]),
	    call(Sysconf)
	->  Expansion = clockticks(TicksPerSec)
	;   Expansion = clockticks(100)
	).
term_expansion(pagesize(sysconf), Expansion) :-
	(   member(Sysconf, [sysconf(pagesize(Bytes))]),
	    call(Sysconf)
	->  Expansion = pagesize(Bytes)
	;   Expansion = pagesize(4096)
	).
clockticks(sysconf).
pagesize(sysconf).
:- else.
clockticks(100).
pagesize(4096).
:- endif.

time_field(utime).
time_field(stime).
time_field(cutime).
time_field(cstime).
time_field(starttime).

page_field(rss).

stat_field(pid,			  1).
stat_field(comm,		  2).
stat_field(state,		  3).
stat_field(ppid,		  4).
stat_field(pgrp,		  5).
stat_field(session,		  6).
stat_field(tty_nr,		  7).
stat_field(tpgid,		  8).
stat_field(flags,		  9).
stat_field(minflt,		  10).
stat_field(cminflt,		  11).
stat_field(majflt,		  12).
stat_field(cmajflt,		  13).
stat_field(utime,		  14).
stat_field(stime,		  15).
stat_field(cutime,		  16).
stat_field(cstime,		  17).
stat_field(priority,		  18).
stat_field(nice,		  19).
stat_field(num_threads,		  20).
stat_field(itrealvalue,		  21).
stat_field(starttime,		  22).
stat_field(vsize,		  23).
stat_field(rss,			  24).
stat_field(rsslim,		  25).
stat_field(startcode,		  26).
stat_field(endcode,		  27).
stat_field(startstack,		  28).
stat_field(kstkesp,		  29).
stat_field(kstkeip,		  30).
stat_field(signal,		  31).
stat_field(blocked,		  32).
stat_field(sigignore,		  33).
stat_field(sigcatch,		  34).
stat_field(wchan,		  35).
stat_field(nswap,		  36).
stat_field(cnswap,		  37).
stat_field(exit_signal,		  38).
stat_field(processor,		  39).
stat_field(rt_priority,		  40).
stat_field(policy,		  41).
stat_field(delayacct_blkio_ticks, 42).
stat_field(guest_time,		  43).
stat_field(cguest_time,		  44).
stat_field(start_data,		  45).
stat_field(end_data,		  46).
stat_field(start_brk,		  47).
stat_field(arg_start,		  48).
stat_field(arg_end,		  49).
stat_field(env_start,		  50).
stat_field(env_end,		  51).
stat_field(exit_code,		  52).


		 /*******************************
		 *	/proc/[pid]/status	*
		 *******************************/

%!	procps_status(-Status:dict) is det.
%!	procps_status(+PID, -Status:dict) is det.
%
%	Get the data from ``/proc/self/status`` as a Prolog dict.
%
%	@tbd Not all fields are currently translated.

procps_status(Stat) :-
	status_file_dict('/proc/self/status', Stat).
procps_status(Pid, Stat) :-
	atomic_list_concat(['/proc/', Pid, '/status'], StatFile),
	status_file_dict(StatFile, Stat).

status_file_dict(StatFile, Status) :-
	setup_call_cleanup(
	    open(StatFile, read, In),
	    read_string(In, _, String),
	    close(In)),
	split_string(String, "\n", " \n", Lines),
	status_line_pairs(Lines, Pairs),
	dict_pairs(Status, status, Pairs).

status_line_pairs([], []).
status_line_pairs([H0|T0], [Name-Value|T]) :-
	split_string(H0, ":", " \t", [NameS, ValueS]),
	string_lower(NameS, NameLS),
	atom_string(Name, NameLS),
	status_value(Name, ValueS, Value), !,
	status_line_pairs(T0, T).
status_line_pairs([_|T0], T) :-
	status_line_pairs(T0, T).

status_value(state, ValueS, State) :- !,
	split_string(ValueS, " ", " ", [Vs|_]),
	atom_string(State, Vs).
status_value(Name, ValueS, Bytes) :-
	sub_atom(Name, 0, _, _, vm), !,
	split_string(ValueS, " ", " ", [Vs,"kB"]),
	number_string(Kb, Vs),
	Bytes is Kb*1024.
status_value(Name, ValueS, Value) :-
	status_field(Name, list(Type)),
	!,
	split_string(ValueS, " \t", " \t", Values),
	maplist(to_type(Type), Values, Value).
status_value(Name, ValueS, Value) :-
	status_field(Name, Type),
	to_type(Type, ValueS, Value).

to_type(integer, String, Int) :-
	number_string(Int, String).
to_type(hex, String, Int) :-
	string_concat('0x', String, Hex),
	number_string(Int, Hex).

status_field(uid, list(integer)).
status_field(gid, list(integer)).
status_field(groups, list(integer)).
status_field(cpus_allowed, hex).
status_field(fdsize, integer).
status_field(threads, integer).
