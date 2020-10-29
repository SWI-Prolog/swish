/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2020, VU University Amsterdam
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

:- module(swish_debug,
          [ pengine_stale_module/1,     % -Module
            pengine_stale_module/2,     % -Module, -State
            stale_pengine/1,            % -Pengine
            swish_statistics/1,         % -Statistics
            start_swish_stat_collector/0,
            swish_stats/2,              % ?Period, ?Dicts
            swish_save_stats/1,         % ?File
            swish_died_thread/2         % ?Thread, ?State
          ]).
:- use_module(library(pengines)).
:- use_module(library(broadcast)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(aggregate)).
:- use_module(library(settings)).
:- use_module(procps).
:- use_module(highlight).
:- if(exists_source(library(mallocinfo))).
:- use_module(library(mallocinfo)).
:- export(malloc_info/1).
:- endif.
:- use_module(swish_redis).
:- use_module(config).

:- setting(stats_file, callable, data('stats.db'),
           "Save statistics to achieve a long term view").
:- setting(stats_interval, integer, 300,        % 5 minutes
           "Save stats every N seconds").

redis_key(Server, Key) :-
    swish_config(redis, Server),
    swish_config(redis_prefix, Prefix),
    redis_consumer(Consumer),
    atomic_list_concat([Prefix, stat | Consumer], :, Key).

wait_redis_key(Server, Key) :-
    between(1, 10, X),
    (   redis_key(Server, Key)
    ->  !
    ;   Wait is (1<<X)*0.1,
        sleep(Wait),
        fail
    ).

use_redis :-
    swish_config(redis, _).


%!  stale_pengine(-Pengine) is nondet.
%
%   True if Pengine is a Pengine who's thread died.

stale_pengine(Pengine) :-
    pengine_property(Pengine, thread(Thread)),
    \+ catch(thread_property(Thread, status(running)), _, fail).


%!  pengine_stale_module(-M) is nondet.
%!  pengine_stale_module(-M, -State) is nondet.
%
%   True if M seems to  be  a   pengine  module  with  no associated
%   pengine. State is a dict that describes   what we know about the
%   module.

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
    member(G, [pengines:pengine_queue(Pengine, Queue, _TimeOut, _Time)]),
    call(G).                % fool ClioPatria cpack xref
stale_module_property(M, pengine_pending_queue, Queue) :-
    pengine_property(Pengine, module(M)),
    member(G, [pengines:output_queue(Pengine, Queue, _Time)]),
    call(G).                % fool ClioPatria cpack xref
stale_module_property(M, thread, Thread) :-
    pengine_property(Pengine, module(M)),
    member(G, [pengines:pengine_property(Pengine, thread(Thread))]),
    call(G).                % fool ClioPatria cpack xref
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

%!  swish_statistics(?State)
%
%   True if State is a statistics about SWISH

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


%!  is_uuid(@UUID)
%
%   True if UUID looks like a UUID

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
                 *           STATISTICS         *
                 *******************************/

:- if(current_predicate(http_unix_daemon:http_daemon/0)).
:- use_module(library(broadcast)).
:- listen(http(post_server_start), start_swish_stat_collector).
:- else.
:- initialization
    start_swish_stat_collector.
:- endif.

%!  start_swish_stat_collector
%
%   Start collecting statistical  performance   information  for the
%   running SWISH server.

start_swish_stat_collector :-
    thread_property(_, alias(swish_stats)),
    !.
start_swish_stat_collector :-
    persistent_stats(Persists),
    swish_stat_collector(
        swish_stats,
                    % Time collected |  Ticks  | Push
        [ 60,       %       1 min    |  1 sec  |  1 min
          60/10,    %       1 hr     |  1 min  | 10 min
          24*6/6,   %       1 day    | 10 min  |  1 hr
          7*24/24,  %       1 week   |  1 hr   |  1 day
          52        %       1 yr     |  1 day
        ],
        1,
        Persists),
    at_halt(swish_save_stats(_)).

swish_stat_collector(Name, Dims, Interval, Persists) :-
    atom(Name),
    !,
    thread_create(stat_collect(Dims, Interval, Persists), _, [alias(Name)]).
swish_stat_collector(Thread, Dims, Interval, Persists) :-
    thread_create(stat_collect(Dims, Interval, Persists), Thread, []).

persistent_stats(save(Path, Interval)) :-
    setting(stats_interval, Interval),
    Interval > 0,
    setting(stats_file, File),
    (   absolute_file_name(File, Path,
                           [ access(write),
                             file_errors(fail)
                           ])
    ->  true
    ;   File =.. [Alias,_],
        DirSpec =.. [Alias, '.'],
        absolute_file_name(DirSpec, Dir,
                       [ solutions(all)
                       ]),
        \+ exists_directory(Dir),
        catch(make_directory(Dir),
              error(permission_error(create, directory, Dir), _),
              fail),
        absolute_file_name(File, Path,
                           [ access(write),
                             file_errors(fail)
                           ])
    ),
    !.
persistent_stats(save(-, 0)).



%!  swish_stats(?Period, ?Stats:list(dict)) is nondet.
%
%   Get the collected statistics for the given Period. Period is one
%   of =minute=, =hour=, =day=, =week= or =year=. Stats is a list of
%   statistics structures, last  one  first.   The  =minute=  period
%   contains 60 second measurements, the hour 60 minutes, the day 24
%   hours, etc.  Each dict constains the following keys:
%
%     * cpu
%     Total process CPU time
%     * d_cpu
%     Differential CPU (is avg CPU per second)
%     * pengines
%     Number of running pengines
%     * pengines_created
%     Total number of pengines created
%     * d_pengines_created
%     Pengines created per second
%     * rss
%     Total resident memory
%     * stack
%     Memory in all Prolog stacks.

swish_stats(Name, Stats) :-
    stats_ring(Name, Ring),
    swish_stats(swish_stats, Ring, Stats).

stats_ring(minute, 1).
stats_ring(hour,   2).
stats_ring(day,    3).
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

stat_collect(Dims, Interval, Persists) :-
    restart_sliding_stats(Persists, Dims, SlidingStat),
    get_time(Now),
    ITime is floor(Now),
    stat_loop(SlidingStat, _{}, ITime, Interval, Persists, [true]).

stat_loop(SlidingStat, Stat0, StatTime, Interval, Persists, Wrap) :-
    (   thread_self(Me),
        thread_get_message(Me, Request,
                           [ deadline(StatTime)
                           ])
    ->  (   reply_stats_request(Request, SlidingStat)
        ->  true
        ;   debug(swish_stats, 'Failed to process ~p', [Request])
        ),
        stat_loop(SlidingStat, Stat0, StatTime, Interval, Persists, Wrap)
    ;   get_stats(Wrap, Stat1),
        dif_stat(Stat1, Stat0, Stat),
        push_sliding_stats(SlidingStat, Stat, Wrap1),
        NextTime is StatTime+Interval,
        save_stats(Persists, SlidingStat),
        stat_loop(SlidingStat, Stat1, NextTime, Interval, Persists, Wrap1)
    ).

dif_stat(Stat1, Stat0, Stat) :-
    maplist(dif_field(Stat1, Stat0),
            [ cpu - d_cpu,
              pengines_created - d_pengines_created
            ],
            Fields),
    !,
    dict_pairs(Extra, _, Fields),
    put_dict(Extra, Stat1, Stat).
dif_stat(Stat, _, Stat).

dif_field(Stat1, Stat0, Key-DKey, DKey-DValue) :-
    DValue is Stat1.get(Key) - Stat0.get(Key).

reply_stats_request(Client-get_stats(Period), SlidingStat) :-
    !,
    arg(Period, SlidingStat, Ring),
    ring_values(Ring, Values),
    thread_send_message(Client, get_stats(Period, Values)).
reply_stats_request(Client-save_stats(File), SlidingStat) :-
    !,
    (   var(File)
    ->  persistent_stats(save(File, _Interval))
    ;   true
    ),
    catch(save_stats_file(File, SlidingStat), E, true),
    (   var(E)
    ->  thread_send_message(Client, save_stats(File))
    ;   thread_send_message(Client, save_stats(error(E)))
    ).


%!  get_stats(+Wrap, -Stats:dict) is det.
%
%   Request elementary statistics.

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
    add_heap(Stats1, Stats2),
    add_visitors(Stats2, Stats).

:- if(current_predicate(malloc_property/1)).
add_heap(Stats0, Stats) :-
    malloc_property('generic.current_allocated_bytes'(Heap)),
    Stats = Stats0.put(heep, Heap).
:- else.
add_heap(Stats, Stats).
:- endif.

:- if(current_predicate(malloc_property/1)).

add_fordblks(_, Stats0, Stats) :-
    malloc_property('generic.current_allocated_bytes'(Used)),
    malloc_property('generic.heap_size'(Heap)),
    !,
    FordBlks is Heap - Used,
    Stats = Stats0.put(fordblks, FordBlks).

:- elif(current_predicate(mallinfo/1)).
:- dynamic fordblks_wrap/1.
fordblks_wrap(0).

add_wrap(0) :- !.
add_wrap(Amount) :-
    retract(fordblks_wrap(Wrap0)),
    Wrap1 is Wrap0+Amount,
    asserta(fordblks_wrap(Wrap1)).

fix_fordblks_wrap(FordBlks0, FordBlks) :-
    fordblks_wrap(Wrap),
    FordBlks1 is FordBlks0+Wrap,
    (   nb_current(fordblks, Prev)
    ->  NW is FordBlks0 mod (1<<32),
        PW is Prev mod (1<<32),
        (   PW > (1<<32)-(1<<30),
            NW < (1<<30)
        ->  Add is 1<<32
        ;   NW > (1<<32)-(1<<30),
            PW < (1<<30)
        ->  Add is -(1<<32)
        ;   Add = 0
        ),
        add_wrap(Add),
        FordBlks = FordBlks1+Add
    ;   FordBlks = FordBlks1
    ).

add_fordblks(Wrap, Stats0, Stats) :-
    (   Wrap = [true|_]
    ->  member(G, [mallinfo(MallInfo)]),
        call(G),                    % fool ClioPatria xref
        FordBlks0 = MallInfo.get(fordblks),
        fix_fordblks_wrap(FordBlks0, FordBlks),
        b_setval(fordblks, FordBlks)
    ;   nb_current(fordblks, FordBlks)
    ),
    !,
    Stats = Stats0.put(fordblks, FordBlks).
:- endif.
add_fordblks(_, Stats, Stats).

add_visitors(Stats0, Stats) :-
    broadcast_request(swish(visitor_count(C))),
    !,
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
        (   push_sliding_stats(I2, Stats, Avg, WrapT)
        ->  true
        ;   true
        )
    ;   WrapT = []
    ).

new_ring(Dim0/Avg, ring(0, Avg, Ring)) :-
    !,
    Dim is Dim0,
    compound_name_arity(Ring, [], Dim).
new_ring(Dim0, ring(0, Dim, Ring)) :-
    Dim is Dim0,
    compound_name_arity(Ring, [], Dim).

push_ring(Ring, Value, Wrap) :-
    Ring = ring(Here0, Avg, Data),
    Here is Here0+1,
    compound_name_arity(Data, _, Size),
    Arg is (Here0 mod Size)+1,
    (   Arg mod Avg =:= 0
    ->  Wrap = true
    ;   Wrap = false
    ),
    nb_setarg(Arg, Data, Value),
    nb_setarg(1, Ring, Here).

ring_values(Ring, Values) :-
    Ring = ring(Here, _, Data),
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

average_ring(ring(Here0,AvgI,Data), Avg) :-
    compound_name_arity(Data, _, Dim),
    Here is ((Here0-1) mod Dim)+1,
    Start0 is Here - AvgI + 1,
    (   Start0 < 1
    ->  Start is Start0+Dim
    ;   Start is Start0
    ),
    avg_window(Start, Here, Dim, Data, Dicts),
    average_dicts(Dicts, Avg).

avg_window(End, End, _, Data, [Dict]) :-
    !,
    arg(End, Data, Dict).
avg_window(Here, End, DIM, Data, [H|T]) :-
    arg(Here, Data, H),
    Here1 is Here+1,
    (   Here1 > DIM
    ->  Here2 is Here1-DIM
    ;   Here2 is Here1
    ),
    avg_window(Here2, End, DIM, Data, T).

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

%!  save_stats(+StaveSpec, +Stats) is det.
%
%   Save the statistics on each interval.

save_stats(save(File, Interval), Stats) :-
    arg(1, Stats, ring(Here, _, _)),
    Here mod Interval =:= 0,
    E = error(_,_),
    catch(save_stats_file(File, Stats),
          E, print_message(warning, E)),
    !.
save_stats(_, _).

save_stats_file(_, Stats) :-
    use_redis,
    !,
    (   redis_key(Server, Key)
    ->  redis(Server, set(Key, prolog(Stats)))
    ;   true
    ).
save_stats_file(File, Stats) :-
    setup_call_cleanup(
        open(File, write, Out),
        save_stats_stream(Stats, Out),
        close(Out)).

save_stats_stream(Stats, Out) :-
    get_time(Now),
    \+ \+ ( numbervars(Stats, 0, _, [singletons(true)]),
            format(Out, 'stats(~1f, ~q).~n', [Now, Stats])
          ).

restart_sliding_stats(save(_, _), Dims, Stats) :-
    use_redis,
    !,
    (   wait_redis_key(Server, Key),
        redis(Server, get(Key), Stats),
        new_sliding_stats(Dims, New),
        compatible_sliding_stats(Stats, New)
    ->  true
    ;   new_sliding_stats(Dims, Stats)
    ).
restart_sliding_stats(save(File, _), Dims, Stats) :-
    exists_file(File),
    E = error(_,_),
    catch(setup_call_cleanup(
              open(File, read, In),
              read(In, stats(_Saved, Stats)),
              close(In)),
          E, (print_message(warning, E), fail)),
    new_sliding_stats(Dims, New),
    compatible_sliding_stats(Stats, New),
    !.
restart_sliding_stats(_, Dims, Stats) :-
    new_sliding_stats(Dims, Stats).

compatible_sliding_stats(Stats1, Stats2) :-
    compound_name_arguments(Stats1, Name, List1),
    compound_name_arguments(Stats2, Name, List2),
    maplist(compatible_window, List1, List2).

compatible_window(ring(_,Avg,Data1), ring(_,Avg,Data2)) :-
    compound_name_arity(Data1, Name, Dim),
    compound_name_arity(Data2, Name, Dim).

%!  swish_save_stats(?File)
%
%   Save statistcs to File or the default file.

swish_save_stats(File) :-
    thread_self(Me),
    catch(thread_send_message(swish_stats, Me-save_stats(File)), E,
          stats_died(swish_stats, E)),
    thread_get_message(save_stats(Result)),
    (   Result = error(E)
    ->  throw(E)
    ;   File = Result
    ).


%!  swish_died_thread(TID, Status) is nondet.
%
%   True if Id is a thread that died   with Status and has not (yet)
%   been joined. Note that such threads may exist for a short while.

swish_died_thread(TID, Status) :-
    findall(TID-Stat, (thread_property(Thread, status(Stat)),
                       Stat \== running,
                       thread_property(Thread, id(TID))), Pairs),
    member(TID-Stat, Pairs),
    status_message(Stat, Status).

status_message(exception(Ex), Message) :-
    !,
    message_to_string(Ex, Message0),
    string_concat('ERROR: ', Message0, Message).
status_message(Status, Status).


                 /*******************************
                 *           SANDBOX            *
                 *******************************/

:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_debug:pengine_stale_module(_)).
sandbox:safe_primitive(swish_debug:pengine_stale_module(_,_)).
sandbox:safe_primitive(swish_debug:stale_pengine(_)).
sandbox:safe_primitive(swish_debug:swish_statistics(_)).
sandbox:safe_primitive(swish_debug:swish_stats(_, _)).
sandbox:safe_primitive(swish_debug:swish_died_thread(_, _)).
:- if(current_predicate(malloc_info:malloc_info/1)).
sandbox:safe_primitive(malloc_info:malloc_info(_)).
:- endif.
