/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2024, SWI-Prolog Solutions b.v.
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

:- module(swish_config_watchdog,
          []).
:- use_module(swish(lib/cron)).
:- use_module(library(threadutil)).

/** <module> Watchdog module

This module deals with regular  maintenance   tasks  for 24x7 instances.
Infrequently Pengines escape their time limit.
*/

% When to consider a thread a runaway?  CPU time in seconds.
runaway_min_time(1800).

:- initialization
    http_schedule_maintenance(daily(02:50), kill_runaway_threads).

kill_runaway_threads :-
    runaway_min_time(MinTime),
    forall(runaway_thread(TID, MinTime, Time),
           kill_thread(TID, Time)).

runaway_thread(TID, MinTime, CPU) :-
    thread_property(TID, id(_)),
    catch(anon_thread_cpu(TID, CPU), error(_,_), fail),
    CPU > MinTime.

anon_thread_cpu(TID, CPU) :-
    \+ thread_property(TID, alias(_)),
    thread_statistics(TID, cputime, CPU).

kill_thread(TID, Time) :-
    print_message(warning, kill_runaway(TID, Time)),
    catch(tbacktrace(TID), error(_,_), true),
    catch(thread_signal(TID, abort),
          error(_,_), true).

:- multifile prolog:message//1.

prolog:message(kill_runaway(TID, Time)) -->
    [ 'Killing runaway thread ~p: used ~0f seconds CPU'-[TID,Time] ].
