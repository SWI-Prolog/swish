/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
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

:- module(http_cron,
          [ http_schedule_maintenance/2         % +When, :Goal
          ]).
:- use_module(library(broadcast)).
:- use_module(library(error)).

:- meta_predicate
    http_schedule_maintenance(+, 0).

:- dynamic
    cron_schedule/2.                            % Schedule, Goal

/** <module> Schedule HTTP server maintenance tasks

This module deals with scheduling low frequency maintenance tasks to run
at specified time stamps. The jobs are   scheduled on the wall clock and
thus the interval is kept over server restarts.
*/

%!  http_schedule_maintenance(+When, :Goal) is det.
%
%   Schedule running Goal based on maintenance   broadcasts. When is one
%   of:
%
%     - daily(Hour:Min)
%     Run each day at Hour:Min.  Min is rounded to a multitude
%     of 5.
%     - weekly(Day, Hour:Min)
%     Run at the given Day and Time each week.  Day is either a
%     number 1..7 (1 is Monday) or a weekday name or abbreviation.
%     - monthly(DayOfTheMonth, Hour:Min)
%     Run each month at the given Day (1..31).  Note that not all
%     months have all days.
%     - clear
%     Clear the schedule for the given goal.
%
%   This  must  be  used   with   a    timer   that   broadcasts   a
%   maintenance(_,_) message (see broadcast/1). Such a timer is part
%   of library(http/http_unix_daemon).
%
%   @arg Goal is the goal called. This   is  executed in the thread that
%   broadcasts the maintenance(_,_)  event,  i.e.,   by  default  in the
%   `main` thread. If a considerable amount of work  is to be done it is
%   adviced to start a _detached_ thread to do the real work.

http_schedule_maintenance(When, Goal) :-
    listen(maintenance(_,_), http_consider_cronstart),
    (   compile_schedule(When, Schedule)
    ->  clear_schedule(Goal),
        (   Schedule == clear
        ->  true
        ;   asserta(cron_schedule(Schedule, Goal))
        )
    ;   domain_error(schedule, When)
    ).

clear_schedule(Goal) :-
    (   clause(cron_schedule(_, Goal0), true, Ref),
        Goal =@= Goal0,
        erase(Ref),
        fail
    ;   true
    ).

compile_schedule(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
compile_schedule(clear, clear).
compile_schedule(daily(Time0), daily(Time)) :-
    compile_time(Time0, Time).
compile_schedule(weekly(Day0, Time0), weekly(Day, Time)) :-
    compile_weekday(Day0, Day),
    compile_time(Time0, Time).
compile_schedule(monthly(Day, Time0), monthly(Day, Time)) :-
    must_be(between(0, 31), Day),
    compile_time(Time0, Time).

compile_time(HH:MM0, HH:MM) :-
    must_be(between(0, 23), HH),
    must_be(between(0, 59), MM0),
    MM is ((MM0+4)//5)*5.

compile_weekday(N, _) :-
    var(N),
    !,
    instantiation_error(N).
compile_weekday(N, N) :-
    integer(N),
    !,
    must_be(between(1,7), N).
compile_weekday(Day, N) :-
    downcase_atom(Day, Lwr),
    (   sub_atom(Lwr, 0, 3, _, Abbr),
        day(N, Abbr)
    ->  !
    ;   domain_error(day, Day)
    ).

%!  http_consider_cronstart
%
%   Run scheduled tasks.

http_consider_cronstart :-
    get_time(NowF),
    Now is round(NowF/60.0)*60,
    (   cron_schedule(Schedule, Goal),
        scheduled(Schedule, Now),
        catch(Goal, E, print_message(warning, E)),
        fail
    ;   true
    ).

scheduled(daily(HH:MM), Now) :-
    stamp_date_time(Now, DateTime, local),
    date_time_value(time, DateTime, time(HH,MM,_)).
scheduled(weekly(Day, Time), Now) :-
    stamp_date_time(Now, DateTime, local),
    date_time_value(date, DateTime, Date),
    day_of_the_week(Date, Day),
    scheduled(daily(Time), Now).
scheduled(monthly(Day, Time), Now) :-
    stamp_date_time(Now, DateTime, local),
    date_time_value(day, DateTime, Day),
    scheduled(daily(Time), Now).

day(1, mon).
day(2, tue).
day(3, wed).
day(4, thu).
day(5, fri).
day(6, sat).
day(7, sun).
