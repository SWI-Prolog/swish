/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2023, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(swish_debug,
          [ swish_debug/3,              % +Topic, +Format, :Args
            debug/1,                    % +Topic
            nodebug/1,                  % +Topic
            swish_debugging/1,          % ?Topic
            list_debug_topics/0,
            list_debug_topics/1,        % +Options

            swish_debug_sentinel/0
          ]).
:- autoload(library(option), [option/3, option/2]).
:- autoload(library(pengines), [pengine_self/1, pengine_output/1]).
:- autoload(library(dcg/high_order), [sequence/5]).
:- autoload(library(pengines_io), [pengine_format/2]).
:- autoload(library(http/html_write), [html/3, print_html/1]).


:- meta_predicate
    debug(+,+,:).

:- thread_local
    debugging/2.                    % Topic, Enabled
:- dynamic
    shared_debug_topic/1.

/** <module> Print debug messages and test assertions

This library is a SWISH  specific   replacement  for library(debug) that
allows for debug/3 messages in SWISH code   as  well as libraries loaded
into SWISH. Unlike library(debug),  this   library  maintains  the debug
state per thread (Pengine).

This library allows for debug/3  calls   inside  user programs stored in
SWISH as well as libraries preloaded into  SWISH. Libraries that wish to
enable debugging by SWISH  users  must   include  this  file rather than
library(debug).  This  may  be  done  dynamically  using  the  following
snippet.

```
:- if(exists_source(swish(lib/swish_debug))).
:- use_module(swish(lib/swish_debug)).
:- else.
:- use_module(library(debug)).
:- endif.
```
*/

%!  swish_debugging(+Topic) is semidet.
%!  swish_debugging(-Topic) is nondet.
%
%   Examine debug topics. The form debugging(+Topic)  may be used to
%   perform more complex debugging tasks.   A typical usage skeleton
%   is:
%
%     ```
%           (   debugging(mytopic)
%           ->  <perform debugging actions>
%           ;   true
%           ),
%           ...
%     ```
%
%   The other two calls are intended to examine existing and enabled
%   debugging tokens and are typically not used in user programs.

swish_debugging(Topic) :-
    debugging(Topic, true).

%!  debug(+Topic) is det.
%!  nodebug(+Topic) is det.
%
%   Add/remove a topic  from  being   printed.  nodebug(_)  removes  all
%   topics. Gives a warning if the  topic   is  not defined unless it is
%   used from a directive. The latter allows placing debug topics at the
%   start of a (load-)file without warnings.
%
%   For debug/1, Topic can be  a  term   `Topic  >  Out`, where `Out` is
%   either a stream or  stream-alias  or   a  filename  (an  atom). This
%   redirects debug information on this topic   to  the given output. On
%   Linux systems redirection can be used   to  make the message appear,
%   even if the `user_error` stream is redefined using
%
%       ?- debug(Topic > '/proc/self/fd/2').
%
%   A platform independent way to  get   debug  messages  in the current
%   console (for example, a `swipl-win` window,  or login using `ssh` to
%   Prolog running an SSH server from the `libssh` pack) is to use:
%
%       ?- stream_property(S, alias(user_error)),
%          debug(Topic > S).
%
%   Do not forget to  disable  the   debugging  using  nodebug/1  before
%   quitting the console if Prolog must remain running.

debug(Topic) :-
    debug(Topic, true).
nodebug(Topic) :-
    debug(Topic, false).

debug(Topic, Enabled) :-
    (   retract(debugging(Topic, _))
    ->  true
    ;   shared_debug_topic(Topic)
    ->  true
    ;   print_message(warning, debug_no_topic(Topic))
    ),
    assert(debugging(Topic, Enabled)).

%!  debug_topic(+Topic) is det.
%
%   Declare a topic for debugging.  This can be used to find all
%   topics available for debugging.

debug_topic(Topic) :-
    pengine_self(_),
    !,
    (   debugging(Registered, _),
        Registered =@= Topic
    ->  true
    ;   assertz(debugging(Topic, false))
    ).
debug_topic(Topic) :-
    (   shared_debug_topic(Registered),
        Registered =@= Topic
    ->  true
    ;   assertz(shared_debug_topic(Topic))
    ).

%!  list_debug_topics is det.
%!  list_debug_topics(+Options) is det.
%
%   List currently known topics for debug/3   and their setting. Options
%   is  either  an  atom  or   string,    which   is   a  shorthand  for
%   `[search(String)]` or a normal option list. Defined options are:
%
%     - search(String)
%       Only show topics that match String.  Match is case insensitive
%       on the printed representation of the term.
%     - active(+Boolean)
%       Only print topics that are active (`true`) or inactive
%       (`false`).
%     - output(+To)
%       Only print topics whose target location matches To.  This option
%       implicitly restricts the output to active topics.

list_debug_topics :-
    list_debug_topics([]).

list_debug_topics(Options) :-
    (   atom(Options)
    ;   string(Options)
    ),
    !,
    list_debug_topics([search(Options)]).
list_debug_topics(Options) :-
    option(active(Activated), Options, _),
    findall(debug_topic(Topic, String, Activated),
            matching_topic(Topic, String, Activated, Options),
            Triples),
    print_message(information, debug_topics(Triples)).

matching_topic(Topic, String, Activated, Options) :-
    known_debug_topic(Topic, Activated),
    topic_to_string(Topic, String),
    (   option(search(Search), Options)
    ->  sub_atom_icasechk(String, _, Search)
    ;   true
    ).

topic_to_string(Topic, String) :-
    numbervars(Topic, 0, _, [singletons(true)]),
    term_string(Topic, String, [quoted(true), numbervars(true)]).

known_debug_topic(Topic, Value) :-
    debugging(Topic, Value).
known_debug_topic(Topic, false) :-
    shared_debug_topic(Topic),
    \+ debugging(Topic, _).


:- multifile
    prolog_debug_tools:debugging_hook/0.

prolog_debug_tools:debugging_hook :-
    (   debugging(_, true)
    ->  list_debug_topics([active(true)])
    ).

%!  swish_debug(+Topic, +Format, :Args) is det.
%
%   Format a message if debug topic  is enabled. Similar to format/3
%   to =user_error=, but only prints if   Topic is activated through
%   debug/1. Args is a  meta-argument  to   deal  with  goal for the
%   @-command.   Output   is   first    handed     to    the    hook
%   prolog:debug_print_hook/3.  If  this  fails,    Format+Args   is
%   translated  to  text   using    the   message-translation   (see
%   print_message/2) for the  term  debug(Format,   Args)  and  then
%   printed to every matching destination   (controlled  by debug/1)
%   using print_message_lines/3.
%
%   The message is preceded by '% ' and terminated with a newline.
%
%   @see    format/3.

swish_debug(Topic, Format, Args) :-
    debugging(Topic, true),
    !,
    print_debug(Topic, Format, Args).
swish_debug(_, _, _).


print_debug(_Topic, _Format, _Args) :-
    nb_current(swish_debug_printing, true),
    !.
print_debug(Topic, Format, Args) :-
    setup_call_cleanup(
        nb_setval(swish_debug_printing, true),
        print_debug_guarded(Topic, Format, Args),
        nb_delete(swish_debug_printing)).

:- html_meta(send_html(html)).

print_debug_guarded(Topic, Format, Args) :-
    topic_to_string(Topic, String),
    format(string(Msg0), Format, Args),
    split_string(Msg0, "", "\n", [Msg]),
    send_html(\debug_msg(Msg, String)).

send_html(HTML) :-
    phrase(html(HTML), Tokens),
    with_output_to(string(HTMlString), print_html(Tokens)),
    pengine_output(HTMlString).

debug_msg(Msg, Topic) -->
    html(div(class('debug-msg'),
             [ span([class('topic'), title('Debug topic')], Topic)
             | pre([class('msg'), title('Debug message')], Msg)
             ])).


		 /*******************************
		 *           EXPANSION		*
		 *******************************/

swish_debug_sentinel.

imports_this_module :-
    prolog_load_context(module, M),
    predicate_property(M:swish_debug_sentinel, imported_from(swish_debug)),
    !.

:- multifile
    user:goal_expansion/2.

user:goal_expansion(debug(Topic, Format, Args),
                    swish_debug(Topic, Format, Args)) :-
    imports_this_module,
    debug_topic(Topic).
user:goal_expansion(debugging(Topic),
                    swish_debugging(Topic)) :-
    imports_this_module,
    debug_topic(Topic).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(debug_topics(List)) -->
    [ ansi(bold, '~w~t ~w~35|~n', ['Debug Topic', 'Activated']),
      '~`\u2015t~35|'-[], nl
    ],
    sequence(debug_topic, [nl], List).

debug_topic(debug_topic(_Topic, TopicString, true)) -->
    [ ansi(bold, '~s~t \u2714~35|', [TopicString]) ].
debug_topic(debug_topic(_Topic, TopicString, false)) -->
    [ '~s~t -~35|'-[TopicString] ].

:- multifile
    sandbox:safe_global_variable/1,
    sandbox:safe_meta/2.

sandbox:safe_global_variable(swish_debug_printing).
sandbox:safe_meta(swish_debug:print_debug(_Topic, Format, Args), Calls) :-
    sandbox:format_calls(Format, Args, Calls).
