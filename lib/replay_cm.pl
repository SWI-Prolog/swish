/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(replay_cm,
	  [ load_log/1,				% +File
	    replay/0,
	    request/2,				% ?Request, +Params
	    request/4,				% ?Request
	    show/1,				% +Request
	    json/2,				% +Request, -JSON
	    save_source/2,			% +Request, +File
	    replay/1				% +Request
	  ]).
:- use_module(library(debug)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

/** <module> Replay CodeMirror highlight requests from a session

  1. ?- load_log('httpd-1.log.gz').
  2. ?- request(Id, Time, Request, false).

You can use ?- replay. to replay all events on http://localhost:3050. To
avoid piling up states you need to set the state timeout on the *server*
low, e.g.

    set_setting(swish:editor_max_idle_time, 2).
*/

% This should not ne needed. Writing the log file should use a fixed set
% of operators.

:- op(900,  fx, <-).
:- op(900, xfx, <-).
:- op(400, yfx, $).
:- op(100, yf,  []).

:- dynamic
	request/4.				% Id, Time, Request, Completed

		 /*******************************
		 *	       READ		*
		 *******************************/

%%	load_log(+Log)
%
%	Load a log file.

load_log(Log) :-
	retractall(request(_,_,_,_)),
	absolute_file_name(Log, Path,
			   [ access(read),
			     extensions(['', log, gz])
			   ]),
	empty_assoc(Active),
	setup_call_cleanup(
	    open(Path, In),
	    read_log(In, Active, 1, Session),
	    close(In)),
	predicate_property(request(_,_,_,_), number_of_clauses(Count)),
	format('Loaded ~D requests from ~D sessions~n', [Count, Session]).

open(Path, In) :-
	open(Path, read, In0, [encoding(utf8)]),
	(   file_name_extension(_, gz, Path)
	->  zopen(In0, In, [])
	;   In = In0
	).


read_log(In, Active, Session0, Session) :-
	read_term(In, Term,
		  [ syntax_errors(dec10),
		    module(replay_cm)
		  ]),
	read_log(Term, Active, In, Session0, Session).

read_log(end_of_file, Active, _, Session, Session) :- !,
	not_completed(Active).
read_log(Term, Active, In, Session0, Session) :-
	assert_event(Term, Active, Active1, Session0, Session1),
	read_log(In, Active1, Session1, Session).

assert_event(request(Id, Time, Request),
	     Active0, Active, Session, Session) :-
	memberchk(path(Path), Request),
	sub_atom(Path, 0, _, _, '/cm/'), !,
	put_assoc(Id, Active0, request(Id, Time, Request), Active).
assert_event(completed(Id, CPU, Bytes, Code, Status),
	     Active0, Active, Session, Session) :-
	del_assoc(Id, Active0, request(Id, Time, Request), Active), !,
	atomic_list_concat([Session, '-', Id], ReqID),
	assertz(request(ReqID, Time, Request,
			completed{ cpu:CPU,
				   bytes:Bytes,
				   code:Code,
				   status:Status
				 })).
assert_event(server(started, _),
	     Active0, Active, Session0, Session1) :- !,
	not_completed(Active0),
	empty_assoc(Active),
	Session1 is Session0 + 1.
assert_event(_, Active, Active, Session, Session).

not_completed(Assoc) :-
	assoc_to_list(Assoc, Pairs),
	forall(member(_Id-request(Id, Time, Request), Pairs),
	       assertz(request(Id, Time, Request,
			       false))).


		 /*******************************
		 *	       PLAY		*
		 *******************************/

request(Id, Params) :-
	request(Id, _Time, Request, _Completed),
	subset(Params, Request).

data(Id, String) :-
	request(Id, _Time, Request, _Completed),
	memberchk(post_data(Encoded), Request),
	post_data_encoded(Bytes, Encoded),
	string_codes(Bytes, List),
	phrase(utf8_codes(Codes), List),
	string_codes(String, Codes).

json(Id, JSON) :-
	data(Id, String),
	setup_call_cleanup(
	    open_string(String, Stream),
	    json_read_dict(Stream, JSON),
	    close(Stream)).

show(Id) :-
	data(Id, String),
	write(String).

save_source(Id, File) :-
	json(Id, JSON),
	setup_call_cleanup(
	    open(File, write, Out),
	    write(Out, JSON.text),
	    close(Out)).

replay(Id) :-
	catch(replay_guarded(Id, _), E,
	      (	  print_message(error, E),
		  fail
	      )).

replay_guarded(Id, Reply) :-
	request(Id, _Time, Request, _Completed),
	memberchk(post_data(Encoded), Request), !,
	memberchk(content_type(ContentType), Request),
	memberchk(path(Path), Request),
	format(atom(URL), 'http://localhost:3050~w', [Path]),
	post_data_encoded(Bytes, Encoded),
	setup_call_cleanup(
	    http_open(URL, In,
		      [ post(bytes(ContentType, Bytes))
		      ]),
	    read_string(In, _, Reply),
	    close(In)).
replay_guarded(Id, Reply) :-
	request(Id, _Time, Request, _Completed),
	memberchk(method(get), Request),
	memberchk(request_uri(URI), Request),
	format(atom(URL), 'http://localhost:3050~w', [URI]),
	setup_call_cleanup(
	    http_open(URL, In, []),
	    read_string(In, _, Reply),
	    close(In)).

replay :-
	forall(request(Id, []),
	       (   replay(Id)
	       ->  true
	       ;   print_message(error, format('Failed for ID=~p', [Id]))
	       )).

