/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam

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
	request(Id, []),
	(   replay(Id)
	->  true
	;   print_message(error, format('Failed for ID=~p', [Id]))
	),
	fail.
