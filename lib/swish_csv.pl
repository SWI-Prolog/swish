/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(swish_csv, []).
:- use_module(library(pengines), []).
:- use_module(library(pairs)).
:- use_module(library(csv), [csv_write_stream/3]).
:- use_module(library(apply)).
:- use_module(library(pprint)).
:- use_module(library(option)).

/** <module> Support CSV output from a Pengines server

This module defines the result-format  `csv`   for  Pengines.  It allows
SWISH users to post a query against the   core  Prolog system or a saved
SWISH program and obtain the results using   a simple web client such as
`curl`. An example shell script is provided in =client/swish-ask.sh=.

@tbd	Provide streaming output
*/

:- multifile
	pengines:write_result/3,
	write_answers/2,		% Answers, Bindings
	write_answers/3.		% Answers, Bindings, Options

%%	pengines:write_result(+Format, +Event, +VarNames) is semidet.
%
%	Hook into library(pengines) that  makes   pengines  support  CSV
%	output.

pengines:write_result(csv, Event, OptionDict) :-
	(   Disposition = OptionDict.get(disposition)
	->  Options = [disposition(Disposition)]
	;   Options = []
	),
	csv(Event, Options).

csv(create(_Id, Features), Options) :- !,
	memberchk(answer(Answer), Features),
	csv(Answer, Options).
csv(destroy(_Id, Wrapped), Options) :- !,
	csv(Wrapped, Options).
csv(success(_Id, Answers, Projection, _Time, More), Options) :- !,
	VarTerm =.. [row|Projection],
	success(Answers, VarTerm, [more(More)|Options]).
csv(error(_Id, Error), _Options) :- !,
	message_to_string(Error, Msg),
	format('Status: 400 Bad request~n'),
	format('Content-type: text/plain~n~n'),
	format('ERROR: ~w~n', [Msg]).
csv(output(_Id, message(_Term, _Class, HTML, _Where)), _Opts) :- !,
	format('Status: 400 Bad request~n'),
	format('Content-type: text/html~n~n'),
	format('<html>~n~s~n</html>~n', [HTML]).
csv(page(Page, Event), Options) :-
	csv(Event, [page(Page)|Options]).
csv(failure(_Id, _Time), Options) :- !,
	success([], -, [more(false)|Options]).
csv(Event, _) :-
	print_term(Event, [output(user_error)]).

success(Answers, VarTerm, Options) :-
	write_answers(Answers, VarTerm, Options), !.
success(Answers, VarTerm, Options) :-
	write_answers(Answers, VarTerm), !,
	assertion(\+option(page(_), Options)).
success(Answers, _VarTerm, Options) :-
	option(page(Page), Options),
	Page > 1, !,
	maplist(csv_answer, Answers, Rows),
	forall(paginate(100, OutPage, Rows),
	       csv_write_stream(current_output, OutPage, [])).
success(Answers, VarTerm, Options) :-
	option(disposition(Disposition), Options, 'swish-result.csv'),
	maplist(csv_answer, Answers, Rows),
	format('Content-encoding: chunked~n'),
	format('Content-disposition: attachment; filename="~w"~n', [Disposition]),
	format('Content-type: text/csv~n~n'),
	projection_row(VarTerm),
	forall(paginate(100, Page, Rows),
	       csv_write_stream(current_output, Page, [])).

projection_row(-) :- !.
projection_row(VarTerm) :-
	csv_write_stream(current_output, [VarTerm], []).

paginate(Len, Page, List) :-
	length(Page0, Len),
	(   append(Page0, Rest, List)
	->  (   Page = Page0
	    ;	paginate(Len, Page, Rest)
	    )
	;   Page = List
	).


csv_answer(JSON, Row) :-
	is_dict(JSON), !,
	dict_pairs(JSON, _, Pairs),
	pairs_values(Pairs, Values),
	maplist(csv_value, Values, CVSValues),
	Row =.. [row|CVSValues].
csv_answer(RowIn, Row) :-
	compound(RowIn), !,
	RowIn =.. [_|Values],
	maplist(csv_value, Values, CVSValues),
	Row =.. [row|CVSValues].

csv_value(Var, '') :-
	var(Var), !.
csv_value(Number, Number) :-
	number(Number), !.
csv_value(Atom, Atom) :-
	atom(Atom), !.
csv_value(String, String) :-
	string(String), !.
csv_value(Term, Value) :-
	term_string(Term, Value).


