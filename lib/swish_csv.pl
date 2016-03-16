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

pengines:write_result(csv, Event, VarNames) :-
	csv(Event, VarNames, []).

csv(create(_Id, Features), VarNames, Options) :- !,
	memberchk(answer(Answer), Features),
	csv(Answer, VarNames, Options).
csv(destroy(_Id, Wrapped), VarNames, Options) :- !,
	csv(Wrapped, VarNames, Options).
csv(success(_Id, Answers, _Time, More), VarNames, Options) :- !,
	VarTerm =.. [row|VarNames],
	success(Answers, VarTerm, [more(More)|Options]).
csv(error(_Id, Error), _VarNames, _Options) :- !,
	message_to_string(Error, Msg),
	format('Status: 400 Bad request~n'),
	format('Content-type: text/plain~n~n'),
	format('ERROR: ~w~n', [Msg]).
csv(output(_Id, message(_Term, _Class, HTML, _Where)), _VarNames, _Opts) :- !,
	format('Status: 400 Bad request~n'),
	format('Content-type: text/html~n~n'),
	format('<html>~n~s~n</html>~n', [HTML]).
csv(page(Page, Event), VarNames, _Options) :-
	csv(Event, VarNames, [page(Page)]).
csv(Event, _VarNames, _) :-
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
success(Answers, VarTerm, _Options) :-
	maplist(csv_answer, Answers, Rows),
	format('Content-encoding: chunked~n'),
	format('Content-disposition: attachment; filename="swish-result.csv"~n'),
	format('Content-type: text/csv~n~n'),
	csv_write_stream(current_output, [VarTerm], []),
	forall(paginate(100, Page, Rows),
	       csv_write_stream(current_output, Page, [])).

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


