/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(swish_config, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- http_handler(swish('config.json'), http_server_config, []).

/** <module> Make HTTP locations known to JSON code
*/

		 /*******************************
		 *	       CONFIG		*
		 *******************************/

%%	http_server_config(+Request)
%
%	Emit a configuration object  to   the  client.  Currently serves
%	http.locations, which is an object that maps HTTP handler ids to
%	HTTP locations for each declared handler   that  has an explicit
%	id(ID) property.

http_server_config(_Request) :-
	http_locations(JSON),
	reply_json(json{ http: json{ locations:JSON
				   }
		       }).

http_locations(JSON) :-
	findall(ID-Path,
		( http_current_handler(Path, _:_, Options),
		  memberchk(id(ID), Options)
		), Pairs),
	keysort(Pairs, Sorted),
	remove_duplicate_ids(Sorted, Cleaned),
	dict_pairs(JSON, json, Cleaned).

remove_duplicate_ids([], []).
remove_duplicate_ids([Id-Path1,Id-Path2|T], [Id-Path1|Cleaned]) :- !,
	same_ids(T, Id, T1, Paths),
	print_message(warning, http(duplicate_handlers(Id, [Path1,Path2|Paths]))),
	remove_duplicate_ids(T1, Cleaned).
remove_duplicate_ids([H|T0], [H|T]) :-
	remove_duplicate_ids(T0, T).

same_ids([], _, [], []).
same_ids([Id-Path|T0], Id, T, [Path|TP]) :- !,
	same_ids(T0, Id, T, TP).
same_ids(T, _, T, []).

:- multifile
	prolog:message//1.

prolog:message(http(duplicate_handlers(Id, Paths))) -->
	[ 'Duplicate HTTP handler IDs: "~w"'-[Id] ],
	paths(Paths).

paths([]) --> [].
paths([H|T]) --> [ '\t~q'-[H], nl ], paths(T).
