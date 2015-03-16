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

:- module(swish_logging,
	  [
	  ]).
:- use_module(library(http/http_log)).
:- use_module(library(broadcast)).
:- use_module(library(settings)).
:- use_module(library(apply)).

/** <module> Add SWISH query execution to the HTTP log file

Add a line of the format below to  the HTTP log file. The src_text(Text)
option of Options is replaced by   `Hash-Text`  for the first occurrence
and just `Hash` for subsequent occurrences.

  ==
  pengine(Time, create(Pengine, Application, Options))
  ==
*/

:- setting(swish:logging, boolean, true,
	   "Enable/disable logging of SWISH query execution").

:- listen(pengine(Action), swish_log(Action)).

swish_log(create(Pengine, Application, Options0)) :-
	maplist(hash_option, Options0, Options),
	get_time(Now),
	format_time(string(HDate), '%+', Now),
	http_log('/*~s*/ pengine(~3f, ~q).~n',
		 [HDate, Now, create(Pengine, Application, Options)]).

:- dynamic
	text_hash/2.

hash_option(src_text(Text), src_text(Result)) :- !,
	(   text_hash(Text, Hash)
	->  Result = Hash
	;   variant_sha1(Text, Hash),
	    assert(text_hash(Text, Hash)),
	    Result = Hash-Text
	).
hash_option(Option, Option).
