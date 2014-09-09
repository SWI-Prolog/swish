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

:- module(swish_app,
	  [
	  ]).
:- use_module(library(pengines)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_json)).

:- use_module(lib/page, []).
:- use_module(lib/storage).
:- use_module(lib/examples).
:- use_module(lib/help).


		 /*******************************
		 *	       PATHS		*
		 *******************************/

user:file_search_path(swish_web, swish(web)).

set_swish_path :-
	absolute_file_name(swish('swish.pl'), _,
			   [file_errors(fail), access(read)]), !.
set_swish_path :-
	prolog_load_context(directory, Dir),
	asserta(user:file_search_path(swish, Dir)).

:- set_swish_path.

http:location(swish, root(swish), []).


		 /*******************************
		 *	   HTTP HANDLERS	*
		 *******************************/

:- http_handler(swish(.), serve_files_in_directory(swish_web), [prefix]).
:- http_handler(swish('config.json'), http_server_config, []).
:- http_handler(/, http_redirect(moved_temporary, swish('index.html')), [priority(-100)]).


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
	dict_pairs(JSON, json, Pairs).


                 /*******************************
                 *   CREATE SWISH APPLICATION   *
                 *******************************/

:- multifile
	pengines:prepare_module/3.

:- pengine_application(swish).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).

% Libraries that are nice to have in SWISH, but cannot be loaded
% because they use directives that are considered unsafe.  We load
% them here, so they only need to be imported, which is just fine.

:- use_module(library(clpfd), []).
:- use_module(library(clpb), []).
