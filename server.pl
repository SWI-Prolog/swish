/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam

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

:- module(server,
	  [ server/0,
	    server/1				% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(broadcast)).
:- use_module(swish).

/** <module> Load the SWISH server for local usage

This module loads the SWISH server for default local usage, i.e., mostly
for developing SWISH. The file `daemon.pl` can be used as a start to run
it as a server process.

This file is normally used from  `run.pl`,   which  is started like this
from   the   shell   to   start   the   SWISH   server   accessible   on
http://localhost:3050/

    swipl run.pl

@see	run.pl and daemon.pl
*/

%%   server is det.
%%   server(?Port) is det.
%
%    Start the web-server on Port.  Port  may   be  unbound  to make the
%    system  select  a  free  port.  Port  can   also  be  of  the  form
%    `localhost:Port`  to  bind  the  server    only  to  the  localhost
%    interface.

server :-
	server(localhost:3050).
server(Port) :-
	broadcast(http(pre_server_start)),
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]),
	broadcast(http(post_server_start)).
