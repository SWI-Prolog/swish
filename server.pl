:- module(server,
	  [ server/0,
	    server/1				% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(swish).

%%   server is det.
%%   server(?Port) is det.
%
%    Start the web-server on Port.  Port  may   be  unbound  to make the
%    system  select  a  free  port.  Port  can   also  be  of  the  form
%    `localhost:Port`  to  bind  the  server    only  to  the  localhost
%    interface.

server :-
	server(3050).
server(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]).
