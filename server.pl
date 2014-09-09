:- module(server,
	  [ server/0,
	    server/1				% ?Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(swish).

%%    server is det.
%%    server(?Port) is det.
%
%    Start the web-server on Port.

server :-
	server(3050).
server(Port) :-
	http_server(http_dispatch,
		    [ port(Port),
		      workers(16)
		    ]).
