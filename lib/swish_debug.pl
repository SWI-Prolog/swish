:- module(swish_debug,
	  [ pengine_stale_module/1	% -Module
	  ]).
:- use_module(library(pengines)).

%%	pengine_stale_module(-M) is nondet.
%
%	True if M seems to  be  a   pengine  module  with  no associated
%	pengine.

pengine_stale_module(M) :-
	current_module(M),
	is_uuid(M),
	\+ live_module(M).

live_module(M) :-
	pengine_property(Pengine, module(M)),
	pengine_property(Pengine, thread(Thread)),
	catch(thread_property(Thread, status(running)), _, fail).

is_uuid(M) :-
	atom(M),
	atom_length(M, 36),
	forall(sub_atom(M, S, 1, _, C),
	       uuid_code(S, C)).

uuid_sep(8).
uuid_sep(13).
uuid_sep(18).
uuid_sep(23).

uuid_code(S, -) :- !, uuid_sep(S).
uuid_code(_, X) :- char_type(X, xdigit(_)).

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_debug:pengine_stale_module(_)).
