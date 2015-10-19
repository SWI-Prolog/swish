:- module(swish_debug,
	  [ pengine_stale_module/1,	% -Module
	    swish_statistics/1		% -Statistics
	  ]).
:- use_module(library(pengines)).
:- use_module(library(broadcast)).
:- use_module(highlight).

%%	pengine_stale_module(-M) is nondet.
%
%	True if M seems to  be  a   pengine  module  with  no associated
%	pengine.

pengine_stale_module(M) :-
	current_module(M),
	is_uuid(M),
	\+ live_module(M),
	\+ current_highlight_state(M, _).

live_module(M) :-
	pengine_property(Pengine, module(M)),
	pengine_property(Pengine, thread(Thread)),
	catch(thread_property(Thread, status(running)), _, fail).

%%	swish_statistics(?State)
%
%	True if State is a statistics about SWISH

swish_statistics(highlight_states(Count)) :-
	aggregate_all(count, current_highlight_state(_,_), Count).
swish_statistics(pengines_created(Count)) :-
	(   flag(pengines_created, Old, Old)
	->  Count = Old
	;   Count = 0
	).

:- listen(pengine(Action), swish_update_stats(Action)).

swish_update_stats(create(_Pengine, _Application, _Options0)) :-
	flag(pengines_created, Old, Old+1).
swish_update_stats(send(_Pengine, _Event)).


%%	is_uuid(@UUID)
%
%	True if UUID looks like a UUID

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
sandbox:safe_primitive(swish_debug:swish_statistics(_)).
