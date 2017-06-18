/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple script to inline all images we   get  from the bower dependencies
into the css file.  Usage:

  - swipl css-inline.pl swish-min.css

Copyright: Public domain
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(main)).
:- use_module(library(debug)).

:- initialization(main, main).
%:- debug(css).

main(['--debug'|Argv]) :- !,
	debug(css),
	main(Argv).
main([In, Out]) :- !,
	process(In, Out).
main([In]) :-
	process(In, In).

process(In, Out) :-
	phrase_from_file(css(Data), In),
	setup_call_cleanup(
	    open(Out, write, O),
	    maplist(save(O), Data),
	    close(O)).

css([s(S), url(URL)|Rest]) -->
	string(Codes),
	"url(", !, string(URLCodes), ")", !,
	{ string_codes(S, Codes),
	  atom_codes(URL, URLCodes)
	},
	css(Rest).
css([s(S)]) -->
	string(Codes), \+ [_], !,
	{ string_codes(S, Codes) }.

save(Out, s(S)) :- !,
	format(Out, '~s', [S]).
save(Out, url(S)) :-
	\+ sub_string(S, 0, _, _, 'data:'),
	file_name_extension(_, Ext, S),
	image_ext(Ext),
	size_file(S, Size),
	(   sub_string(S, _, _, _, '/bower_components/')
	->  true
	;   Size < 2000
	), !,
	debug(css, 'Inlining image: ~q (~D bytes)', [S, Size]),
	read_file_to_codes(S, Codes, [type(binary)]),
	phrase(base64(Codes), Base64),
	format(Out, 'url(data:image/~w;base64,~s)', [Ext,Base64]).
save(Out, url(S)) :-
	sub_atom(S, 0, _, _, 'data:'), !,
	(   atom_length(S, Len),
	    Len < 50
	->  S1 = S
	;   sub_atom(S, 0, 50, _, S1)
	),
	debug(css, 'URL: already inlined: ~s ...', [S1]),
	format(Out, 'url(~s)', [S]).
save(Out, url(S)) :-
	debug(css, 'URL: ~q', [S]),
	format('~s~n', [S]),
	format(Out, 'url(~s)', [S]).

image_ext(gif).
image_ext(png).
