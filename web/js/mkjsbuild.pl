#!/usr/bin/env swipl

:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(main)).

:- initialization(main, main).

main([]) :- !,
	main(['swish.js', 'build.js.in']).
main([Main, Build]) :-
	config(Main, Config),
%	format('Got ~s~n', [Config]),
	phrase_from_file((string(Pre), "@CONFIG@", string(Post)), Build),
	file_name_extension(OutFile, in, Build),
	setup_call_cleanup(
	    open(OutFile, write, Out),
	    format(Out, '~s~s,~s', [Pre, Config, Post]),
	    close(Out)).

config(File, Config) :-
	phrase_from_file(config(Config), File).

config(Config) -->
	...,
	"require.config(", whites, "{",
	(   blanks, "urlArgs:", string_without(`\n`, _)
	->  []
	;   []
	),
	string(Config), blanks,
	"}", whites, ")", whites, ";", whites, "//", whites, "require.config", !,
	... .

... --> []|[_],... .
