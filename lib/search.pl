/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2018, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_search,
	  [ search_box//1,		% +Options
	    match/3			% +Line, +Query, +Options
	  ]).
:- use_module(library(lists)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(prolog_source)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(solution_sequences)).

:- use_module(config).

:- multifile
	typeahead/4.			% +Set, +Query, -Match, +Options

/** <module> SWISH search from the navigation bar

This library supports both typeahead of the   search  box and the actual
search from the server side. What do we want to search for?

  - Predicates (built-in, library)
    - How should we handle documentation?  PlDoc?  Manual?
  - Source files (name, tags, meta-data, content?)
    - Show matching sources in modal dialog and allow switching to
      these?
*/

:- http_handler(swish(typeahead), typeahead, [id(swish_typeahead)]).

%%	search_box(+Options)//
%
%	Render a Bootstrap search box.

search_box(_Options) -->
	html(form([class('navbar-form'), role(search)],
		  div(class('input-group'),
		      [ input([ type(text),
				class(['form-control', typeahead]),
				placeholder('Search'),
				'data-search-in'([source,files,predicates]),
				title('Searches code, documentation and files'),
				id('search')
			      ]),
			div(class('input-group-btn'),
			    button([ class([btn, 'btn-default']),
				     type(submit)],
				   i(class([glyphicon, 'glyphicon-search']),
				     [])))
		      ]))).


%%	typeahead(+Request)
%
%	Support the search typeahead widget. The  handler returns a JSON
%	array of matches. Each match is an object that contains at least
%	a label.

typeahead(Request) :-
	http_parameters(Request,
			[ q(Query,     [default('')]),
			  set(Set,     [default(predicates)]),
			  match(Match, [default(sow)])
			]),
	findall(Result, typeahead(Set, Query, Result, _{match:Match}), Results),
	reply_json_dict(Results).

%%	typeahead(+Type, +Query, -Match, +Options:dict) is nondet.
%
%	Find  typeahead  suggestions  for  a  specific  search  category
%	(Type). This oredicate is a   multifile  predicate, which allows
%	for  adding  new  search  targets.  The  default  implementation
%	offers:
%
%	  - predicates
%	  Searches for built-in and configured library predicates
%	  - sources
%	  Searches all loaded source files.
%
%	@tbd: Limit number of hits?

:- multifile
	swish_config:source_alias/2.

typeahead(predicates, Query, Template, _) :-
	swish_config(templates, Templates),
	member(Template, Templates),
	_{name:Name, arity:_} :< Template,
	sub_atom(Name, 0, _, _, Query).
typeahead(sources, Query, Hit, Options) :-
	source_file(Path),
	(   file_alias_path(Alias, Dir),
	    once(swish_config:source_alias(Alias, _)),
	    atom_concat(Dir, File, Path)
	->  true
	),
	file_name_extension(Base, Ext, File),
	(   sub_atom(File, 0, _, _, Query)
	->  Hit = hit{alias:Alias, file:Base, ext:Ext, query:Query}
	;   Hit = hit{alias:Alias, file:Base, ext:Ext,
		      query:Query, line:LineNo, text:Line},
	    limit(5, search_file(Path, Query, LineNo, Line, Options))
	).
typeahead(sources, Query, hit{alias:Alias, file:Base, ext:Ext,
			      query:Query, line:LineNo, text:Line}, Options) :-
	swish_config:source_alias(Alias, AliasOptions),
	option(search(Pattern), AliasOptions),
	DirSpec =.. [Alias,.],
	absolute_file_name(DirSpec, Dir,
			   [ access(read),
			     file_type(directory),
			     solutions(all),
			     file_errors(fail)
			   ]),
	directory_file_path(Dir, Pattern, FilePattern),
	expand_file_name(FilePattern, Files),
	atom_concat(Dir, /, DirSlash),
	member(Path, Files),
	\+ source_file(Path),		% already did this one above
	atom_concat(DirSlash, File, Path),
	file_name_extension(Base, Ext, File),
	limit(5, search_file(Path, Query, LineNo, Line, Options)).

search_file(Path, Query, LineNo, Line, Options) :-
	debug(swish(search), 'Searching ~q for ~q (~q)', [Path, Query, Options]),
	setup_call_cleanup(
	    open(Path, read, In),
	    read_string(In, _, String),
	    close(In)),
	split_string(String, "\n", "\r", Lines),
	nth1(LineNo, Lines, Line),
	match(Line, Query, Options).

%%	match(+Line:string, +Query:string, +Options:dict) is semidet.
%
%	True if Line matches Query, respecting Options.

match(Text, Query, Options) :-
	sub_string(Text, Start, _, _, Query),
	(   Options.get(match) == sow
	->  sow(Text, Start), !
	;   Options.get(match) == sol
	->  !, Start == 0
	;   !
	).

sow(_, 0) :- !.
sow(Text, Offset) :-
	Pre is Offset-1,
	sub_atom(Text, Pre, 1, _, Before),
	sub_atom(Text, Offset, 1, _, Start),
	(   \+ char_type(Before, csym),
	    char_type(Start, csym)
	;   Before == '_',
	    char_type(Start, csym)
	;   char_type(Start, upper),
	    char_type(Before, lower)
	), !.
