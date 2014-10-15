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

:- module(swish_render_codes,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module('../render').

:- register_renderer(codes, "Render a list of character codes").

/** <module> SWISH code-list renderer

Render lists of character codes as a `string`
*/

%%	term_rendering(+Codes, +Vars, +Options)//
%
%	Renders  a  list  of  character  codes   as  a  string.  Options
%	processed:
%
%	  - min_length(+Integer)
%	  Codes must be a list of at least Integer length. Default is
%	  `3`.
%	  - ellipsis(+Integer)
%	  Write list as `bla bla ... bla` if longer than Integer.
%	  Default is 30.
%	  - partial(+Boolean)
%	  It `true` (default), allow a partial list (ending in a
%	  variable).
%	  - charset(+Charset)
%	  Set of characters to accept.  Currently allows for
%	    - ascii
%	    Allow 32..126
%	    - iso_latin_1
%	    Allow 32..126 and 160..255

term_rendering(Codes, _Vars, Options) -->
	{ is_code_list(Codes, Len, Options)
	},
	(   { option(ellipsis(Ellipsis), Options, 30),
	      Len > Ellipsis
	    }
	->  { First is Ellipsis - 5,
	      Skip is Len - 5,
	      skip_first(Skip, Codes, Rest),
	      phrase(put_n_codes(First, Codes), PrefixCodes),
	      phrase(put_codes(Rest), PostfixCodes),
	      string_codes(Prefix, PrefixCodes),
	      string_codes(Postfix, PostfixCodes)
	    },
	    html(span([ 'data-render'('Truncated list of codes as a string'),
			class('render-code-list'),
			title('Code list of length: '+Len)
		      ],
		      [ '`~s'-[Prefix],
			span(class('render-ellipsis'), ...),
			'~s`'-[Postfix]
		      ]))
	;   { phrase(put_codes(Codes), TextCodes),
	      string_codes(String, TextCodes)
	    },
	    html(span([ 'data-render'('List of codes as a string'),
			class('render-code-list')
		      ],
		  '`~s`'-String))
	).

skip_first(N, [_|T0], T) :-
	succ(N2, N), !,
	skip_first(N2, T0, T).
skip_first(_, L, L).

put_n_codes(N, [H|T]) -->
	{ succ(N2, N) }, !,
	emit_code(H),
	put_n_codes(N2, T).
put_n_codes(_, _) --> [].

put_codes(Var) -->
	{ var_or_numbered(Var) }, !,
	dcg_format('|~p', [Var]).
put_codes([]) --> [].
put_codes([H|T]) -->
	emit_code(H),
	put_codes(T).

emit_code(0'\b) --> !, "\\b".
emit_code(0'\r) --> !, "\\r".
emit_code(0'\n) --> !, "\\n".
emit_code(0'\t) --> !, "\\t".
emit_code(C)	--> [C].

dcg_format(Fmt, Args, List, Tail) :-
	format(codes(List, Tail), Fmt, Args).

%%	is_code_list(+Codes, -Length, +Options) is semidet.

is_code_list(Codes, Length, Options) :-
	'$skip_list'(Length, Codes, Tail),
	code_list_tail(Tail, Options),
	option(min_length(MinLen), Options, 3),
	Length >= MinLen,
	option(charset(Charset), Options, ascii),
	all_codes(Codes, Charset).

code_list_tail([], _) :- !.
code_list_tail(Var, Options) :-
	var_or_numbered(Var),
	option(partial(true), Options, true).

var_or_numbered(Var) :-
	var(Var), !.
var_or_numbered('$VAR'(_)).

all_codes(Var, _) :-
	var_or_numbered(Var), !.
all_codes([], _).
all_codes([H|T], Charset) :-
	integer(H),
	is_code(H, Charset), !,
	all_codes(T, Charset).

is_code(9,  _).
is_code(10, _).
is_code(13, _).
is_code(C, Charset) :-
	charset_code(Charset, C).

charset_code(ascii,       C) :- between(32,126,C).
charset_code(iso_latin_1, C) :- between(32,126,C) ; between(160,255,C).
