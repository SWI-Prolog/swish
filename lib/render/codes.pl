/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, VU University Amsterdam
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
