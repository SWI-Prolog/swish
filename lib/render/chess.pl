/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2016, VU University Amsterdam
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

:- module(swish_render_chess,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module('../render').

:- register_renderer(chess, "Render chess board representations").

/** <module> SWISH chessboard renderer

Render chessboards. Currently only deals with the N-queens problem. This
file is nevertheless called =chess.pl= because   it should be trivial to
extend this to more general chess positions.

The   styling   is    a    small     modification    of    [CSS3   Chess
Board](http://designindevelopment.com/css/css3-chess-board/)
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Render an N-queens  problem.  This   renderer  assumes  that the
%	solution is represented by a permutation   of a list of integers
%	1..N, where the I-th integer describes   the column of the queen
%	at row I.

term_rendering(Term, _Vars, _Options) -->
	{ is_nqueens(Term),
	  length(Term, N),
	  LineHeight is 200/N
	},
	html(div([ style('display:inline-block;'+
			 'line-height:'+LineHeight+'px;'+
			 'font-size:'+LineHeight+'px;'
			),
		   'data-render'('Chess board')
		 ],
		 [ table(class('chess-board'),
			 \nqueens(Term, N)),
		   \chess_style
		 ])).

is_nqueens(Term) :-
	is_list(Term),
	maplist(integer, Term),
	length(Term, N),
	numlist(1, N, All),
	sort(Term, All).

nqueens([], _) --> [].
nqueens([H|T], N) -->
	html(tr(\nrow(0, N, H))),
	nqueens(T, N).

nrow(N, N, _) --> !.
nrow(I, N, At) -->
	{ I2 is I+1 },
	(   { I2 == At }
	->  html(td(&('#9819')))
	;   html(td([]))
	),
	nrow(I2, N, At).

%%	chess_style//
%
%	@see http://designindevelopment.com/css/css3-chess-board/

chess_style -->
	html({|html||
<style>
.chess-board {
  border:2px solid #333; width:200px; height:200px;
}
.chess-board td {
  background:#fff;
  background:-moz-linear-gradient(top, #fff, #eee);
  background:-webkit-gradient(linear,0 0, 0 100%, from(#fff), to(#eee));
  box-shadow:inset 0 0 0 1px #fff;
  -moz-box-shadow:inset 0 0 0 1px #fff;
  -webkit-box-shadow:inset 0 0 0 1px #fff;
  text-align:center;
  vertical-align:middle;
}
.chess-board tr:nth-child(odd) td:nth-child(even),
.chess-board tr:nth-child(even) td:nth-child(odd) {
  background:#ccc;
  background:-moz-linear-gradient(top, #ccc, #eee);
  background:-webkit-gradient(linear,0 0, 0 100%, from(#ccc), to(#eee));
  box-shadow:inset 0 0 10px rgba(0,0,0,.4);
  -moz-box-shadow:inset 0 0 10px rgba(0,0,0,.4);
  -webkit-box-shadow:inset 0 0 10px rgba(0,0,0,.4);
}
</style>
	     |}).
