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
