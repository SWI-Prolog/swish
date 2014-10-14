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

:- module(swish_render_sudoku,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module('../render').

:- register_renderer(sudoku, "Render a sudoku matrix").

/** <module> SWISH Sudoku renderer

Renders a term like below as a _sudoku matrix_

  ==
  [[_,_,_,_,_,_,_,_,_],
   [_,_,_,_,_,3,_,8,5],
   [_,_,1,_,2,_,_,_,_],
   [_,_,_,5,_,7,_,_,_],
   [_,_,4,_,_,_,1,_,_],
   [_,9,_,_,_,_,_,_,_],
   [5,_,_,_,_,_,_,7,3],
   [_,_,2,_,1,_,_,_,_],
   [_,_,_,_,4,_,_,_,9]]
  ==
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders Term as a sudoku matrix. Term must be a list of 9 lists,
%	each of which must have  9  elements   that  are  all either the
%	integer 1..9 or unbound.

term_rendering(Term, _Vars, _Options) -->
	{ is_sudoku(Term)
	}, !,
	html(div([class(sudoku),
		  'data-render'('Sudoku matrix')
		 ],
		 [\rows(Term, 1), \sudoku_style])).

sudoku_style -->
	html({|html||
	      <style>
div.sudoku { vertical-align: top;
	     display:inline-block;
	     border: 3px solid black;
	     width: 220px;
	     height: 220px;
	     font-size: 0;
	   }
div.sudoku-row     { height: 11.11%; }
div.sudoku-row.fat { border-bottom: 2px solid black;}
div.sudoku-cell { width: 11.11%; height: 100%;
		  font-size: 12px;
		  font-weight: bold;
		  display: inline-block;
		  box-sizing: border-box;
		  border: 1px solid #888;
		  margin: 0px;
		  text-align: center;
		  vertical-align: middle;
		}
div.sudoku-cell.fat { border-right: 2px solid black;}
	      </style>
	     |}).

rows([], _) --> [].
rows([H|T], I) -->
	{ I2 is I+1,
	  (   (I == 3 ; I == 6)
	  ->  Extra = [fat]
	  ;   Extra = []
	  )
	},
	html(div(class(['sudoku-row'|Extra]), \cells(H, 1))),
	rows(T, I2).

cells([], _) --> [].
cells([H|T], I) -->
	{ I2 is I+1,
	  (   (I == 3 ; I == 6)
	  ->  Extra = [fat]
	  ;   Extra = []
	  )
	},
	html(div(class(['sudoku-cell'|Extra]), \value(H))), cells(T, I2).

value(H) --> { var(H) }, !.
value(H) --> term(H, []).


%%	is_sudoku(+Term) is semidet.
%
%	Type check for a term  to  be   a  representation  for  a Sudoku
%	puzzle.

is_sudoku(Term) :-
	is_list(Term),
	length(Term, 9),
	maplist(is_row, Term).

is_row(Row) :-
	is_list(Row),
	length(Row, 9),
	maplist(is_cell, Row).

is_cell(Var) :- var(Var).
is_cell(I)   :- integer(I), between(1, 9, I).
