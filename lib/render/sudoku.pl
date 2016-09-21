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
