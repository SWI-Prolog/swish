/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(swish_render_c3,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(gensym)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module('../render').

:- register_renderer(c3, "Render data as tables").

/** <module> SWISH C3.js based chart renderer

Render data as a chart.
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders Term as a C3.js chart. This renderer recognises C3, as a
%	dict with tag `c3`.

term_rendering(C3, _Vars, _Options) -->
	{ is_dict(C3, c3),
	  gensym('c3js', Id),
	  atom_concat(#, Id, RefId),
	  put_dict(bindto, C3, RefId, C3b)
	},
	html(div([ class('render-C3'),
		   'data-render'('As C3 chart')
		 ],
		 [ div(id(Id), []),
		   \js_script({|javascript(C3b)||
(function() {
  if ( $.ajaxScript ) {
    var w = $.ajaxScript.parents("div.answer").innerWidth();
    var data = C3b;

    data.size = data.size||{};
    if ( data.size.width == undefined ) {
       data.size.width = Math.max(w*0.8, 100);
       if ( data.size.height == undefined ) {
	 data.size.height = data.size.width/2+50;
       }
    }

    require(["d3", "c3"], function(d3, c3) {
      c3.generate(data);
    });
  }
})();
			      |})
		 ])).
