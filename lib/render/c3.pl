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
:- use_module(library(error)).
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
	  valid_c3(C3),
	  gensym('c3js', Id),
	  atom_concat(#, Id, RefId),
	  put_dict(bindto, C3, RefId, C3b)
	},
	html(div([ class(['render-C3', 'reactive-size']),
		   'data-render'('As C3 chart')
		 ],
		 [ div(id(Id), []),
		   \js_script({|javascript(C3b)||
(function() {
  if ( $.ajaxScript ) {
    var div  = $.ajaxScript.parent();
    var data = C3b;
    var chart;
    var sizing = {};
    var tmo;

    div.on("reactive-resize", function() {
      if ( chart ) {
	if ( tmo ) clearTimeout(tmo);
	tmo = setTimeout(function() {
	  if ( updateSize() ) chart.resize(data.size);
	}, 1000);
      }
    });

    function updateSize() {
      data.size = data.size||{};
      var w0 = data.size.width;
      var h0 = data.size.height;

      if ( data.size.width == undefined || sizing.width ) {
	 var w = div.parents("div.answer").innerWidth();
	 data.size.width = Math.max(w*0.85, 100);
	 sizing.width = true;
	 if ( data.size.height == undefined || sizing.height ) {
	   data.size.height = data.size.width/2+50;
	   sizing.height = true;
	 }
      }

      return data.size.width != w0 || data.size.height != h0;
    }

    require(["d3", "c3"], function(d3, c3) {
      updateSize();
      chart = c3.generate(data);
    });
  }
})();
			      |})
		 ])).


%%	valid_c3(+C3) is det.
%
%	Perform sanity tests on the C3 representation.

valid_c3(C3) :-
	valid_c3_data(C3.data).

valid_c3_data(C3) :-
	valid_c3_array(C3.get(rows)), !.
valid_c3_data(C3) :-
	valid_c3_array(C3.get(columns)), !.
valid_c3_data(C3) :-
	throw(error(c3_no_data(C3), _)).

valid_c3_array(Array) :-
	must_be(list(list(ground)), Array).

:- multifile
	prolog:error_message//1.

prolog:error_message(c3_no_data(C3)) -->
	[ 'C3.data contains no rows nor columns: ~p'-[C3] ].
