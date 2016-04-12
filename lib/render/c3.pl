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
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(gensym)).
:- use_module(library(error)).
:- use_module(library(dif)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- if(exists_source(library(dicts))).
:- use_module(library(dicts)).
:- endif.
:- use_module('../render').

:- register_renderer(c3, "Render data as tables").

/** <module> SWISH C3.js based chart renderer

Render data as a chart.
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders Term as a C3.js chart. This renderer recognises C3, as a
%	dict with tag `c3`.

% This renderer hooks into SWISH using two event-handlers and associated
% classes. The `reactive-size` is called  after   the  window or pane is
% resized and the `export-dom` is called from the _download_ button.

term_rendering(C30, _Vars, _Options) -->
	{ is_dict(C30, Tag),
	  Tag == c3,
	  valid_c3(C30, C3),
	  gensym('c3js', Id),
	  atom_concat(#, Id, RefId),
	  put_dict(bindto, C3, RefId, C3b)
	},
	html(div([ class(['render-C3', 'reactive-size', 'export-dom']),
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

    div.on("export-dom", function(ev, r) {
      var svg = div.find("svg");
      svg.attr("xmlns", "http://www.w3.org/2000/svg");
      svg.css("font", "10px sans-serif");
      svg.find(".c3-path,.c3-line,.tick,.domain").css("fill", "none");
      svg.find(".tick,.domain").css("stroke", "#000");
      r.element = svg[0];
      r.extension = "svg";
      r.contentType = "image/svg+xml";
    });

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


%%	valid_c3(+C3In, -C3Out) is det.
%
%	Perform sanity tests on the C3 representation.

valid_c3(C30, C31) :-
	Data0 = C30.data,
	valid_c3_data(Data0, Data),
	(   same_term(Data0, Data)
	->  C31 = C30
	;   C31 = C30.put(data,Data)
	).

valid_c3_data(Data0, Data) :-
	Rows0 = Data0.get(rows), !,
	must_be(acyclic, Rows0),
	rows_to_matrix(Rows0, Rows),
	must_be(list(ground), Rows),
	(   same_term(Rows0, Rows)
	->  Data0 = Data
	;   Data = Data0.put(rows,Rows)
	).
valid_c3_data(Data, Data) :-
	Columns = Data.get(columns), !,
	must_be(acyclic, Columns),
	must_be(list,Columns),
	maplist(must_be(list),Columns).

valid_c3_data(Data, Data) :-
	throw(error(c3_no_data(Data), _)).

%%	rows_to_matrix(+RowsIn, -Rows) is semidet.
%
%	Translate alternative row representations into  a list of lists.
%	Recognised input rows are:
%
%	  * Dicts having the same set of keys (if library(dicts) is
%	    available)
%	  * Compounds having same name and arity, e.g., pairs.
%	  * Lists having the same length

:- if(current_predicate(dicts_to_compounds/4)).
rows_to_matrix(Dicts, [Keys|Rows]) :-
	maplist(is_dict, Dicts), !,
	maplist(dict_keys, Dicts, KeysList),
	append(KeysList, Keys0),
	sort(Keys0, Keys),
	dicts_to_compounds(Dicts, Keys, dict_fill(undefined), Compounds),
	maplist(compound_arguments, Compounds, Rows).
:- endif.
rows_to_matrix(Compounds, Rows) :-
	dif(Name/Arity, []/2),		% avoid lists
	maplist(name_arity_compound(Name, Arity), Compounds, Rows), !.
rows_to_matrix(Lists, Lists) :-
	maplist(length_list(_Columns), Lists).

name_arity_compound(Name, Arity, Compound, Arguments) :-
	compound(Compound),
	compound_name_arity(Compound, Name, Arity),
	compound_name_arguments(Compound, Name, Arguments).

compound_arguments(Compound, Arguments) :-
	compound_name_arguments(Compound, _, Arguments).

length_list(Length, List) :-
	length(List, Length).

:- multifile
	prolog:error_message//1.

prolog:error_message(c3_no_data(C3)) -->
	[ 'C3.data contains no rows nor columns: ~p'-[C3] ].
