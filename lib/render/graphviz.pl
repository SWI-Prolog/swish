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

:- module(swish_render_graphviz,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(process)).
:- use_module(library(sgml)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).
:- use_module('../render').

:- register_renderer(graphviz, "Render data using graphviz").

/** <module> Render data using graphviz

This renderer exploits  [graphviz](http://www.graphviz.org)   to  render
graphs from Prolog data. It takes two   representations.  The first is a
straightforward term Program(String), e.g.,

  ```
  dot("digraph G {Hello->World}")
  ```

The   second   takes   a   Prolog    term     as    input.    The   [dot
language](http://www.graphviz.org/content/dot-language)  is  represented
as follows:

  ```
  Graph      := graph(Statements)
              | graph(Options, Statements)
	      | digraph(Statements)
	      | digraph(Options, Statements)
  Options    := ID | [ID] | [strict, ID]
  Statements := List of statements
  Statement  := NodeStm | EdgeStm | AttrStm | Name = Value | SubGraph
  NodeStm    := NodeID | node(NodeID, AttrList)
  NodeID     := ID | ID:Port | ID:Port:CompassPT
  CompassPT  := n | ne | e | se | s | sw | w | nw | c | _
  EdgeStm    := (NodeID|SubGraph) (EdgeOp (NodeID|SubGraph))+
  EdgeStm     | edge(NodeID|SubGraph) (EdgeOp (NodeID|SubGraph))+), AttrList)
  EdgeOp     := - | ->
  AttrStm    := graph(AttrList)
	      | node(AttrList)
	      | edge(AttrList)
  AttrList   := List of attributes
  Attribute  := Name = Value
	      | Name(Value)
  SubGraph   := subgraph(ID, Statements)
  ```
*/

:- http_handler(swish(graphviz), swish_send_graphviz, []).

:- dynamic
	dot_data/3.				% +Hash, +Data, +Time

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders data using graphviz.  Options:
%
%	  - svg(+Mode)
%	  One of `inline` (default) or `object`, rendering the SVG using
%	  an HTML <object> element.

term_rendering(Data, Vars, Options) -->
	{ debug(graphviz(vars), 'Data: ~q, vars: ~p', [Data, Vars]),
	  data_to_graphviz_string(Data, DOTString, Program)
	},
	render_dot(DOTString, Program, Options).

%%	render_dot(+DotString, +Program, +Options)// is det.
%
%	Render a dot program. First checks whether Program is available.
%	It has two modes, producing  inline   SVG  or  producing an HTML
%	<object> element, which calls the server again to fetch the SVG.

render_dot(_DOTString, Program, _Options) -->
	{ \+ has_graphviz_renderer(Program) }, !,
	no_graph_viz(Program).
render_dot(DOTString, Program, Options) -->	% <object> rendering
	{ option(svg(object), Options, inline), !,
          variant_sha1(DOTString, Hash),
	  get_time(Now),
	  assert(dot_data(Hash,
			  _{ program: Program,
			     dot: DOTString
			   }, Now)),
	  remove_old_data(Now),
	  http_link_to_id(swish_send_graphviz,
			  [ hash(Hash),
			    lang(svg),
			    target('_top')
			  ], HREF),
	  Attrs = []				% TBD
	}, !,
	html([ object([ data(HREF),
			type('image/svg+xml')
		      | Attrs
		      ],
		      [])
	     ]).
render_dot(DOTString, Program, _Options) -->	% <svg> rendering
	{ graphviz_stream(_{program:Program, dot:DOTString},
			  PID, XDotOut, ErrorOut),
	  call_cleanup((   read_string(XDotOut, _, SVG),
			   read_string(ErrorOut, _, Error)
		       ),
		       (   process_wait(PID, _Status),
			   close(XDotOut)
		       ))
	},
	(   { Error == "" }
	->  html(\[SVG])
	;   html(div(style('color:red;'),
		     [ '~w'-[Program], ': ', Error]))
	).

%%	data_to_graphviz_string(+Data, -DOTString, -Program) is semidet.
%
%	Extract the DOT data and graphviz program to run on the data.

data_to_graphviz_string(Compound, String, Program) :-
	compound(Compound),
	compound_name_arguments(Compound, Program, [Data]),
	graphviz_program(Program),
	(   atomic(Data)
	->  String = Data
	;   phrase(graph(Data), Codes),
	    string_codes(String, Codes),
	    debug(graphviz, '~s', [String])
	).
data_to_graphviz_string(Compound, String, dot) :-
	compound(Compound),
	compound_name_arity(Compound, Type, Arity),
	graph_type(Type),
	between(1,2,Arity), !,
	phrase(graph(Compound), Codes),
	string_codes(String, Codes),
	debug(graphviz, '~s', [String]).


graphviz_program(dot).
graphviz_program(neato).
graphviz_program(fdp).
graphviz_program(sfdp).
graphviz_program(twopi).
graphviz_program(circo).

graph_type(graph).
graph_type(digraph).

%%	swish_send_graphviz(+Request)
%
%	HTTP handler to send a GraphViz graph

swish_send_graphviz(Request) :-
	http_parameters(Request,
			[ hash(Hash,
			       [ description('Hash-key to the graph-data')
			       ])
			]),
	dot_data(Hash, Data, _),
	graphviz_stream(Data, PID, XDotOut, ErrorOut),
	call_cleanup(( load_structure(stream(XDotOut),
				      SVGDom0,
				      [ dialect(xml) ]),
		       read_string(ErrorOut, _, Error)
		     ),
		     (	 process_wait(PID, _Status),
			 close(XDotOut)
		     )),
	(   Error == ""
	->  true
	;   print_message(error, format('~w', [Error]))
	),
	rewrite_sgv_dom(SVGDom0, SVGDom),
	format('Content-type: ~w~n~n', ['image/svg+xml; charset=UTF-8']),
	xml_write(current_output, SVGDom,
		  [ layout(false)
		  ]).

graphviz_stream(Data, PID, XDotOut, Error) :-
	process_create(path(Data.program), ['-Tsvg'],
		       [ stdin(pipe(ToDOT)),
			 stdout(pipe(XDotOut)),
			 stderr(pipe(Error)),
			 process(PID)
		       ]),
	set_stream(ToDOT, encoding(utf8)),
	set_stream(XDotOut, encoding(utf8)),
	thread_create(send_to_dot(Data.dot, ToDOT), _,
		      [ detached(true) ]).


rewrite_sgv_dom([element(svg, Attrs, Content)],
		[element(svg, Attrs,
			 [ element(script, ['xlink:href'=SVGPan], []),
			   element(g, [ id=viewport
				      ],
				   Content)
			 ])]) :-
	http_absolute_location(js('SVGPan.js'), SVGPan, []).
rewrite_sgv_dom(DOM, DOM).

send_to_dot(Data, Out) :-
	call_cleanup(format(Out, '~s', [Data]),
		     close(Out)), !.

%%	remove_old_data(+Now)
%
%	Remove data that are older than 15 minutes.

remove_old_data(Time) :-
	(   dot_data(Hash, _, Stamp),
	    Time > Stamp+900,
	    retract(dot_data(Hash, _, Stamp)),
	    fail
	;   true
	).

has_graphviz_renderer(Renderer) :-
	exe_options(ExeOptions),
	absolute_file_name(path(Renderer), _,
			   [ file_errors(fail)
			   | ExeOptions
			   ]).

exe_options(Options) :-
	current_prolog_flag(windows, true), !,
	Options = [ extensions(['',exe,com]), access(read) ].
exe_options(Options) :-
	Options = [ access(execute) ].

no_graph_viz(Renderer) -->
	html(div([ class('no-graph-viz'),
		   style('color:red;')
		 ],
		 [ 'The server does not have the graphviz program ',
		   code(Renderer), ' installed in PATH. ',
		   'See ', a(href('http://www.graphviz.org/'),
			     'http://www.graphviz.org/'), ' for details.'
		 ])).


%%	add_defaults(Statements0, Statements) is det.

add_defaults(Statements0, Statements) :-
	\+ memberchk(bgcolor=_, Statements0), !,
	Statements = [bgcolor=transparent|Statements0].
add_defaults(Statements, Statements).


		 /*******************************
		 *   GENERATING A DOT PROGRAM	*
		 *******************************/

graph(graph(Statements)) -->
	graph(graph([], Statements)).
graph(digraph(Statements)) -->
	graph(digraph([], Statements)).
graph(graph(Options, Statements)) -->
	{graph_options(Options, graph, Ctx)},
	graph(Statements, Ctx).
graph(digraph(Options, Statements)) -->
	{graph_options(Options, digraph, Ctx)},
	graph(Statements, Ctx).

graph_options([], Type,
	      gv{type:Type, indent:2}).
graph_options([strict], Type,
	      gv{strict:true, type:Type, indent:2}).
graph_options([strict, ID], Type,
	      gv{strict:true, id:ID, type:Type, indent:2}).

graph(Statements, Options) -->
	{ add_defaults(Statements, Statements1) },
	strict(Options), keyword(Options.type), ws, graph_id(Options),
	"{", nl,
	statements(Statements1, Options),
	"}", nl.

strict(Options) -->
	{ true == Options.get(strict) }, !,
	keyword(strict).
strict(_Options) --> [].

graph_id(Options) -->
	{ ID = Options.get(id) }, !,
	id(ID), ws.
graph_id(_) --> [].

statements([], _) --> [].
statements([H|T], Options) -->
	indent(Options),
	(   statement(H, Options)
	->  ";", nl
	;   {domain_error(graphviz_statement, H)}
	),
	statements(T, Options).

statement(graph(Attrs), O) --> keyword(graph), ws, attributes(Attrs, O).
statement(edge(Attrs), O) --> keyword(edge), ws, attributes(Attrs, O).
statement(node(Attrs), O) --> keyword(node), ws, attributes(Attrs, O).
statement(node(ID, Attrs), O) --> node(ID, O), ws, attributes(Attrs, O).
statement(edge(Edge, Attrs), O) --> edge(Edge, O), ws, attributes(Attrs, O).
statement(A - B, O) --> edge(A - B, O).
statement(A -> B, O) --> edge(A -> B, O).
statement(Name = Value, O) --> attribute(Name=Value, O).
statement(subgraph(Statements), O) -->
	{ step_indent(O, O1) },
	keyword(subgraph), ws, "{", nl,
	statements(Statements, O1), indent(O), "}".
statement(subgraph(ID, Statements), O) -->
	{ step_indent(O, O1) },
	keyword(subgraph), ws, id(ID), ws, "{", nl,
	statements(Statements, O1), indent(O), "}".

step_indent(O, O2) :-
	I is O.indent+2,
	O2 = O.put(indent, I).

edge((A-B)-C, O)   --> !, edge(A-B, O), edgeop(O), id(C).
edge(A-(B-C), O)   --> !, node(A, O), edgeop(O), edge(B-C, O).
edge(A-B, O)       --> node(A, O), edgeop(O), node(B, O).
edge((A->B)->C, O) --> !, edge(A->B, O), edgeop(O), node(C, O).
edge(A->(B->C), O) --> !, node(A, O), edgeop(O), edge(B->C, O).
edge(A->B, O)      --> node(A, O), edgeop(O), node(B, O).

edgeop(O) --> { graph == O.type }, !, " -- ".
edgeop(_) --> " -> ".

node(ID:Port:Compass, _O) --> !,
	id(ID), ":", id(Port), ":", compass(Compass).
node(ID:Port, _O) --> !,
	id(ID), ":", id(Port).
node(ID, _O) --> !,
	id(ID).

compass(Compass) -->
	{ compass(Compass) },
	atom(Compass).
compass(Compass) -->
	{ domain_error(compass, Compass) }.

compass('_') :- !.	% handles variables
compass(n).
compass(ne).
compass(e).
compass(se).
compass(s).
compass(sw).
compass(w).
compass(nw).
compass(c).

attributes([], _) --> !.
attributes(List, O) --> "[", attribute_list(List, O), "]".

attribute_list([], _) --> [].
attribute_list([H|T], O) -->
	attribute(H, O),
	(   {T == []}
	->  []
	;   ",", attribute_list(T, O)
	).

attribute(Name=Value, _O) -->
	atom(Name),"=",value(Name, Value).
attribute(html(Value), _, List, Tail) :- !,
	format(codes(List,Tail), 'label=<~w>', [Value]).
attribute(NameValue, _O)  -->
	{NameValue =.. [Name,Value]}, !,
	atom(Name),"=",value(Name, Value).

value(Name, Value) -->
	{ string_attribute(Name), !,
	  atom_codes(Value, Codes)
	},
	"\"", cstring(Codes), "\"".
value(_Name, Value, List, Tail) :-
	format(codes(List,Tail), '~w', [Value]).

id(ID) --> { number(ID) }, !, number(ID).
id(ID) --> { atom(ID), !, atom_codes(ID, Codes) }, "\"", cstring(Codes), "\"".
id(ID) --> { string(ID), !, string_codes(ID, Codes) }, "\"", cstring(Codes), "\"".
id(ID) --> { format(codes(Codes), '~p', [ID]) }, "\"", cstring(Codes), "\"".

keyword(Kwd) --> atom(Kwd).
indent(Options) -->
	{ Level = Options.indent },
	spaces(Level).
ws --> " ".
nl --> "\n".

spaces(0) --> !.
spaces(N) -->
	{ succ(N2, N) },
	" ",
	spaces(N2).



		 /*******************************
		 *	  DOT PRIMITIVES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code is copied from ClioPatria, rdf_graphviz.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

string_attribute(label).
string_attribute(url).
string_attribute(href).
string_attribute(id).
string_attribute('URL').
string_attribute(fillcolor).
string_attribute(style).
string_attribute(size).

%%	gv_attr(?AttrName, ?Element, ?Type) is nondet.
%
%	Name and type-declarations for GraphViz   attributes.  Types are
%	defined my must_be/2.
%
%	@see http://www.graphviz.org/doc/info/shapes.html

gv_attr(align,	      table, oneof([center,left,right])).
gv_attr(bgcolor,      table, atom).
gv_attr(border,	      table, atom).
gv_attr(cellborder,   table, atom).
gv_attr(cellpadding,  table, atom).
gv_attr(cellspacing,  table, atom).
gv_attr(color,	      table, atom).
gv_attr(fixedsize,    table, boolean).
gv_attr(height,	      table, atom).
gv_attr(href,	      table, atom).
gv_attr(port,	      table, atom).
gv_attr(target,	      table, atom).
gv_attr(title,	      table, atom).
gv_attr(tooltip,      table, atom).
gv_attr(valign,	      table, oneof([middle,bottom,top])).
gv_attr(width,	      table, atom).

gv_attr(align,	      td,    oneof([center,left,right,text])).
gv_attr(balign,	      td,    oneof([center,left,right])).
gv_attr(bgcolor,      td,    atom).
gv_attr(border,	      td,    atom).
gv_attr(cellpadding,  td,    atom).
gv_attr(cellspacing,  td,    atom).
gv_attr(color,	      td,    atom).
gv_attr(colspan,      td,    integer).
gv_attr(fixedsize,    td,    boolean).
gv_attr(height,	      td,    atom).
gv_attr(href,	      td,    atom).
gv_attr(port,	      td,    atom).
gv_attr(rowspan,      td,    integer).
gv_attr(target,	      td,    atom).
gv_attr(title,	      td,    atom).
gv_attr(tooltip,      td,    atom).
gv_attr(valign,	      td,    oneof([middle,bottom,top])).
gv_attr(width,	      td,    atom).

gv_attr(color,	      font,  atom).
gv_attr(face,	      font,  atom).
gv_attr('point-size', font,  integer).

gv_attr(align,	      br,    oneof([center,left,right])).

gv_attr(scale,	      img,   oneof([false,true,width,height,both])).
gv_attr(src,	      img,   atom).


%%	cstring(+Codes)//
%
%	Create a C-string. =dot= uses UTF-8 encoding.

cstring([]) -->
	[].
cstring([H|T]) -->
	(   cchar(H)
	->  []
	;   [H]
	),
	cstring(T).

cchar(0'") --> "\\\"".
cchar(0'\n) --> "\\n".
cchar(0'\t) --> "\\t".
cchar(0'\b) --> "\\b".
