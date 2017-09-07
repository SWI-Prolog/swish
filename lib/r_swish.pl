/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(r_swish,
	  [ r_download/0,			% Download all
	    r_download/1			% +File
	  ]).
:- use_module(library(pengines)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).

% We publish to the R interface to `swish`
:- use_module(swish:library(r/r_call)).
:- use_module(swish:library(r/r_data)).

:- use_module(library(r/r_call)).
:- use_module(library(r/r_serve)).
:- use_module(download).

/** <Module> Bind Rserve to SWISH

The user must provide the  file  search   path  =rserve=  to local the R
connection library.
*/

:- multifile
	r_call:r_console/2,
	r_call:r_display_images/1,
	r_call:r_console_property/1.

%%	r_call:r_console(+Stream, ?Data)
%
%	Relay Rserve captured output to SWISH using writeln.

r_call:r_console(stdout, []) :- !.
r_call:r_console(stdout, Strings) :-
	atomics_to_string(Strings, "\n", String),
	send_html(pre(class(['R', console]), String)).

send_html(HTML) :-
	phrase(html(HTML), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)),
	pengine_output(HTMlString).

%!	r_call:r_console_property(?Property)
%
%	Relay the size of the console

r_call:r_console_property(size(Rows, Cols)) :-
	swish:tty_size(Rows, Cols).

%%	r_call:r_display_images(+Images)
%
%	Relay   received   images   to   the     SWISH   console   using
%	pengine_output/1.

r_call:r_display_images(Images) :-
	svg_html(Images, HTMlString),
	pengine_output(HTMlString).

%%	svg_html(+Images, -HTMlString) is det.
%
%	Turn a list of SVG images into an HTML string.

svg_html(Images, HTMlString) :-
	phrase(svg_html(Images), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)).

svg_html(Images) -->
	html(div(class('Rplots'), \rplots(Images))).

rplots([]) --> [].
rplots([H|T]) -->
	html(div(class(['reactive-size', 'R', svg]), \plot(H, []))),
	rplots(T).


plot(svg(SVG), _Options) --> !,
	html(\[SVG]),
	pan_zoom,
	"".
plot(Term, _Options) --> !,
	{ domain_error(image, Term) }.

%%	pan_zoom
%
%	Add pan and soom behaviour to embedded SVG.  This function also
%	renames the `id` attribute and their references.
%
%	@bug	We need a generic way to fix all references to the ID.
%		Is there a list of such attributes?
%	@bug	Instead of `"use"`, we should use `"[xlink\\:href]"`,
%		but this does not seem to work!?
%	@bug	When generalised, this could move into runner.js.

pan_zoom -->
	html(\js_script({|javascript||
var svg  = node.node().find("svg");
var data = { w0: svg.width(),
	     h0: svg.height()
	   };
var pan;

function fixIDs(node, prefix1) {
  var i=0;
  node.each(function() {
    var prefix = prefix1+(i++)+"_";
    var img = $(this);
    var hprefix = "#"+prefix;
    var re = /(url\()#([^)]*)(\))/;

    img.find("[id]").each(function() {
      var elem = $(this);
      elem.attr("id", prefix+elem.attr("id"));
    });
    img.find("use").each(function() {
      var elem = $(this);
      var r = elem.attr("xlink:href");
      if ( r.charAt(0) == "#" )
	elem.attr("xlink:href", hprefix+r.slice(1));
    });
    img.find("[clip-path]").each(function() {
      var elem = $(this);
      var r = elem.attr("clip-path").match(re);
      if ( r.length == 4 )
	elem.attr("clip-path", r[1]+hprefix+r[2]+r[3]);
    });
  });
}

fixIDs(svg, "N"+node.unique_id()+"_");

function updateSize() {
  var w = svg.closest("div.Rplots").innerWidth();
  console.log(data.w0, w);

  function reactive() {
    if ( !data.reactive ) {
      var div = svg.closest("div.reactive-size");
      data.reactive = true;
      div.on("reactive-resize", updateSize);
    }
  }

  reactive();
  w = Math.max(w*0.95, 100);
  if ( w < data.w0 ) {
    svg.width(w);
    svg.height(w = Math.max(w*data.h0/data.w0, w/4));
    if ( pan ) {
      pan.resize();
      pan.fit();
      pan.center();
    }
  }
}

require(["svg-pan-zoom"], function(svgPanZoom) {
  updateSize()
  pan = svgPanZoom(svg[0], {
    maxZoom: 50
  });
});
		      |})).


%%	r_download
%
%	Provide download buttons for all created  files. First calls the
%	R function `graphics.off()` to close all graphics devices.

r_download :-
	nb_current('R', _), !,
	<- graphics.off(),
	Files <- list.files(),
	maplist(r_download, Files).
r_download.

%%	r_download(File)
%
%	Provide a download button for the indicates file.

r_download(File) :-
	nb_current('R', _), !,
	catch(r_read_file($, File, Content), E,
	      r_error(E, File)),
	(   debugging(r(file))
	->  string_length(Content, Len),
	    debug(r(file), 'Got ~D bytes from ~p', [Len, File])
	;   true
	),
	file_name_extension(_Name, Ext, File),
	download_encoding(Ext, Enc),
	download_button(Content,
			[ filename(File),
			  encoding(Enc)
			]).
r_download(File) :-
	existence_error(r_file, File).

r_error(error(r_error(70),_), File) :- !,
	existence_error(r_file, File).
r_error(Error, _) :- throw(Error).

download_encoding(svg, utf8) :- !.
download_encoding(csv, utf8) :- !.
download_encoding(_,   octet).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(r_swish:r_download).
sandbox:safe_primitive(r_swish:r_download(_)).
