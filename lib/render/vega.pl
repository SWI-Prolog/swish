/*  Part of SWISH

    Author:        Kwon-Young Choi
    E-mail:        kwon-young.choi@hotmail.fr
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

:- module(swish_render_vega,
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

:- register_renderer(vega, "Render data using Vega-Lite").

/** <module> SWISH Vega-Lite based chart renderer

Render data as a chart using Vega-Lite.
*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Renders Term as a Vega-Lite chart. This renderer recognises a
%	dict with tag `vega`.

term_rendering(Vega, _Vars, _Options) -->
	{ is_dict(Vega, Tag),
	  Tag == vega,
	  gensym('vega', Id),
	  atom_concat('#', Id, RefId)
	},
	html(div([ class(['render-vega']),
		   'data-render'('As Vega chart')
		 ],
		 [ div(id(Id), []),
		   \js_script({|javascript(RefId, Vega)||
(function() {
  if ( $.ajaxScript ) {
    var spec = Vega;
    require(["vega-embed"], function(vegaEmbed) {
      vegaEmbed(RefId, spec, {"actions": true});
    });
  }
})();
			      |})
		 ])).


