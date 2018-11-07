/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(swish_render_mathjax,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module('../render').

:- register_renderer(mathjax, "Render mathematical formulas").

:- setting(swish:mathjax_url, string,
           "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js\c
            ?config=TeX-MML-AM_CHTML",
           "Location from which to download Mathjax").

/** <module> SWISH Mathjax renderer

Render mathematical formulas
*/

%!  term_rendering(+Term, +Vars, +Options)//
%
%   Render Term using Mathjax.

term_rendering(math(String), _Vars, _Options) -->
    { string(String),
      setting(swish:mathjax_url, MathJaxURL)
    },
    html(span([ class(mathjax),
		'data-render'('As math')
	      ],
	      [ '\\(', String, '\\)',
		\js_script({|javascript(MathJaxURL)||
(function() {
  if ( $.ajaxScript ) {
    var span = $.ajaxScript.parent()[0];
    require([MathJaxURL],
	    function() {
		MathJax.Hub.Queue(["Typeset",MathJax.Hub,span]);

    });
  }
})();
			   |})
	      ])).
