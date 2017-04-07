/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
			      CWI Amsterdam
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

/**
 * @fileOverview
 * Load SWISH. Just provides the RequireJS config, requires jswish.js
 * and initialises this on the body.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

require.config({
  urlArgs: "ts="+new Date().getTime(),	/* prevent caching during development */
  waitSeconds: 60,			/* swish-min.js is big */
  paths:
  { jquery:      "../bower_components/jquery/dist/jquery.min",
    "jquery-ui": "../bower_components/jquery-ui/jquery-ui.min",
    laconic:     "../bower_components/laconic/laconic",
    bootstrap:   "../bower_components/bootstrap/dist/js/bootstrap.min",
    bloodhound:  "../bower_components/typeahead.js/dist/bloodhound",
    typeahead:   "../bower_components/typeahead.js/dist/typeahead.jquery",
    splitter:    "../bower_components/jquery.splitter/js/jquery.splitter-0.15.0",
    tagmanager:  "../bower_components/tagmanager/tagmanager",
    sha1:        "../bower_components/js-sha1/src/sha1",
    c3:          "../bower_components/c3/c3",
    d3:          "../bower_components/d3/d3",
    "svg-pan-zoom": "../bower_components/svg-pan-zoom/dist/svg-pan-zoom.min",
    sparkline:	 "../bower_components/sparkline/dist/jquery.sparkline",

					/* CodeMirror extensions */
    "cm/mode/prolog": "codemirror/mode/prolog",
    "cm/addon/hover/prolog-hover": "codemirror/addon/hover/prolog-hover",
    "cm/addon/hover/text-hover": "codemirror/addon/hover/text-hover",
    "cm/addon/hint/templates-hint": "codemirror/addon/hint/templates-hint",
    "cm/addon/hint/show-context-info": "codemirror/addon/hint/show-context-info",

					/* Standard CodeMirror */
    "cm" : "../bower_components/codemirror"
  },
  shim:
  { bootstrap:
    { deps:["jquery"]
    },
    typeahead: /* HACK: See https://github.com/twitter/typeahead.js/issues/1211 */
    { deps:["jquery"],
      init: function ($) {
	return require.s.contexts._.registry['typeahead.js'].factory($);
      }
    },
    bloodhound:
    { deps:["jquery"]
    },
    splitter:
    { deps:["jquery"]
    },
    laconic:
    { deps:["jquery"]
    },
    tagmanager:
    { deps:["jquery"]
    },
  }
}); //require.config

/*
 * Create the SWISH application.  Note that we need two levels of
 * require because the first gives us the location of the pengine
 * API, while the second fetches the pengines and starts the
 * application.
 */
require(["jquery", "config", "jswish"],
	function($, config, swish) {
  require([config.http.locations.pengines+"/pengines.js"],
	  function() {
    $(function() {
      $("body").swish(config.swish||{});
    });
  });
});

