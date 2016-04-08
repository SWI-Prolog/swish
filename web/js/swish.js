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
    typeahead:   "../bower_components/typeahead.js/dist/typeahead.bundle.min",
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
    typeahead:
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
    }
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

