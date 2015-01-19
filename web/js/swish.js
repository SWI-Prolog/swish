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
    splitter:    "../bower_components/jquery.splitter/js/jquery.splitter-0.14.0",
    tagmanager:  "../bower_components/tagmanager/tagmanager",

    "cm/lib/codemirror": "../bower_components/codemirror/lib/codemirror",
					/* CodeMirror standard extensions */
    "cm/addon/edit/matchbrackets": "../bower_components/codemirror/addon/edit/matchbrackets",
    "cm/addon/comment/continuecomment": "../bower_components/codemirror/addon/comment/continuecomment",
    "cm/addon/comment/comment": "../bower_components/codemirror/addon/comment/comment",
    "cm/addon/hint/show-hint": "../bower_components/codemirror/addon/hint/show-hint",
    "cm/addon/hint/anyword-hint": "../bower_components/codemirror/addon/hint/anyword-hint",
    "cm/addon/display/placeholder": "../bower_components/codemirror/addon/display/placeholder",
    "cm/addon/runmode/runmode": "../bower_components/codemirror/addon/runmode/runmode",
    "cm/keymap/emacs" : "../bower_components/codemirror/keymap/emacs",

					/* Our own Prolog mode */
    "cm/mode/prolog/prolog": "codemirror/mode/prolog/prolog",
    "cm/mode/prolog/prolog_keys": "codemirror/mode/prolog/prolog_keys",
    "cm/mode/prolog/prolog_query": "codemirror/mode/prolog/prolog_query",
    "cm/mode/prolog/prolog_server": "codemirror/mode/prolog/prolog_server",

    "cm/addon/hover/text-hover": "codemirror/addon/hover/text-hover",
    "cm/addon/hover/prolog-hover": "codemirror/addon/hover/prolog-hover",

    "cm/addon/hint/templates-hint": "codemirror/addon/hint/templates-hint",
    "cm/addon/hint/show-context-info": "codemirror/addon/hint/show-context-info",
    "cm/mode/prolog/prolog-template-hint": "codemirror/mode/prolog/prolog-template-hint"
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

require(["jquery", "config", "jswish"],
	function($, config, swish) {

$(function() {
  $("body").swish(config.swish||{});
}); //$();

}); // require

