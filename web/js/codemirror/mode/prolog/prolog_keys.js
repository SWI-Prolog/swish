// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";


		 /*******************************
		 *	    ACTIVE KEYS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Support if-then-else layout like this:

goal :-
    (	Condition
    ->  IfTrue
    ;   IfFalse
    ).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


  CodeMirror.commands.prologStartIfThenElse = function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    if ( token.state.goalStart == true )
    { cm.replaceSelection("(   ", "end");
      return;
    }

    return CodeMirror.Pass;
  }

  CodeMirror.commands.prologStartThen = function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    /* FIXME: These functions are copied from prolog.js.  How
       can we reuse these?
    */
    function nesting(state) {
      var len = state.nesting.length;
      if ( len > 0 )
	return state.nesting[len-1];
      return null;
    }

    function isControl(state) {		/* our terms are goals */
      var nest = nesting(state);
      if ( nest ) {
	if ( nest.type == "control" ) {
	  return true;
	}
	return false;
      } else
	return state.inBody;
    }

    if ( start.ch == token.end &&
	 token.type == "operator" &&
	 token.string == "-" &&
	 isControl(token.state) )
    { cm.replaceSelection(">  ", "end");
      return;
    }

    return CodeMirror.Pass;
  }

  CodeMirror.commands.prologStartElse = function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    if ( token.start == 0 && start.ch == token.end &&
	 !/\S/.test(token.string) )
    { cm.replaceSelection(";   ", "end");
      return;
    }

    return CodeMirror.Pass;
  }

  CodeMirror.defineOption("prologKeys", null, function(cm, val, prev) {
    if (prev && prev != CodeMirror.Init)
      cm.removeKeyMap("prolog");
    if ( val ) {
      var map = { name:     "prolog",
		  "'('":    "prologStartIfThenElse",
		  "'>'":    "prologStartThen",
		  "';'":    "prologStartElse",
		  "Ctrl-L": "refreshHighlight"
		};
      cm.addKeyMap(map);
    }
  });

});
