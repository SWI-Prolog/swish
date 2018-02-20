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

  CodeMirror.commands.prologMaybeFireQuery = function(cm) {
    var lastl  = cm.lineCount()-1;

    while(lastl >= 0 && cm.getLine(lastl).trim() == "")
      lastl--;

    if ( lastl >= 0 ) {
      var endc  = cm.getLine(lastl).length;
      var token = cm.getTokenAt({line:lastl, ch:endc}, true);

      if ( token.type == "fullstop" ) {
	var c = cm.getCursor();

	if ( c.line > lastl || (c.line == lastl && c.ch >= token.end) ) {
	  $(".swish-event-receiver")
	     .trigger("feedback",
		      { html: "Use <b>Ctrl+Enter</b> to execute the query",
			owner: $(cm.display.wrapper).closest(".pane-wrapper")
		      });
	}
      }
    }

    return CodeMirror.Pass;
  }

  CodeMirror.commands.prologFireQuery = function(cm) {

    return cm.prologFireQuery(cm.getValue());
  }

  CodeMirror.defineOption("prologQuery", null, function(cm, func, prev) {
    if (prev && prev != CodeMirror.Init)
      cm.removeKeyMap("prologQuery");
    if ( typeof(func) == "function" ) {
      var map = { name:         "prologQuery",
		  "Ctrl-Enter": "prologFireQuery",
		  "Enter":	"prologMaybeFireQuery"
		};
      cm.addKeyMap(map);
      cm.prologFireQuery = func;
    }
  });
});
