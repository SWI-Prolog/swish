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

  CodeMirror.commands.prologFireQuery = function(cm) {
    var start = cm.getCursor("start");
    var token = cm.getTokenAt(start, true);

    if ( token.type == "fullstop" )
      return cm.prologFireQuery(cm.getValue());

    return CodeMirror.Pass;
  }

  CodeMirror.defineOption("prologQuery", null, function(cm, func, prev) {
    if (prev && prev != CodeMirror.Init)
      cm.removeKeyMap("prologQuery");
    if ( typeof(func) == "function" ) {
      var map = { name:     "prologQuery",
		  "Enter":  "prologFireQuery"
		};
      cm.addKeyMap(map);
      cm.prologFireQuery = func;
    }
  });
});
