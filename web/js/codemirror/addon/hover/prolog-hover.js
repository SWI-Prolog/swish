(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

var tokenHelp = {
  "goal_built_in":  "Built-in predicate",
  "goal_autoload":  "Autoloaded predicate",
  "goal_imported":  "Imported predicate",
  "goal_recursion": "Recursive call",
  "goal_local":     "Local predicate",
  "goal_dynamic":   "Dynamic predicate",
  "goal_undefined": "Undefined predicate",

  "head_unreferenced": "Predicate is not called",

  "head":       null,
  "var":        null,
  "int":        null,
  "comment":    null,
  "neck":       null,
  "list_open":  null,
  "list_close": null
};

CodeMirror.registerHelper("textHover", "prolog", function(cm, data, node) {
  if (data) {
    var token = data.token;
    var html;

    if ( tokenHelp[token.type] !== undefined ) {
      html = tokenHelp[token.type];
    } else {
      html = token.type;
    }

    if ( html ) {
      var result = document.createElement('div');
      result.innerHTML = html;
      return result;
    }
  }

  return null;
});

});
