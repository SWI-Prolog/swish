define([ "../../lib/codemirror",
	 "jquery",
	 "laconic"
       ],
       function(CodeMirror, $) {
"use strict";

var tokenHelp = {
  "goal_built_in":  function(data, cm) {
    if ( data ) {
      return $.el.div(predName(data), ": ",
		      cm.predicateInfo(data));
    } else {
      return "Built-in predicate";
    }
  },

  "goal_autoload":  function(data, cm) {
    if ( data ) {
      return $.el.div(predName(data), " (autoload from ",
		      fileName(data), "): ",
		      cm.predicateInfo(data));
    } else {
      return "Autoloaded predicate";
    }
  },

  "goal_imported":  function(data, cm) {
    if ( data ) {
      return $.el.div(predName(data), " (imported from ",
		      fileName(data), "): ",
		      cm.predicateInfo(data));
    } else {
      return "Imported predicate";
    }
  },

  "goal_recursion": "Recursive call",
  "goal_local":     "Locally defined predicate",
  "goal_dynamic":   "Dynamic predicate",
  "goal_undefined": "Undefined predicate",

  "head_unreferenced": "Predicate is not called",

  "singleton": "Variable appearing only once",

  "head":       null,
  "var":        null,
  "int":        null,
  "number":     null,
  "atom":       null,
  "comment":    null,
  "neck":       null,
  "list_open":  null,
  "list_close": null
};

function predName(data) {
  return data.text+"/"+data.arity;
}

function fileName(data) {
  var last;

  if ( (last=data.file.lastIndexOf("/")) ) {
    return data.file.substring(last+1);
  } else
    return data.file;
}

function summary(data, cm) {
  var server = cm.state.prologHighlightServer;

  if ( server && server.url && server.url.info ) {
  }
}


CodeMirror.registerHelper("textHover", "prolog", function(cm, data, node) {
  if ( data ) {
    var token = data.token;
    var help  = tokenHelp[token.type];
    var html;

    if ( help !== undefined ) {
      if ( typeof(help) === "function" ) {
	var r = help(cm.getEnrichedToken(token), cm);

	if ( typeof(r) === "string" )
	  return $.el.div(r);
	else
	  return r;
      } else if ( typeof(help) === "string" ) {
	return $.el.div(help);
      }
    } else {
      return $.el.div(token.type);
    }
  }

  return null;
});

});
