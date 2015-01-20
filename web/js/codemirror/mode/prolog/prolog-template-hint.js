// Loosely based on codemirror/addon/hint/xml-hint.js
(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define([ "../../lib/codemirror",
	     "../../addon/hint/templates-hint",
	     "jquery", "config", "laconic"
	   ], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror, TemplatesHint, $, config) {
  "use strict";

  var templates = config.swish.templates || [];

  function PrologHint(options) {
    var hint = this;

    function modeToTemplate(mode) {
      var varp = /[-+?:^@!]*([A-Z][A-Za-z_0-9]*)/g;
      var detp = /\bis\s+(det|nondet|semidet|fail|multi)$/;

      if ( !hint.template ) {
	hint.template    = mode.replace(varp, "$${$1}")
			       .replace(detp, "");
	if ( !hint.template.match(/\${cursor}/) )
	  hint.template += "${cursor}";
      }

      if ( !hint.displayText )
	hint.displayText = mode;

      if ( !hint.varTemplates ) {
	var metav = mode.match(/:[A-Z][A-Za-z_0-9]*/g);

	if ( metav && metav.length > 0 ) {
	  var vt = {};

	  for(var i=0; i<metav.length; i++) {
	    vt[metav[i].substring(1)] = templates;
	  }
	  hint.varTemplates = vt;
	}
      }
    }

    var copy = [ "template",
		 "displayText",
		 "text",
		 {from:"summary", to:"description"},
		 "className",
		 "varTemplates"
	       ];

    if ( typeof(options) == "string" ) {
      this.displayText = options;
      this.text = options;
    } else {
      for(var i=0; i<copy.length; i++) {
	var spec = copy[i];
	if ( typeof(spec) == "string" ) {
	  if ( options[spec] )
	    this[spec] = options[spec];
	} else {
	  if ( options[spec.from] )
	    this[spec.to] = options[spec.from];
	}
      }

      if ( options.mode )
	modeToTemplate(options.mode);

      if ( options.classes )
	options.className = options.classes.join(" ");
    }

    /**
     * Render a completion label inside a <li>.  This hook is called
     * from standard CM addons/hint/show-hint.js to render the hinting
     * alternatives.
     * @param li is the list element
     * @param data seems to be the result from getHints()
     * @param me is this PrologHint instance
     */
    this.render = function(li, data, me) {
      $(li).append(me.displayText);
    }
    /**
     * Provide an HTML string or element that describes the completion.
     * used by addon/hint/show-context-info.js
     * @param me is this PrologHint instance
     */
    this.info = function(me) {
      return me.description;
    }
  }

  PrologHint.prototype.hint = function(cm, data, completion) {
    var template = new CodeMirror.templatesHint.Template(this);
    template.insert(cm, data);
  };

  function hintsFor(cm, state, options) {
    var text = state.token.string;
    var results = [];
    var myTemplates = templates;

    function startsWith(str, start) {
      return str.slice(0, start.length) == start;
    }

    if ( cm._hintTemplateMarker ) {
      if ( text == "\u2630" )
	text = "";
      myTemplates = cm._hintTemplateMarker._templates;
    }

    var symbol = text.length > 0 && !text.match(/\w/);

    if ( myTemplates ) {
      for(var i=0; i<myTemplates.length; i++) {
	var templ = myTemplates[i];
	if ( typeof(templ) == "string" ) {
	  if ( startsWith(templ, text) )
	    results.push(new PrologHint(templ));
	} else {
	  var fields = ["name", "mode", "template", "text"];

	  if ( symbol ) {
	    if ( templ.name && templ.name.indexOf(text) >= 0 )
	      results.push(new PrologHint(templ));
	  } else {
	    for(var j=0; j<fields.length; j++) {
	      if ( templ[fields[j]] )
	      { if ( startsWith(templ[fields[j]], text) ) {
		  results.push(new PrologHint(templ));
	        }
		break;
	      }
	    }
	  }
	}
      }
    }

    if ( results.length == 0 ) {	/* Include AnyWord hinting completions */
      var anyword = CodeMirror.hint.anyword;
      var opts = (text==""&&cm._hintTemplateMarker) ?
		  { word: /[A-Z][A-Za-z0-9_]*/ } : options;
      var anyhint = anyword(cm, opts);
      for(var i=0; i<anyhint.list.length; i++)
	results.push(new PrologHint(anyhint.list[i]));
    }

    return { list: results,
             from: state.position.from,
	     to:   state.position.to
           };
  }

  function getHints(cm, callback, options) {
    var state = getState(cm);
  //console.log(state);
    var data  = hintsFor(cm, state, options);
    CodeMirror.attachContextInfo(data);
    callback(data);
  }
  getHints.async = true;

  function getState(cm) {
    var cur = cm.getCursor();
    var token = cm.getTokenAt(cur);
//  var enriched = cm.getEnrichedToken(token);
    var inner = CodeMirror.innerMode(cm.getMode(), token.state);
    if (inner.mode.name != "prolog") {
      return null;
    }

    var tokenPosition = {
      from: new CodeMirror.Pos(cur.line, token.start),
      to:   new CodeMirror.Pos(cur.line, token.end)
    };

    return { token:    token,
	     position: tokenPosition
           };
  }

  CodeMirror.registerHelper("hint", "prologTemplate", getHints);

  return {
    getHints: getHints,
    getState: getState
  };
});
