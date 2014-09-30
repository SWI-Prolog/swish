// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  addon  extends  the  "prolog"   mode  to  perform  server-assisted
highlighting.   Server-assisted   highlighting   provides   a   semantic
classification of tokens.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  "use strict";

  var DEFAULT_DELAY = 1000;

  function State(options) {

    function generateUUID() {
      var d = new Date().getTime();
      var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
	.replace(/[xy]/g, function(c) {
	  var r = (d + Math.random()*16)%16 | 0;
	  d = Math.floor(d/16);
	  return (c=='x' ? r : (r&0x7|0x8)).toString(16);
	});
      return uuid;
    };

    if (typeof options == "object") {
      this.uuid = options.uuid || generateUUID();
      this.url  = { change: options.url + "change",
		    tokens: options.url + "tokens",
		    leave:  options.url + "leave"
		  },
      this.delay = options.delay ? options.delay : DEFAULT_DELAY;
      this.generationFromServer = -1;
      this.tmo = null;			/* timeout handle */
    }
  }

  function changeEditor(cm, change) {
    var state = cm.state.prologHighlightServer;

    if ( state == null || state.url == null )
      return;

    if ( state.tmo ) {			/* changing: delay refresh */
      cm.askRefresh();
    }

    $.ajax({ url: state.url.change,
             dataType: "json",
	     contentType: 'application/json',
	     type: "POST",
	     data: JSON.stringify({ uuid: state.uuid,
	                            change: change
				  }),
	     success: function() {
	       if ( change.origin == "setValue" ||
		    state.generationFromServer == -1 )
		 cm.serverAssistedHighlight();
	     }
	   });
  }

  function leaveEditor(cm) {
    var state = cm.state.prologHighlightServer;

    if ( state == null || state.url == null )
      return;

    console.log("Leaving CodeMirror "+state.uuid);

    $.ajax({ url: state.url.leave,
	     async: false,  // otherwise it is killed before completion
	     contentType: 'application/json',
	     type: "POST",
	     data: JSON.stringify({ uuid: state.uuid
				  })
	   });
  }

  CodeMirror.defineOption("prologHighlightServer", false, function(cm, val, old) {
    if ( old && old != CodeMirror.Init ) {
      /* FIXME: Unregister the Prolog server */
      cm.off("change", changeEditor);
    }
    if ( val ) {
      cm.state.prologHighlightServer = new State(val);
      cm.on("change", changeEditor);
      window.addEventListener("unload", function() { leaveEditor(cm); });
    }
  });


  CodeMirror.prototype.askRefresh = function(time) {
    var cm = this;
    var state = cm.state.prologHighlightServer;

    if ( state == null )
      return;

    if ( time === undefined )
      time = state.delay;

    if ( state.tmo )
      clearTimeout(state.tmo);

    state.tmo = setTimeout(function() { cm.serverAssistedHighlight(); },
			   time);
  };


  CodeMirror.prototype.serverAssistedHighlight = function(always) {
    var cm = this;
    var state = cm.state.prologHighlightServer;

    state.tmo = null;

    if ( state == null || state.url == null ||
	 (cm.isClean(state.generationFromServer) && !always) )
      return;

    function modeOptions() {
      var opts = cm.getOption("mode");

      if ( typeof(opts) != "object" ) {
	opts = { name:"prolog",
		 enrich:enrichStyle,
		 editor:cm
	       };
      } else if ( !opts.enrich ) {
	opts.enrich = enrichStyle;
	opts.editor = cm;
      }
      return opts;
    }

    state.generationFromServer = cm.changeGeneration();
    $.ajax({ url: state.url.tokens,
	     dataType: "json",
	     data: { uuid: state.uuid },
	     success: function(data, status) {
	       var opts = modeOptions();
	       opts.metainfo = data;
	       cm.setOption("mode", opts);
	     }
	   });
  }

  CodeMirror.commands.refreshHighlight = function(cm) {
    cm.serverAssistedHighlight(true);
  }

  var syncOnType = { "var": "var",	/* JavaScript Types */
		     "atom": "atom",
		     "qatom": "qatom",
		     "bqstring": "string",
		     "symbol": "atom",
		     "functor": "functor",
		     "tag": "tag",
		     "number": "number",
		     "string": "string",
		     "code": "number",
		     "neg-number": "number",
		     "list_open": "list_open",
		     "list_close": "list_close",
		     "qq_open": "qq_open",
		     "qq_sep": "qq_sep",
		     "qq_close": "qq_close",
		     "dict_open": "dict_open",
		     "dict_close": "dict_close",
		     "brace_term_open": "brace_term_open",
		     "brace_term_close": "brace_term_close",
		     "neck": "neck",
		     "fullstop": "fullstop"
		   };
  var serverSync = { "var": "var",	/* Server Types */
		     "singleton": "var",
		     "atom": "atom",
		     "qatom": "qatom",
		     "string": "string",
		     "codes": "string",
		     "chars": "string",
		     "functor": "functor",
		     "tag": "tag",
		     "control": "atom",
		     "meta": "atom",	/* or number 0-9 */
		     "op_type": "atom",
		     "int": "number",
		     "float": "number",
		     "key": "atom",
		     "sep": "atom",	/* : in dict */

		     "expanded": "expanded",
		     "comment_string":"string",
		     "identifier": "atom",
		     "module": "atom",

		     "head_exported": "atom",
		     "head_public": "atom",
		     "head_extern": "atom",
		     "head_dynamic": "atom",
		     "head_multifile": "atom",
		     "head_unreferenced": "atom",
		     "head_hook": "atom",
		     "head_meta": "atom",
		     "head_constraint": "atom",
		     "head_imported": "atom",
		     "head_built_in": "atom",
		     "head_iso": "atom",
		     "head_def_iso": "atom",
		     "head_def_swi": "atom",
		     "head": "atom",

		     "goal_built_in": "atom",
		     "goal_imported": "atom",
		     "goal_autoload": "atom",
		     "goal_global": "atom",
		     "goal_undefined": "atom",
		     "goal_thread_local": "atom",
		     "goal_dynamic": "atom",
		     "goal_multifile": "atom",
		     "goal_expanded": "atom",
		     "goal_extern": "atom",
		     "goal_recursion": "atom",
		     "goal_meta": "atom",
		     "goal_foreign": "atom",
		     "goal_local": "atom",
		     "goal_constraint": "atom",
		     "goal_not_callable": "atom",

		     "xpce_method": "functor",
		     "xpce_class_builtin":"atom",
		     "xpce_class_lib":"atom",
		     "xpce_class_user":"atom",
		     "xpce_class_undef":"atom",

		     "option_name": "atom",
		     "no_option_name": "atom",
		     "flag_name": "atom",
		     "no_flag_name": "atom",

		     "file_no_depends": "atom",
		     "file": "atom",
		     "nofile": "atom",

		     "list_open": "list_open",
		     "list_close": "list_close",
		     "qq_open": "qq_open",
		     "qq_sep": "qq_sep",
		     "qq_close": "qq_close",
		     "qq_type": "atom",
		     "dict_open": "dict_open",
		     "dict_close": "dict_close",
		     "brace_term_open": "brace_term_open",
		     "brace_term_close": "brace_term_close",
		     "neck": "neck",
		     "fullstop": "fullstop",

		     "html": "functor",
		     "entity": "atom",
		     "html_attribute": "functor",
		     "sgml_attr_function": "atom",
		     "http_location_for_id": "atom",
		     "http_no_location_for_id": "atom"
		   };

  /* Enrich the style using the token list from the server.

  @param stream  is the CM input stream
  @param state   is the mode state object
  @param type    is the syntactic category detected by the mode
  @param content is the textual content of the token (if any)
  @param style   is the style determined by the mode.
  */

  function enrichStyle(stream, state, type, content, style) {
    var parserConfig = this;

    /* serverToken(state) returns the server token at
       state.curTerm.curToken or null
    */

    function serverToken(state) {
      var term = parserConfig.metainfo[state.curTerm];
      if ( !term ) return null;
      var token = term[state.curToken];
      if ( !token ) return null;

      return token;
    }

    /* outOfSync() is called whenever we lost synchronization.  It
       records the position where we lost sync and asks the server
       for a fresh analysis.
    */

    function outOfSync() {
      if ( !state.outOfSync ) {
	console.log("Mismatch: ("+content+") "+type+"/"+token.type);
	state.outOfSync = { okToken:       state.curToken,
			    okTerm:        state.curTerm,
			    skippedTerms:  0,
			    skippedTokens: [],
			  };
      }
      parserConfig.editor.askRefresh();
    }

    /* reSync() tries to re-synchronize after we lost synchronization
       with the server due to local edits.  If successful, it clears
       state.outOfSync and returns the enriched token.  Else it returns
       null.

       Data to work from:

	 state.outOfSync.skipped: skipped tokens since out-of-sync or
		last term
	 state.outOfSync.skippedTerms: number of fullstop seen since
		we lost synchronisation
    */

    function reSync() {
      var oos = state.outOfSync;
      var resyncState = { curToken: oos.okToken,
			  curTerm:  oos.okTerm
			};

      /* TBD */
      if ( skippedTerms ) {

      } else {
      }

      return null;
    }


    /* match the next token.  It is possible that the server has combined
       several tokens into one logical unit.  In that case the token is
       merely a prefix of what the server returned and we try to eat the
       remainder.  Examples are files specifications such as
       library(lists).
    */

    function matchTokenText(tokenText) {
      var start;

      if ( content == tokenText )
	return true;

      if ( (start=tokenText.lastIndexOf(content,1)) >= 0 ) {
	var left = tokenText.substring(content.length+start);
	for(var i=0; i<left.length; i++) {
	  if ( !stream.eat(left.charAt(i)) ) {
	    stream.backUp(i);
	    return false;
	  }
	}
	return true;
      }

      return false;
    }

    /* matchToken(token, state) matches the server token `token` to the
       current token and updates state.curToken and/or state.curTerm if
       the two matches.  It return the enriched style or null.
    */

    function matchToken(token, state) {
      if ( token && syncOnType[type] ) {
	if ( token.text && content ) {
	  if ( matchTokenText(token.text) ) {
	    state.curToken++;
	    return token.type;
	  }

	  return null;
	} else if ( syncOnType[type] == serverSync[token.type] ) {
	  if ( type == "fullstop" ) {
	    state.curTerm++;
	    state.curToken = 0;
	  } else {
	    state.curToken++;
	  }
	  return token.type;
	} else if ( type == "qatom" && serverSync[token.type] == "atom" ) {
          state.curToken++;
	  return token.type;
	} else if ( type == "number" && token.type == "meta" ) {
	  state.curToken++;	/* 0-9 as meta_predicate arguments */
	  return token.type;
	} else if ( type == "neg-number" &&
		    token.text && token.text == "-" ) {
	      /* HACK: A-1 is tokenised as "var" "neg-number" */
	      /* But the server says "var" "atom" "number" */
	      /* Needs operator logic to fix at the client */
	  state.curToken += 2;
	  return "number";
        }
      }

      if ( token && content && token.text == content ) {
	state.curToken++;			/* ,; are not synced */
	return token.type;
      }

      return null;
    }

    /* enrichStyle() body */

    if ( state.curTerm != null ) {
      var token;
      var serverStyle;

      if ( state.syntax_error ) {		/* error state; recap after . */
	if ( type == "fullstop" )
	  delete state.syntax_error;
	return style;
      }

      if ( state.outOfSync ) {			/* lost synchronization */
	var oos = state.outOfSync;

	if ( oos.skippedTerms <= 3 ) {
	  oos.skipped.push({ type:    type,
			     style:   style,
			     content: content
			   });

	  if ( (serverStyle=reSync()) ) {
	    return serverStyle;			/* re-synchronized! */
	  } else if ( type == "fullstop" ) {
	    oos.skipped = [];
	    oos.skippedTerms++;
	  }
	}

	return style;
      }

      if ( !(token=serverToken(state)) ) {
	parserConfig.editor.askRefresh();	/* more text added at the end */
	return style;
      }

      //console.log("Enrich: ("+content+") "+type+"/"+token.type);

      if ( (serverStyle=matchToken(token, state)) ) {
	return serverStyle;
      } else if ( token.type == "syntax_error" ) {
	state.syntax_error = true;
	state.curToken = 0;
	state.curTerm++;
	return style;
      } else {
	outOfSync();
	return style + " outofsync";
      }
    }

    return style;
  }

		 /*******************************
		 *	  FETCH ENRICHED	*
		 *******************************/

  CodeMirror.prototype.getEnrichedToken = function(token) {
    if ( token.state.curTerm != null && token.state.curToken != null )
    { var state = this.getOption("mode");

      if ( state.metainfo )
	return state.metainfo[token.state.curTerm][token.state.curToken-1];
    }

    return undefined;
  }

});
