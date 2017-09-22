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
  var USE_CHANGES_IF_LONGER_THEN = 1000;

  function State(options) {
    if (typeof options == "object") {
      this.enabled = options.enabled || false;
      this.role    = options.role    || "source";
      if ( options.sourceID )
	this.sourceID = options.sourceID;
      this.url  = { change: options.url + "change",
		    tokens: options.url + "tokens",
		    leave:  options.url + "leave",
		    info:   options.url + "info"
		  },
      this.delay = options.delay ? options.delay : DEFAULT_DELAY;
      this.generationFromServer = -1;
      this.tmo = null;			/* timeout handle */
    }
  }

  /**
   * Trap CodeMirror change events. This asks for
   * serverAssistedHighlight() after a configured delay.  If there
   * is a mirror on the server, we collect the changes in an array
   * and post them when we ask for server tokens.  Earlier versions
   * posted immediately, but this is a waste of resources.  We might
   * want to restore that behaviour if we want to forward changes to
   * other users.
   */
  function changeEditor(cm, change) {
    var state = cm.state.prologHighlightServer;

    if ( state == null || state.url == null || !state.enabled )
      return;

    if ( state.tmo ) {			/* changing: delay refresh */
      cm.askRefresh();
    }

    if ( state.changes !== undefined )
      state.changes.push(change);

    if ( change.origin == "setValue" ||
	 state.generationFromServer == -1 ) {
      state.changes = undefined;	/* force a clean start */
      cm.serverAssistedHighlight();
    }
  }

  function leaveEditor(cm) {
    var state = cm.state.prologHighlightServer;

    if ( state == null || state.url == null || state.uuid == null )
      return;
    var uuid = state.uuid;
    delete state.uuid;

    $.ajax({ url: state.url.leave,
	     async: false,  // otherwise it is killed before completion
	     contentType: 'application/json',
	     type: "POST",
	     dataType: "json",
	     data: JSON.stringify({ uuid: uuid
				  })
	   });
  }

  /**
   * control server side highlight support. This can be in three states:
   * (1) absent, in which case `cm.state.prologHighlightServer` is not
   * present, (2) disabled and (3) enabled.
   */
  CodeMirror.defineOption("prologHighlightServer", false, function(cm, val, old) {
    function leaveCM() { leaveEditor(cm); }

    if ( cm.state.prologHighlightServer ) {
      if ( val == null ) {		/* remove the highlight server */
	leaveEditor(cm);
	cm.off("change", changeEditor);
	window.removeEventListener("unload", leaveCM);
	delete cm.state.prologHighlightServer;
	cm.setOption("mode", {name:"prolog"});
      } else {
	if ( val.enabled != old.enabled ) {
	  cm.state.prologHighlightServer.enabled = val.enabled;
	  if ( val.enabled ) {		/* enable the highlight server */
	    cm.on("change", changeEditor);
	    window.addEventListener("unload", leaveCM);
	    if ( cm.lineCount() > 0 ) {
	      cm.serverAssistedHighlight(true);
	    }
	  } else {			/* disable */
	    leaveEditor(cm);
	    cm.off("change", changeEditor);
	    window.removeEventListener("unload", leaveCM);
	    cm.setOption("mode", {name:"prolog"});
	  }
	}
      }
    } else if ( val ) {			/* create for the first time */
      cm.state.prologHighlightServer = new State(val);
      if ( cm.state.prologHighlightServer.enabled ) {
	cm.on("change", changeEditor);
	window.addEventListener("unload", leaveCM);
	if ( cm.lineCount() > 0 ) {
	  cm.serverAssistedHighlight(true);
	}
      }
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
    var msg = {};

    state.tmo = null;

    if ( state == null || state.url == null || !state.enabled ||
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

    if ( state.uuid ) {			/* server has a mirror */
      msg.uuid = state.uuid;
      if ( state.changes == undefined ) {
	msg.text = cm.getValue();
	if ( msg.text.length > USE_CHANGES_IF_LONGER_THEN )
	  state.changes = [];
      } else {
	msg.changes = state.changes;
	state.changes = [];
      }
    } else {
      msg.text   = cm.getValue();
      if ( msg.text.trim() == "" )
	return;
      state.uuid = generateUUID();
      msg.uuid   = state.uuid;
    }
    msg.role = state.role;
    if ( typeof(state.sourceID) == "function" )
      msg.sourceID = state.sourceID();

    state.generationFromServer = cm.changeGeneration();
    $.ajax({ url: state.url.tokens,
	     dataType: "json",
	     contentType: 'application/json',
	     type: "POST",
	     data: JSON.stringify(msg),
	     success: function(data, status) {
	       var opts = modeOptions();
	       opts.metainfo = data.tokens;
	       cm.setOption("mode", opts);
	     },
	     error: function(jqXHR) {
	       if ( jqXHR.status == 409 ) {
		 delete state.uuid;
		 /* And refresh?  problem is this might get us into
		  * a loop.  We'd need some info from the server that
		  * this won't happen again
		  */
	       }
	     }
	   });
  }

  CodeMirror.commands.refreshHighlight = function(cm) {
    cm.serverAssistedHighlight(true);
  }

  function generateUUID() {
    var d = new Date().getTime();
    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
      .replace(/[xy]/g, function(c) {
        var r = (d + Math.random()*16)%16 | 0;
        d = Math.floor(d/16);
        return (c=='x' ? r : (r&0x7|0x8)).toString(16);
    });
    return uuid;
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
		     "pos-number": "number",
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
		     "uatom": "var",	/* var_prefix in effect */
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
		     "key": "atom",	/* TBD: or integer */
		     "sep": "atom",	/* : in dict */
		     "ext_quant": "atom", /* setof ^-symbol */

		     "expanded": "expanded",
		     "comment_string":"string",
		     "identifier": "atom",
		     "delimiter": "atom",
		     "module": "atom",

		     "constraint": "atom",

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

		     "string_terminal": "string",

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
      if ( oos.skippedTerms ) {

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

    /**
     * Test whether we are nested in quoted material.  That happens if
     * a newline appears in the quoted value, where our CodeMirror
     * tokeniser gives multiple "string", etc. tokens, while the server
     * only gives one, so we should not increment the server curToken.
     */
    function isQuoted(nesting) {
      var last = nesting.slice(-1)[0];
      return last && last.type == "quoted";
    }

    /**
     * Matches the server token `token` to the current token and updates
     * state.curToken and/or state.curTerm if the two matches.
     *
     * @param {Object} token is the next token from the server array
     * @param {Object} state is the mode state object
     * @returns {String|undefined} enriched style, the original style
     * or `undefined` if the mode token does not match the server token.
    */

    function matchToken(token, state) {
      if ( token ) {
	if ( syncOnType[type] ) {
	  if ( token.text && content ) {
	    if ( matchTokenText(token.text) ) {
	      state.curToken++;
	      return token.type;
	    }

	    return undefined;
	  } else if ( syncOnType[type] == serverSync[token.type] ) {
	    if ( type == "fullstop" ) {
	      state.curTerm++;
	      state.curToken = 0;
	    } else if ( !isQuoted(state.nesting) ) {
	      state.curToken++;
	    }
	    return token.type;
	  } else if ( syncOnType[type] == token.base ) {
	    state.curToken++;
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
	  } else if ( type == "pos-number" &&
		      token.text && token.text == "+" ) {
		/* HACK: A+1 is tokenised as "var" "pos-number" */
		/* But the server says "var" "atom" "number" */
		/* Needs operator logic to fix at the client */
	    state.curToken += 2;
	    return "number";
	  }
	} else if ( content && token.text == content ) {
	  state.curToken++;		/* ,; are not synced */
	  return token.type;
	} else {
	  return style;			/* not-synced client token */
	}
      }

      return undefined;
    }

    /* enrichStyle() body */

    if ( state.curTerm != null ) {
      var token;
      var serverStyle;

      //console.log(type,style,content,state);

      if ( state.syntax_error ) {		/* error state; recap after . */
	if ( type == "fullstop" ) {
	  parserConfig.editor.askRefresh();
	  delete state.syntax_error;
	}
	return style;
      }

      if ( state.outOfSync ) {			/* lost synchronization */
	var oos = state.outOfSync;

	if ( oos.skippedTerms <= 3 ) {
	  oos.skippedTokens.push({ type:    type,
			           style:   style,
				   content: content
			         });

	  if ( (serverStyle=reSync()) ) {
	    return serverStyle;			/* re-synchronized! */
	  } else if ( type == "fullstop" ) {
	    oos.skippedTokens = [];
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

      if ( (serverStyle=matchToken(token, state)) !== undefined ) {
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
      var terminfo;

      if ( state.metainfo && (terminfo=state.metainfo[token.state.curTerm]) )
	return terminfo[token.state.curToken-1];
    }

    return undefined;
  }

  /**
   * @param {Object} [elem] is the element that will be filled with
   * the token information.  If not provided, a `<span>` of class
   * `token-info` is returned.
   * @returns {Object} DOM object providing info that will be filled
   * later from the ajax call.
   */
  CodeMirror.prototype.tokenInfo = function(token, elem) {
    var state = this.state.prologHighlightServer;

    if ( !elem )
      elem = $($.el.span({class:"token-info"}, "..."));

    $.ajax({ url: state.url.info,
	     data: token,
	     success: function(data) {
	       elem.html(data);
	     }
           });

    return elem[0];
  }

  /**
   * @param {Object} token is an enriched token
   * @returns {Array(Object)} is an array of source references.
   */

  CodeMirror.prototype.getTokenReferences = function(token) {
    var result = [];

    function setFile(obj, from) {
      if ( from && from.indexOf("swish://") == 0 ) {
	obj.file = from.substring(8);
	return true;
      }
    }

    switch(token.type) {
      case "goal_local":
	var obj = {
	  title: "Source for "+token.text+"/"+token.arity,
	  line:  token.line,
	  regex: new RegExp("\\b"+RegExp.escape(token.text), "g"),
	  showAllMatches: true
	};
	setFile(obj, token.file);
	result.push(obj);
	break;
      case "file":
	var obj = {};

	if ( setFile(obj, token.path) ) {
	  obj.title = "Included file " + obj.file;
	  result.push(obj);
	}
        break;
    }

    return result;
  }
});
