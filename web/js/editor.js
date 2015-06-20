/**
 * @fileOverview
 * Prolog editor plugin based on [CodeMirror](http://codemirror.net)
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires codemirror
 */

define([ "cm/lib/codemirror",
	 "config",
	 "preferences",
	 "form",
	 "cm/mode/prolog/prolog-template-hint",
	 "modal",
	 "tabbed",

	 "storage",

	 "cm/mode/prolog/prolog",
	 "cm/mode/prolog/prolog_keys",
	 "cm/mode/prolog/prolog_query",
	 "cm/mode/prolog/prolog_server",

	 "cm/mode/markdown/markdown",

	 "cm/addon/edit/matchbrackets",
	 "cm/addon/comment/continuecomment",
	 "cm/addon/comment/comment",
	 "cm/addon/hint/show-hint",
	 "cm/addon/hint/anyword-hint",
	 "cm/addon/display/placeholder",
	 "cm/addon/runmode/runmode",

	 "cm/addon/hover/text-hover",
	 "cm/addon/hover/prolog-hover",

	 "cm/addon/hint/templates-hint",
	 "cm/addon/hint/show-context-info",

         "jquery", "laconic",

	 "cm/keymap/emacs",
       ],
       function(CodeMirror, config, preferences, form, templateHint,
		modal, tabbed) {

(function($) {
  var pluginName = 'prologEditor';

  var modeDefaults = {
    prolog: {
      mode: "prolog",
      role: "source",
      placeholder: "Your Prolog program goes here ...",
      lineNumbers: true,
      save: false,
      theme: "prolog",
      matchBrackets: true,
      textHover: true,
      prologKeys: true,
      extraKeys: {
	"Ctrl-Space": "autocomplete",
	"Alt-/": "autocomplete",
      },
      hintOptions: {
      hint: templateHint.getHints,
      completeSingle: false
      }
    },

    markdown: {
      mode: "markdown",
      placeholder: "Your markdown block goes here ...",
      lineWrapping: true,
      save: false
    }
  };

  var roleDefaults = {
    query: {
      mode: "prolog",
      role: "query",
      placeholder: "Your query goes here ...",
      lineNumbers: false,
      lineWrapping: true,
      save: false
    }
  };

  /** @lends $.fn.prologEditor */
  var methods = {
    /**
     * Initialize a Prolog editor.
     * @param {Object} [options]
     * @param {String} [options.role="source"] determines the role of
     * the editor. It is one of `source` or `query`.
     * @param {String} [options.placeholder="Your Prolog program goes here ..."]
     * sets the placeholder for the editor.
     * @param {Boolean} [options.lineNumbers=true] defines whether or
     * not a left-gutter with line numbers is displayed.
     * @param {Boolean} [options.save=false] defines whether the
     * editor responds to storage events.
     * @param {String} [options.mode="prolog"] defines the mode used by
     * CodeMirror.
     * @param {String} [options.theme="prolog"] defines the CSS used for
     * highlighting.
     * @param {Boolean} [options.matchBrackets=true] defines whether the
     * matching bracket is highlighted.
     * @param {Boolean} [options.prologKeys=true] defines whether "(",
     * ">" and ";" act as active keys to support if-then-else layout.
     * @param {Object} [options.extraKeys] specifies additional key
     * bindings.  Default is to bind "Ctrl-Space" and "Alt-/" to
     * "autocomplete".
     *
     */
    _init: function(opts) {

      return this.each(function() {
	var elem = $(this);
	var storage = {};		/* storage meta-data */
	var data = {};			/* our data */
	var ta;				/* textarea */

	opts      = opts||{};
	opts.mode = opts.mode||"prolog";

	options      = $.extend({}, modeDefaults[opts.mode]);
	if ( opts.role && roleDefaults[opts.role] )
	  options = $.extend(options, roleDefaults[opts.role]);
	options = $.extend(options, opts);

	if ( preferences.getVal("emacs-keybinding") )
	  options.keyMap = "emacs";

	if ( options.mode == "prolog" ) {
	  if ( config.http.locations.cm_highlight ) {
	    options.prologHighlightServer =
	    { url:  config.http.locations.cm_highlight,
	      role: options.role,
	      enabled: preferences.getVal("semantic-highlighting")
	    };
	    if ( options.sourceID )
	      options.prologHighlightServer.sourceID = options.sourceID;
	    options.extraKeys["Ctrl-R"] = "refreshHighlight";
	  }

	  if ( options.role == "source" ) {
	    options.continueComments = "Enter";
	    options.gutters = ["Prolog-breakpoints"]
	  }
	}

	if ( (ta=elem.children("textarea")[0]) ) {
	  function copyData(name) {
	    var value = $(ta).data(name);
	    if ( value ) storage[name] = value;
	  }

	  copyData("file");
	  copyData("title");
	  if ( window.swish && window.swish.meta_data )
	    storage.meta = window.swish.meta_data;
	} else {
	  ta = $.el.textarea({placeholder:options.placeholder},
			     options.value||elem.text());
	  elem.append(ta);
	}

	data.cm              = CodeMirror.fromTextArea(ta, options);
	data.role            = options.role;

	elem.data(pluginName, data);
	elem.addClass("swish-event-receiver");
	elem.on("preference", function(ev, pref) {
	  elem.prologEditor('preference', pref);
	});

	if ( options.save ) {
	  elem.prologEditor('setupStorage', storage);
	}

	if ( options.mode == "prolog" && data.role == "source" ) {
	  elem.on("activate-tab", function(ev) {
	    elem.prologEditor('makeCurrent');
	    data.cm.refresh();		/* needed if a tab has been opened */
	  });

	  elem.on("source-error", function(ev, error) {
	    elem.prologEditor('highlightError', error);
	  });
	  elem.on("trace-location", function(ev, prompt) {
	    elem.prologEditor('showTracePort', prompt);
	  });
	  elem.on("clearMessages", function(ev) {
	    elem.prologEditor('clearMessages');
	  });
	  data.cm.on("gutterClick", function(cm, n) {
	    var info = cm.lineInfo(n);

	    function makeMarker() {
	      return $("<span class=\"breakpoint-marker\">&#9679;</span>")[0];
	    }

	    if ( info.gutterMarkers )
	      cm.setGutterMarker(n, "Prolog-breakpoints", null);
	    else
	      cm.setGutterMarker(n, "Prolog-breakpoints", makeMarker());
	  });
	}
      });
    },

    /**
     * @example // Get the CodeMirror instance
     * $(element).prologEditor('getOption', 'cm');
     * @param {String} opt Name of option to fetch.
     * @return {*}
     */

    getOption: function(opt) {
      return this.data(pluginName)[opt];
    },

    /**
     * @example // Set the keybinding for the editor
     * $(element).prologEditor('setKeybinding', 'emacs') set
     * keybinding schema emacs.
     * @param {String} schema Name of the keybinding
     * return {*}
     */
    setKeybinding: function(schema) {
      schema = schema || "default";
      this.data(pluginName).cm.options.keyMap = schema;
    },

    /**
     * Get the defined breakpoints.
     * @returns {Array.integer} array of lines that need a breakpoint
     */
    getBreakpoints: function() {
      var breakpoints = [];
      var offset = 0;

      this.each(function() {
	var cm = $(this).data(pluginName).cm;
	var line = cm.firstLine();
	var last = cm.lastLine();

	for( ; line < last; line++ ) {
	  var info = cm.lineInfo(line);
	  if ( info.gutterMarkers )
	    breakpoints.push(offset+line+1);
	}
	offset += 2+last;			/* two newlines */
      });

      return breakpoints;
    },

    /**
     * FIXME: Add indication of the source, such that errors
     * can be relayed to the proper editor.
     * @returns {String} current contents of the editor.  If
     * the jQuery object holds multiple editors, we return the
     * joined content of the editors.
     */
    getSource: function() {
      var src = [];

      this.each(function() {
	src.push($(this).data(pluginName).cm.getValue());
      });

      return src.join("\n\n");
    },

    /**
     * @returns {Object} holding extended source information
     */
    getSourceEx: function() {
      var obj = { value: this.data(pluginName).cm.getValue()
		};
      var bps = this.prologEditor('getBreakpoints');
      if ( bps.length > 0 )
	obj.breakpoints = bps;

      return obj;
    },

    /**
     * @return {String[]} UUIDs of the sources used for
     * server-side analysis.  The array may contain `null`s
     * for sources that have no server side backup.
     */
     getSourceID: function() {
       var ids = [];

       this.each(function() {
	 var cm =  $(this).data(pluginName).cm;

	 if ( cm.state.prologHighlightServer )
	   ids.push(cm.state.prologHighlightServer.uuid);
	 else
	   ids.push(null);
       });

       return ids;
     },

    /**
     * @param {String} source sets the new content for the editor.  If
     * the editor is associated with a storage plugin, the call is
     * forwarded to the storage plugin.
     * @param {Boolean} [direct=false] if this parameter is `true`, the
     * message is never delegated to the storage
     */
    setSource: function(source, direct) {
      if ( this.data('storage') && direct != true ) {
	this.storage('setSource', source);
      } else {
	var data = this.data(pluginName);

	data.cm.setValue(source);
	if ( data.role == "source" ) {
	  $(".swish-event-receiver").trigger("program-loaded", this);
	}
      }
      return this;
    },

    /**
     * Advertise this editor as the current editor.  This is the
     * one used by the default query editor.
     */
    makeCurrent: function() {
      $(".swish-event-receiver").trigger("current-program", this);
      return this;
    },

    /**
     * print the current content of the editor after applying the
     * the CodeMirror mode to it.
     * @param {String} [src] Prolog source to print. Default is to print
     * the content of the editor.
     */
    print: function(src) {
      var pre = $.el.pre({class:"cm-s-prolog"});

      if ( !src ) src = this.prologEditor('getSource');

      CodeMirror.runMode(src, "prolog", pre);

      function printWithIframe(elem) {
	var iframe = $.el.iframe({src:"about:blank"});
	$("body").append(iframe);
	$("body", iframe.contentWindow.document).append(elem);
	iframe.contentWindow.print();
      }

      $.ajax({ url: "/swish/js/codemirror/theme/prolog.css",
	       dataType: "text",
	       success: function(data) {
		 printWithIframe($.el.div($.el.style(data),
					  pre));
	       },
	       error: function(jqXHDR) {
		 modal.ajaxError(jqXHR);
	       }
             });

      return this;
    },

    /**
     * Manage user preference changes.  Defines preferences are:
     *
     *   - "highlight" -- one of `semantic` or `syntactic`
     *
     * @param {Object} pref describes a preference
     * @param {String} pref.name name of the preference
     * @param {Any}    pref.value value of the preference
     */
    preference: function(pref) {
      var data = this.data(pluginName);

      if ( pref.name == "semantic-highlighting" ) {
	data.cm.setOption("prologHighlightServer",
			  { enabled: pref.value });
      }

      if ( pref.name == "emacs-keybinding") {
	if (pref.value == true) {
	  data.cm.setOption("keyMap", "emacs");
	} else {
	  data.cm.setOption("keyMap", "default");
	}
      }

      return this;
    },

    /**
     * Highlight a (syntax) error in the source.
     * @param {Object} error
     * @param {String} error.data contains the error message
     * @param {Object} error.location contains the location, providing
     * `line` and `ch` attributes.
     */
    highlightError: function(error) {
      var data = this.data(pluginName);
      var msg  = $(error.data).text();
      var left;

      if ( error.location.ch ) {
	left = data.cm.charCoords({ line: error.location.line-1,
				    ch:   error.location.ch
				  },
				  "local").left;
      } else {
	left = 0;
      }

      msg = msg.replace(/^.*?:[0-9][0-9]*: /, "");
      var elem = $.el.span({class:"source-msg error"},
			   msg,
			   $("<span>&times;</span>")[0]);
      $(elem).css("margin-left", left+"px");

      var widget = data.cm.addLineWidget(error.location.line-1, elem);

      $(elem).on("click", function() {
	widget.clear();
      });
      $(elem).data("cm-widget", widget);

      return this;
    },

    /**
     * Re-run the highlighting.  Used for query editors if the
     * associated editor has changed.
     */
    refreshHighlight: function() {
      var data = this.data(pluginName);
      data.cm.serverAssistedHighlight(true);
      return this;
    },

    /**
     * Remove all inline messages from the editor
     */
    clearMessages: function() {
      this.find(".source-msg").each(function() {
	$(this).data("cm-widget").clear();
      });

      this.prologEditor('showTracePort', null);

      return this;
    },

    /**
     * Highlight source events
     */
    showTracePort: function(prompt) {
      var data = this.data(pluginName);

      if ( data.traceMark ) {
	data.traceMark.clear();
	data.traceMark = null;
      }

      if ( prompt && prompt.source &&
	   prompt.source.file &&
	   prompt.source.file.startsWith("pengine://") ) {
	if ( prompt.source.from && prompt.source.to ) {
	  var from = data.cm.charOffsetToPos(prompt.source.from);
	  var to   = data.cm.charOffsetToPos(prompt.source.to);

	  if ( from && to ) {
	    data.traceMark = data.cm.markText(from, to,
					      { className: "trace "+prompt.port
					      });
	    data.cm.scrollIntoView(from, 50);
	  }
	}
      }

      return this;
    },

    /**
     * Extract example queries from text.  By   default,  this looks for
     * structured  comment  blocks  labelled   *examples*  and  extracts
     * fragments between `^ *?-` and `.`
     *
     * @param {String} [src] Source to parse. Default is the editor
     * content.
     * @param {Boolean} [inComment=true] if `true`, only process text
     * that is in an *examples* structured comment block
     * @returns {null|Array} Examples extracted from the source code.  If
     * there is _no source_ code, `null` is returned.
     */
    getExamples: function(src, inComment) {
      var source = src ? src : this.prologEditor('getSource');
      var comments;
      var exlist = [];

      if ( $.trim(source) == "" )
	return null;

      if ( inComment == false )
	comments = [src];
      else
	comments = source.match(/\/\*\* *<?examples>?[\s\S]*?\*\//igm);

      if ( comments ) {
	for(var i=0; i<comments.length; i++) {
	  var exl = comments[i].match(/^ *\?-[\s\S]*?[^-#$&*+./:<=>?@\\^~]\.\s/gm);

	  if ( exl ) {
	    for(var j=0; j<exl.length; j++) {
	      var ex = exl[j].replace(/^ *\?-\s*/, "")
			     .replace(/\s*$/, "");
	      exlist.push(ex);
	    }
	  }
	}
      }

      return exlist;
    },

    /**
     * @param {RegExp} re is the regular expression to search for
     * @param {Object} [options]
     * @param {number} [options.max] is the max number of hits to return
     * @returns {Array.object} list of objects holding the matching line
     * content and line number.
     */
    search: function(re, options) {
      var cm      = this.data(pluginName).cm;
      var start   = cm.firstLine();
      var end     = cm.lastLine();
      var matches = [];

      for(var i=start; i<=end; i++) {
	var line = cm.getLine(i);
	if ( line.search(re) >= 0 ) {
	  matches.push({line:i+1, text:line});
	  if ( options.max && options.max === matches.length )
	    return matches;
	}
      }

      return matches;
    },

    /**
     * Go to a given 1-based line number and optionally highlight the
     * match(es).
     *
     * @param {number} line
     * @param {Object} [options]
     * @param {RegExp} [options.regex] If provided, highlight the
     * matches.
     * @param {Boolean} [options.showAllMatches] if `true`, show all
     * matches in the viewport.
     */
    gotoLine: function(line, options) {
      var data = this.data(pluginName);
      var cm   = data.cm;
      var ch   = 0;
      var re;

      function clearSearchMarkers(cm) {
	if ( cm._searchMarkers !== undefined ) {
	  for(var i=0; i<cm._searchMarkers.length; i++)
	    cm._searchMarkers[i].clear();
	  cm.off("cursorActivity", clearSearchMarkers);
	}
	cm._searchMarkers = [];
      }

      line = line-1;
      re   = options.regex;
      clearSearchMarkers(cm);
      options = options||{};

      if ( re ) {
	ch = cm.getLine(line).search(re);
	if ( ch < 0 )
	  ch = 0;
      }

      cm.setCursor({line:line,ch:ch});
      var myHeight = cm.getScrollInfo().clientHeight;
      var coords = cm.charCoords({line: line, ch: 0}, "local");
      cm.scrollTo(null, (coords.top + coords.bottom - myHeight) / 2);

      if ( re ) {
	function markMatches(line, className) {
	  var match;

	  while( (match=re.exec(cm.getLine(line))) ) {
	    cm._searchMarkers.push(
	      cm.markText({line:line,ch:match.index},
			  {line:line,ch:match.index+match[0].length},
			  {className:className,
			   clearOnEnter: true,
			   clearWhenEmpty: true,
			   title: "Search match"
			  }));
	  }
	}

	markMatches(line, "CodeMirror-search-match");
	if ( options.showAllMatches ) {
	  var vp = cm.getViewport();

	  for(var i=vp.from; i<vp.to; i++) {
	    if ( i != line ) {
	      markMatches(i, "CodeMirror-search-alt-match");
	    }
	  }
	}

	if ( cm._searchMarkers.length > 0 )
	  cm.on("cursorActivity", clearSearchMarkers);
      }
    },

    /**
     * @return {Integer} change generation for this editor
     */
    changeGen: function() {
      return this.data(pluginName).cm.changeGeneration();
    },

    /**
     * Associate the editor with the server side (gitty) source
     */
    setupStorage: function(storage) {
      var data = this.data(pluginName);
      var elem = this;

      storage.setValue = function(source) {
	elem.prologEditor('setSource', source, true);
      };
      storage.getValue = function() {
	return data.cm.getValue();
      };
      storage.changeGen = function() {
	return data.cm.changeGeneration();
      };
      storage.isClean = function(generation) {
	return data.cm.isClean(generation);
      };

      storage.cleanGeneration = data.cm.changeGeneration();
      storage.cleanData       = data.cm.getValue();
      storage.cleanCheckpoint = "load";

      this.storage(storage);
      return this;
    }

  }; // methods

  tabbed.tabTypes.program = {
    dataType: "pl",
    label: "Program",
    create: function(dom) {
      $(dom).addClass("prolog-editor")
            .prologEditor({save:true})
	    .prologEditor('makeCurrent');
    }
  };

  /**
   * The prologEditor jQuery plugin converts a `<div>` into an code
   * editor based on [CodeMirror](http://codemirror.net)
   *
   * @class prologEditor
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @example // Create a default Prolog editor
   * $("#editor").prologEditor();
   * @example // Extract embedded examples
   * $("#editor").prologEditor('getExamples');
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.prologEditor = function(method) {
    if ( methods[method] ) {
      return methods[method]
	.apply(this, Array.prototype.slice.call(arguments, 1));
    } else if ( typeof method === 'object' || !method ) {
      return methods._init.apply(this, arguments);
    } else {
      $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
    }
  };
}(jQuery));

		 /*******************************
		 *	     FUNCTIONS		*
		 *******************************/

CodeMirror.prototype.charOffsetToPos = function(offset) {
  var line = this.firstLine();
  var last = this.lastLine();
  var charno = 0;

  for( ; line < last; line++ ) {
    var text = this.getLine(line);

    if ( charno <= offset && charno+text.length >= offset )
      return {line:line, ch:offset-charno};

    charno += text.length + 1;		/* one extra for the newline */
  }
};


		 /*******************************
		 *	      EMACS		*
		 *******************************/

CodeMirror.keyMap.emacs.Enter = "newlineAndIndent";


		 /*******************************
		 *	STYLE CONFIGURATION	*
		 *******************************/

/**
 * Include styles provided through the configuration object.
 *
 * @param {Object} style is an object mapping style names into style
 * properties.  The properties are also in an object, linking style
 * names to values.  For example:
 *
 *    ```
 *    { column: {color: "#8b008b},
 *      table:  {color: "#8b008b, "font-weight":"bold"}
 *    }
 *    ```
 */

function loadStyleExtensions(style, prefix)
{ var parts=[];

  prefix = prefix || "";

  parts.push("<style>\n");
  for(var sname in style) {
    if ( style.hasOwnProperty(sname) ) {
      var attrs = style[sname];

      parts.push(prefix, sname, "{");

      for(var a in attrs) {
	if ( attrs.hasOwnProperty(a) ) {
	  parts.push(a, ":", attrs[a], ";");
	}
      }

      parts.push("}\n");
    }
  }
  parts.push("</style>\n");

  $("body").append(parts.join(""));
}

if ( config.swish.cm_style )
  loadStyleExtensions(config.swish.cm_style,
		      ".cm-s-prolog span.cm-");
if ( config.swish.cm_hover_style )
  loadStyleExtensions(config.swish.cm_hover_style,
		      ".CodeMirror-hover-tooltip ");

}); // define
