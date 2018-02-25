/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2017, VU University Amsterdam
			      CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

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
	 "prolog",

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
	 "cm/addon/search/search",
	 "cm/addon/search/searchcursor",
	 "cm/addon/search/jump-to-line",
	 "cm/addon/dialog/dialog",

	 "cm/addon/hover/text-hover",
	 "cm/addon/hover/prolog-hover",

	 "cm/addon/hint/templates-hint",
	 "cm/addon/hint/show-context-info",

         "jquery", "laconic",

	 "cm/keymap/emacs",
       ],
       function(CodeMirror, config, preferences, form, templateHint,
		modal, tabbed, prolog) {

(function($) {
  var pluginName = 'prologEditor';

  var modeDefaults = {
    prolog: {
      mode: "prolog",
      role: "source",
      placeholder: "Your Prolog rules and facts go here ...",
      lineNumbers: true,
      autoCurrent: true,
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

  var lastEditor;

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

	var options = $.extend({}, modeDefaults[opts.mode]);
	if ( opts.role && roleDefaults[opts.role] )
	  options = $.extend(options, roleDefaults[opts.role]);
	options = $.extend(options, opts);

	if ( preferences.getVal("emacs-keybinding") )
	  options.keyMap = "emacs";

	if ( options.mode == "prolog" ) {
	  data.role = options.role;
	  if ( options.getSource )
	    data.getSource = options.getSource;

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

	  /*
	   * Long click detection and handling.
	   */
	  data.long_click = {};
	  function moveLongClick(ev) {
	    var lc = data.long_click;
	    var dx = ev.clientX - lc.clientX;
	    var dy = ev.clientY - lc.clientY;
	    if ( Math.sqrt(dx*dx+dy*dy) > 5 )
	      cancelLongClick();
	  }
	  function cancelLongClick() {
	    elem.off("mousemove", moveLongClick);
	    var lc = data.long_click;
	    if ( lc.timeout ) {
	      clearTimeout(lc.timeout);
	      lc.target  = undefined;
	      lc.timeout = undefined;
	    }
	  }

	  elem.on("mousedown", ".CodeMirror-code", function(ev) {
	    var lc = data.long_click;

	    lc.clientX = ev.clientX;
	    lc.clientY = ev.clientY;
	    elem.on("mousemove", moveLongClick);
	    data.long_click.timeout = setTimeout(function() {
	      cancelLongClick();
	      elem.prologEditor('contextAction');
	    }, 500);
	  });
	  elem.on("mouseup", function(ev) {
	    cancelLongClick();
	  });
	}

	/*
	 * Create CodeMirror
	 */
	if ( (ta=elem.children("textarea")[0]) ) {
	  function copyData(name) {
	    var value = $(ta).data(name);
	    if ( value ) {
	      storage[name] = value;
	    }
	  }

	  copyData("file");
	  copyData("url");
	  copyData("title");
	  copyData("meta");
	  copyData("st_type");
	  copyData("chats");

	  data.cm = CodeMirror.fromTextArea(ta, options);
	} else {
	  if ( !options.value )
	    options.value = elem.text();
	  data.cm = CodeMirror(elem[0], options);
	}

	elem.data(pluginName, data);
	elem.prologEditor('loadMode', options.mode);

	elem.addClass("swish-event-receiver");
	elem.addClass("prolog-editor");
	elem.on("preference", function(ev, pref) {
	  elem.prologEditor('preference', pref);
	});
	elem.on("print", function() {
	  if ( data.role != "query" )
	    elem.prologEditor('print');
	});
	elem.on("clearMessages", function(ev) {
	  elem.prologEditor('clearMessages');
	});
	elem.on("edit-command", function(ev, command) {
	  elem.prologEditor('execCommand', command);
	});
	data.cm.on("blur", function(ev) {
	  elem.prologEditor('execCommand', 'prepare');
	});

	if ( options.save ) {
	  storage.typeName = options.typeName||"program";
	  elem.prologEditor('setupStorage', storage);
	}

	if ( options.mode == "prolog" && data.role == "source" ) {
	  elem.on("activate-tab", function(ev) {
	    if ( options.autoCurrent )
	      elem.prologEditor('makeCurrent');
	    data.cm.refresh();		/* needed if a tab has been opened */
	  });

	  elem.on("source-error", function(ev, error) {
	    elem.prologEditor('highlightError', error);
	  });
	  elem.on("pengine-died", function(ev, id) {
	    if ( data.pengines ) {
	      var i = data.pengines.indexOf(id);
	      if ( i >= 0 )
		data.pengines.splice(i, 1);
	    }
	    if ( data.traceMark && data.traceMark.pengine == id ) {
	      data.traceMark.clear();
	      data.traceMark = null;
	    }
	  });
	  elem.on('addExample', function(ev, query) {
	    elem.prologEditor('addExample', query);
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
	} /* end if prolog source */

	data.cm.on("change", function(cm, change) {
	  var clean;

	  if ( change.origin == "setValue" ) {
	    clean = true;
	  } else {
	    var store = elem.data("storage");
	    var gen = store ? store.cleanGeneration : data.cleanGeneration;

	    clean = data.cm.isClean(gen);
	  }

	  elem.prologEditor('markClean', clean);
	});
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
     * Switch the editor to the requested mode, possibly by dynamically
     * loading the mode.  It seems that if we use RequireJS, we should
     * also use this for loading modes dynamically.
     */
    loadMode: function(mode) {
      var data = this.data(pluginName);

      if ( !CodeMirror.modes[mode] ) {
	require(["cm/mode/"+mode+"/"+mode],
		  function() {
		    data.cm.setOption("mode", mode);
		  });
      } else if ( mode != data.mode ) {
	data.cm.setOption("mode", mode);
      }

      return this;
    },

    /**
     * True if this source needs to be sent to the pengine.  This is
     * the case of the source is loaded.  We should also exclude module
     * files.  How do we detect a module file?  Detecting the module
     * header without support from Prolog is rather hard: count the
     * arity and ignore preceeding comments, encoding and conditional
     * compilation directives.
     */
    isPengineSource: function() {
      var data = $(this).data(pluginName);
      if ( data && data.role == "source" ) {
	var storageData = $(this).data('storage');

	if ( storageData && storageData.meta ) {
	  if ( storageData.meta.loaded ||
	       storageData.meta.module )
	    return false;
	}
      }

      return this;
    },

    /**
     * Get the defined breakpoints.
     * @param {String} pengineID is the pengine asking for the
     * breakpoints.
     * @returns {Array.Object} an array holding one object per source
     * with breakpoints.  The object contains `file` and `breakpoints`,
     * where the latter is an array of integers.
     */
    getBreakpoints: function(pengineID) {
      var result = [];

      this.each(function() {
	var data = $(this).data(pluginName);
	var breakpoints = [];
	var offset = 0;
	var cm = data.cm;
	var line = cm.firstLine();
	var last = cm.lastLine();

	for( ; line < last; line++ ) {
	  var info = cm.lineInfo(line);
	  if ( info.gutterMarkers )
	    breakpoints.push(offset+line+1);
	}

	if ( breakpoints.length > 0 ) {
	  var file;

	  if ( data.pengines && data.pengines.indexOf(pengineID) >= 0 ) {
	    file = "pengine://"+pengineID+"/src";
	  } else {
	    var store = $(this).data("storage");
	    if ( store )
	      file = "swish://"+store.file;
	  }

	  if ( file )
	    result.push({ file: file,
		          breakpoints: breakpoints
		        });
	}
      });

      return result;
    },

    /**
     * Get the source text from a set of editors.  Each source is
     * preceeded by a line :- '#file'(DocID, Line).
     *
     * @param {String} [role] Only return source for editors that
     * match the given role.
     * @param {Boolean} [direct] If `true`, do not try to indirect
     * over the `data.getSource` function.
     * @returns {String} current contents of the editor.  If
     * the jQuery object holds multiple editors, we return the
     * joined content of the editors.
     */
    getSource: function(role, direct) {
      var src = [];

      this.each(function() {
	if ( $(this).prologEditor('isPengineSource') ) {
	  var data = $(this).data(pluginName);

	  if ( data ) {
	    if ( !role || (role == data.role) ) {
	      var mysrc;
//TBD	      var docid = $(this).prologEditor('docid');

	      if ( typeof(data.getSource) == "function" && !direct ) {
		mysrc = data.getSource();
	      } else {
		mysrc = data.cm.getValue();
	      }
//TBD	      if ( role == "source" )
//TBD		src.push(":- '#file'("+Pengine.stringify(docid)+",1).");
	      src.push(mysrc);
	    }
	  }
	}
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

    /*
     * @returns {String} document identifier relating this editor to
     * the server side document store.
     */
    docid: function() {
      var st;

      if ( this.hasClass("storage") )
	return this.storage('docid');
      else if ( (st=this.closest(".storage")) && st.length > 0 ) {
	stdoc = st.storage('docid');
	if ( stdoc )
	  return stdoc + "#" + this.closest(".nb-cell").attr("name");
      }
    },

    /**
     * @return {String[]} UUIDs of the sources used for
     * server-side analysis.  The array may contain `null`s
     * for sources that have no server side backup.
     */
     getSourceID: function() {
       var ids = [];

       this.each(function() {
	 var data = $(this).data(pluginName);

	 if ( data && data.cm && data.cm.state.prologHighlightServer )
	   ids.push(data.cm.state.prologHighlightServer.uuid);
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
      if ( typeof(source) == "string" )
	source = {data:source};

      if ( this.data('storage') && direct != true ) {
	this.storage('setSource', source);
      } else {
	var data = this.data(pluginName);

	data.cm.setValue(source.data);
	if ( source.line || source.prompt ) {
	  data.cm.refresh();

	  if ( source.line ) {
	    this.prologEditor('gotoLine', source.line, source);
	  } else {
	    this.prologEditor('showTracePort', source.prompt);
	  }
	}

	if ( data.role == "source" ) {
	  $(".swish-event-receiver").trigger("program-loaded",
					     { editor: this,
					       query: source.query
					     });
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
     * Called if the editor changes from clean to dirty or visa versa.
     * This triggers `data-is-clean`, which is trapped by the tab to
     * indicate the changed state of the editor.
     */
    markClean: function(clean) {
      var data = this.data(pluginName);

      if ( data.clean_signalled != clean )
      { data.clean_signalled = clean;
	this.trigger("data-is-clean", clean);
      }
    },

    /**
     * Set notion of clean for editors that are not associated with a
     * storage
     */
    setIsClean: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName);
	data.cleanGeneration = data.cm.changeGeneration();
	data.clean_signalled = true;
      });
    },

    /**
     * @param {Object} options
     * @param {String} [options.add] Id of pengine to add
     * @param {String} [options.has] Match pengine, returning boolean
     */
    pengine: function(options) {
      var data = this.data(pluginName);

      if ( data ) {
	if ( options.add ) {
	  data.pengines = data.pengines || [];
	  if ( data.pengines.indexOf(options.add) < 0 )
	    data.pengines.push(options.add);

	  return this;
	} else if ( options.has ) {
	  return (data.pengines &&
		  data.pengines.indexOf(options.has) >= 0);
	}
      }
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

      $.ajax({ url: config.http.locations.swish+"js/codemirror/theme/prolog.css",
	       dataType: "text",
	       success: function(data) {
		 printWithIframe($.el.div($.el.style(data),
					  pre));
	       },
	       error: function(jqXHDR) {
		 modal.ajaxError(jqXHDR);
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
     * @param {String} error.data contains the error message (HTML
     * string)
     * @param {Object} error.location contains the location, providing
     * `line` and `ch` attributes.
     */
    highlightError: function(error) {
      if ( error.location.file &&
	   (error.location.file == true ||
	    this.prologEditor('isMyFile', error.location.file)) ) {
	var data = this.data(pluginName);
	var chmark;

	if ( error.location.ch ) {
	  left = data.cm.charCoords({ line: error.location.line-1,
				      ch:   error.location.ch-1
				    },
				    "local").left;
	  chmark = $.el.div({class:"source-msg-charmark"},
			    $.el.span({class:"glyphicon glyphicon-chevron-up"}));
	  $(chmark).css("padding-left", left+"px");
	}

	var elem = $.el.div({ class:"source-msg error error-context",
			      title:"Error message.  Click to remove"
			    },
			    chmark,
			    $(error.data)[0],
			    $.el.span({class:"glyphicon glyphicon-remove-circle"}));
	var widget = data.cm.addLineWidget(error.location.line-1, elem);

	if ( error.error_context )
	  $(elem).data("error_context", error.error_context);
	$(elem).on("click", function(ev) {
	  if ( error.error_handler &&
	       error.error_handler(ev) == false )
	    return;
	  widget.clear();
	});
	$(elem).data("cm-widget", widget);
      }

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
     * Refresh the editor.  This is often needed if it is resized.
     */
    refresh: function() {
      var data = this.data(pluginName);
      if ( data )
	data.cm.refresh();
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
     * Execute a command on the editor from the menu.  The trick is to
     * find the current editor.  For that purpose we make "blur" trigger
     * the 'prepare' command that sets the last editor.  On the
     * following menu action we execute on the last editor.
     */
   execCommand: function(command) {
     if ( command == 'prepare' ) {
       lastEditor = this[0];
     } else if ( lastEditor == this[0] ) {
       elem = $(lastEditor);
       var data = elem.data(pluginName);
       data.cm.execCommand(command);
       elem.find(".Codemirror-dialog input").focus();
     }

     return this;
   },

    /**
     * @param {String} file is the file as known to Prolog,
     * which is `pengine://<pengine>/src/` for the pengine main file
     * and `swish://store.pl` for included files.
     * @return {Boolean} whether or not this is my file.
     */
    isMyFile: function(file) {
      var prefix = "swish://";

      if ( file.startsWith("pengine://") ) {
	var data = this.data(pluginName);

	if ( data.pengines &&
	     (id = file.split("/")[2]) &&
	     data.pengines.indexOf(id) >= 0 )
	  return true;
      }

      if ( file.startsWith(prefix) ) {
	var store = this.data("storage");

	if ( store && file.slice(prefix.length) == store.file )
	  return true;
      }

      return false;
    },

    /**
     * Highlight source events.  The source pengine gets a prompt
     * with `prompt.file` set to `pengine://<id>/src`.
     * @param {Object|null} prompt for a tracer action.  Use `null`
     * to clear.
     * @return {jQuery|undefined} `this` if successful.  `undefined`
     * if this is a valid trace event, but I cannot process it.
     */
    showTracePort: function(prompt) {
      if ( this.length == 0 )
	return this;

      var data  = this.data(pluginName);

      if ( data.traceMark ) {
	data.traceMark.clear();
	data.traceMark = null;
      }

      if ( prompt && prompt.source && prompt.source.file ) {
	var file  = prompt.source.file;

	if ( this.prologEditor('isMyFile', file) ) {
	  if ( prompt.source.from && prompt.source.to ) {
	    var from = data.cm.charOffsetToPos(prompt.source.from);
	    var to   = data.cm.charOffsetToPos(prompt.source.to);

	    if ( !this.is(":visible") )
	      this.storage('expose', "trace");

	    if ( from && to ) {
	      data.traceMark = data.cm.markText(from, to,
						{ className: "trace "+prompt.port
						});
	      data.traceMark.pengine = prompt.pengine;
	      data.cm.scrollIntoView(from, 50);
	    }
	  }

	  return this;
	}
      } else {
	return this;
      }
    },

    /**
     * Get the selection for later reuse.
     * @returns {null|Array} Array of selection descriptions for each
     * editor in the jQuery object that has a selection.  Each editor
     * selection contains `editor` and `selections`, where `selections`
     * is an array of objects with `from`, `to` (line,ch), `string` and
     * `context`.  The latter two allow for fuzzy restoration of the
     * selection.
     */
    getSelection: function() {
      var selection = [];

      this.each(function() {
	var ed   = $(this);
	var data = ed.data(pluginName);

	if ( data.cm.somethingSelected() == true ) {
	  var sel    = data.cm.listSelections();
	  var esel   = {selections:[]};

						/* Hack */
	  var cell_name = ed.closest(".nb-cell").attr("name");
	  if ( cell_name )
	    esel.cell = cell_name;

	  for(var i=0; i<sel.length; i++) {
	    var s = sel[i];
	    var sr = {};

	    function cmploc(l1, l2) {
	      if ( l1.line < l2.line ) return -1;
	      if ( l1.line > l2.line ) return  1;
	      if ( l1.ch   < l2.ch   ) return -1;
	      if ( l1.ch   > l2.ch   ) return  1;
	      return 0;
	    }
	    function sol(pos) {
	      return {line:pos.line, ch:0};
	    }
	    function eol(pos) {
	      return {line:pos.line, ch:data.cm.getLine(pos.line).length};
	    }
	    function cppos(pos) {
	      return {line:pos.line, ch:pos.ch};
	    }

	    switch ( cmploc(s.anchor, s.head) )
	    { case -1:
		sr.from = cppos(s.anchor);
	        sr.to   = cppos(s.head);
		break;
	      case 1:
		sr.to   = cppos(s.anchor);
	        sr.from = cppos(s.head);
		break;
	      case 0:
		continue;
	    }

	    sr.string  = data.cm.getRange(sr.from, sr.to);
	    sr.context = data.cm.getRange(sol(sr.from), eol(sr.to));

	    esel.selections.push(sr);
	  }

	  selection.push(esel);
	}
      });

      return selection.length > 0 ? selection : null;
    },

    /**
     * @param {Array} sel is the selection to restore
     * @fixme deal with notebook selections
     */
    restoreSelection: function(selection) {
      function restoreEditorSelection(ed, sel) {
	var data    = ed.data(pluginName);
	var cm      = data.cm;
	var cmsel   = [];
	var loffset = "";

	function findsel(s) {
	  if ( cm.getRange(s.from, s.to) == s.string ) {
	    return {anchor:s.from, head: s.to};
	  } else {
	    var start   = cm.firstLine();
	    var end     = cm.lastLine();
	    var offset  = 0;
	    var goffset = 1;

	    function contextMatch(l0, s) {
	      var lines = s.split("\n");
	      for(var i=0; i<lines.length; i++) {
		if ( cm.getLine(l0+i) != lines[i] )
		  return false;
	      }
	      return true;
	    }

	    function stringMatch(l0, s) {
	      var lines = s.split("\n");
	      for(var i=0; i<lines.length; i++) {
		var cml = cm.getLine(l0);
		var   l = lines[i];
		var choff;

		if ( i == 0 ) {
		  if ( i == lines.length-1 ) {
		    return cml.indexOf(l);
		  } else {
		    choff = cml.indexOf(l);

		    if ( !(choff >= 0 && l.length+choff == cml.length) )
		      return -1;
		  }
		} else if ( i == lines.length-1 ) {
		  if ( cml.indexOf(l) != 0 )
		    return -1;
		} else {
		  if ( cm.getLine(l0+i) != lines[i] )
		    return -1;
		}
	      }
	      return choff;			/* ch of selection start */
	    }

	    function poff(p, l, ch) {
	      ch = ch||0;
	      return {line:p.line+l, ch:p.ch+ch};
	    }

	    while( s.from.line+offset >= start &&
		   s.to.line+offset <= end ) {
	      var ch;

	      if ( contextMatch(s.from.line+offset, s.context) )
		return { anchor:poff(s.from, offset),
			 head:  poff(s.to,   offset),
			 offset:offset
		       };
	      if ( (ch=stringMatch(s.from.line+offset, s.string)) >= 0 ) {
		var soff = ch-s.from.ch;
		var toff = s.to.line != s.from.line ? 0 : soff;

		return { anchor:poff(s.from, offset, soff),
			 head:  poff(s.to,   offset, toff),
			 offset:offset
		       };
	      }

	      goffset++;
	      offset = Math.floor(goffset/2);
	      if ( goffset%2 == 1 )
		offset = -offset;
	    }
	  }
	}

	for(var i=0; i<sel.length; i++) {
	  var r = findsel(sel[i]);
	  if ( r ) {
	    cmsel.push(r);
	    if ( r.offset ) {
	      if ( loffset != "" )
		loffset += ";";
	      loffset += r.offset
	    }
	  }
	}

	if ( cmsel.length > 0 )
	  cm.setSelections(cmsel, 0);
	if ( loffset != "" || cmsel.length < sel.length ) {
	  var msg;

	  if ( cmsel.length == sel.length )
	    msg = "Found selections at offset "+loffset;
	  else if ( loffset == "" )
	    msg = "Could not restore all selections";
	  else
	    msg = "Only found some selections at offsets " + loffset;

	  modal.feedback({ html: msg, owner: ed });
	}
      }

      // Our body
      if ( selection[0].cell ) {	/* notebook style */
	var editors = this;

	for(var i=0; i<selection.length; i++) {
	  var s = selection[i];

	  function findEditor(name) {
	    for(var i=0; i<editors.length; i++) {
	      if ( $(editors[i]).closest(".nb-cell").attr("name") == name )
		return $(editors[i]);
	    }
	  }

	  var ed = findEditor(s.cell);
	  if ( ed )
	    restoreEditorSelection(ed, s.selections);
	}
      } else {				/* plain editor */
	restoreEditorSelection(this, selection);
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
     * Add an example to the examples comment block or create such a
     * block.
     */
    addExample: function(query) {
      var cm      = this.data(pluginName).cm;
      var source = this.prologEditor('getSource');
      var start  = this.prologEditor('search',
				     /\/\*\* *<?examples>?/igm, {dir:-1,max:1});
      var end    = this.prologEditor('search', /\*\//, {start:start.line,max:1});

      if ( start.length == 1 )
      { var end = this.prologEditor('search', /\*\//,
				    {start:start[0].line, max:1});
	var current = this.prologEditor('getExamples', source);

	if ( current && current.indexOf(query) != -1 )
	{ modal.alert("Query is already in examples");
	  return this;
	}
	if ( end.length != 1 )
	{ modal.alert("/** <examples> block is not terminated with */");
	  return this;
	}

	query = query.split("\n").join("\n   ");
	cm.setSelection({line:end[0].line-1, ch:0});
	cm.replaceSelection("?- "+query+"\n");
      } else
      { cm.setSelection({line:cm.lastLine(), ch:0});
	cm.replaceSelection("/** <examples>\n" +
			    "?- "+query+"\n" +
			    "*/\n");
      }

      return this;
    },

    /**
     * @param {RegExp} re is the regular expression to search for
     * @param {Object} [options]
     * @param {number} [options.max] is the max number of hits to return
     * @param {number} [options.dir=1] is -1 to search backwards
     * @param {number} [options.start] to start at a given line
     * @param {number} [options.end] to end at a given line
     * @returns {Array.object} list of objects holding the matching line
     * content and line number.
     */
    search: function(re, options) {
      var cm      = this.data(pluginName).cm;
      var dir     = options.dir||1;
      var start   = options.start == undefined ? cm.firstLine() : options.start;
      var end     = options.end   == undefined ? cm.lastLine()  : options.end;
      var matches = [];

      if ( (dir == -1 && end > start) ||
	   (dir == 1 && start > end) )
      { var tmp = start;
	start = end;
	end = tmp;
      }

      if ( (dir > 0 && start > end) ||
	   (dir < 0 && start < end) )
	return matches;
      end += dir;

      for(var i=start; i!=end; i+=dir) {
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

      clearSearchMarkers(cm);
      options = options||{};
      re      = options.regex;
      line    = line-1;

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
      } else {					/* mark entire line */
	cm._searchMarkers.push(
	      cm.markText({line:line, ch:0},
			  {line:line, ch:cm.getLine(line).length},
			  {className:"CodeMirror-search-match",
			   clearOnEnter: true,
			   clearWhenEmpty: true,
			   title: "Target line"
			  }));
      }

      return this;
    },

    /**
     * @return {Integer} change generation for this editor
     */
    changeGen: function() {
      return this.data(pluginName).cm.changeGeneration();
    },

    isClean: function(gen) {
      return this.data(pluginName).cm.isClean(gen);
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
      storage.markClean = function(clean) {
	elem.prologEditor('markClean', clean);
      };

      storage.cleanGeneration = data.cm.changeGeneration();
      storage.cleanData       = data.cm.getValue();
      storage.cleanCheckpoint = "load";

      this.storage(storage);
      return this;
    },

    /**
     * Act on the current token.  Normally invoked after a long click.
     */
    contextAction: function() {
      var elem  = this;
      var data  = this.data(pluginName);
      var here  = data.cm.getCursor();
      var token = data.cm.getTokenAt(here, true);
      var et    = data.cm.getEnrichedToken(token);
      var locations = data.cm.getTokenReferences(et);

      if ( locations && locations.length > 0 ) {
	var ul = $.el.ul();
	var select  = $.el.div({class: "goto-source"}, $.el.div("Go to"), ul);
	var modalel = $.el.div({class: "edit-modal"},
			       $.el.div({class: "mask"}),
			       select)

	for(var i=0; i<locations.length; i++) {
	  var loc = locations[i];
	  $(ul).append($.el.li($.el.a({'data-locindex':i}, loc.title)));
	}

	var coord = data.cm.cursorCoords(true);
	$(select).css({top: coord.bottom, left: coord.left});

	$("body").append(modalel);
	$(modalel).on("click", function(ev) {
	  var i = $(ev.target).data('locindex');
	  $(modalel).remove();

	  if ( i !== undefined ) {
	    var loc = locations[i];

	    if ( loc.file ) {
	      elem.closest(".swish").swish('playFile', loc);
	    } else {
	      var editor;

	      // If we are the query editor, we must find the related
	      // program editor.
	      if ( data.role == "query" ) {
		editor = elem.closest(".prolog-query-editor")
			     .queryEditor('getProgramEditor');

		if ( !editor[0] )
		  modal.alert("No related program editor");
	      } else
	      { editor = elem;
	      }

	      if ( editor && editor[0] )
		editor.prologEditor('gotoLine', loc.line, loc).focus();
	    }

	  }
	});

	$(modalel).show();
      }

      return this;
    },

		 /*******************************
		 *	QUERY MANIPULATION	*
		 *******************************/

    /**
     * @param {String} [query] query to get the variables from
     * @param {Boolean} [anon] if `true`, also include _X variables.
     * @return {List.string} is a list of Prolog variables without
     * duplicates
     */

    variables: function(query, anon) {
      var qspan = $.el.span({class:"query cm-s-prolog"});
      var vars = [];

      CodeMirror.runMode(query, "prolog", qspan);

      function addVars(selector) {
	$(qspan).find(selector).each(function() {
	  var name = $(this).text();
	  if ( vars.indexOf(name) < 0 )
	    vars.push(name);
	});
      }

      addVars("span.cm-var");
      if ( anon )
	addVars("span.cm-var-2");

      return vars;
    },

    /**
     * Wrap current query in a solution modifier.
     * TBD: If there is a selection, only wrap the selection
     *
     * @param {String} wrapper defines the type of wrapper to use.
     */
    wrapSolution: function(wrapper) {
      var query = prolog.trimFullStop(this.prologEditor('getSource', "query"));
      var that = this;
      var vars = this.prologEditor('variables', query);

      function wrapQuery(pre, post) {
	that.prologEditor('setSource', pre + "("+query+")" + post + ".")
	    .focus();
	return that;
      }

      function prefixQuery(pre) {
	that.prologEditor('setSource', pre + query + ".")
	    .focus();
	return that;
      }

      function order(l) {
	var order = [];
	for(var i=0; i<vars.length; i++)
	  order.push("asc("+vars[i]+")");
	return order.join(",");
      }

      switch ( wrapper ) {
        case "Aggregate (count all)":
	  return wrapQuery("aggregate_all(count, ", ", Count)");
	case "Projection":
	  return prefixQuery("projection(["+vars.join(",")+"]),\n");
        case "Order by":
	  return wrapQuery("order_by(["+order(vars)+"], ", ")");
        case "Distinct":
	  return wrapQuery("distinct(["+vars.join(",")+"], ", ")");
        case "Limit":
	  return wrapQuery("limit(10, ", ")");
        case "Time":
	  return wrapQuery("time(", ")");
        case "Debug (trace)":
	  return wrapQuery("trace, ", "");
	default:
	  alert("Unknown wrapper: \""+wrapper+"\"");
      }
    }
  }; // methods

  tabbed.tabTypes.program = {
    dataType: "pl",
    typeName: "program",
    label: "Program",
    contentType: "text/x-prolog",
    order: 100,
    create: function(dom, options) {
      $(dom).addClass("prolog-editor")
            .prologEditor($.extend({save:true}, options))
	    .prologEditor('makeCurrent');
    }
  };

  if ( config.swish.tab_types ) {
    var editDefaults = {
      save: true,
      lineNumbers: true
    };

    for(var i=0; i<config.swish.tab_types.length; i++) {
      var tabType = config.swish.tab_types[i];
      if ( tabType.editor ) {
	var options = $.extend({typeName:tabType.typeName},
			       editDefaults,
			       tabType.editor);

	tabType.create = function(dom) {
	  $(dom).addClass("prolog-editor")
	        .prologEditor(options);
	};

	tabbed.tabTypes[tabType.typeName] = tabType;
      }
    }
  }


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
