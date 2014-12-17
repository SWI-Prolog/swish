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

	 "cm/mode/prolog/prolog",
	 "cm/mode/prolog/prolog_keys",
	 "cm/mode/prolog/prolog_query",
	 "cm/mode/prolog/prolog_server",

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

         "jquery", "laconic"
       ],
       function(CodeMirror, config, preferences, form, templateHint) {

(function($) {
  var pluginName = 'prologEditor';

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
    _init: function(options) {

      return this.each(function() {
	var elem = $(this);
	var data = {};
	var ta;					/* textarea */

	options = $.extend({
	  role: "source",
	  placeholder: "Your Prolog program goes here ...",
	  lineNumbers: true,
	  mode: "prolog",
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
	}, options);

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

	if ( options.role != "query" )
	  options.continueComments = "Enter";

	if ( (ta=elem.children("textarea")[0]) ) {
	  var file = $(ta).attr("data-file");

	  if ( file )
	    data.file = file;
	  if ( window.swish && window.swish.meta_data )
	    data.meta = window.swish.meta_data;
	} else {
	  ta = $.el.textarea({placeholder:options.placeholder},
			     elem.text());
	  elem.append(ta);
	}

	data.cm              = CodeMirror.fromTextArea(ta, options);
	data.cleanGeneration = data.cm.changeGeneration();
	data.role            = options.role;

	elem.data(pluginName, data);
	elem.addClass("swish-event-receiver");
	elem.on("preference", function(ev, pref) {
	  elem.prologEditor('preference', pref);
	});

	if ( data.role == "source" ) {
	  elem.on("source", function(ev, src) {
	    elem.prologEditor('setSource', src);
	  });
	  elem.on("saveProgram", function(ev, data) {
	    elem.prologEditor('save', data);
	  });
	  elem.on("fileInfo", function() {
	    elem.prologEditor('info');
	  });
	  elem.on("source-error", function(ev, error) {
	    elem.prologEditor('highlightError', error);
	  });
	  elem.on("clearMessages", function(ev) {
	    elem.prologEditor('clearMessages');
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
      var elem = this;
      return elem.data(pluginName)[opt];
    },

    /**
     * @returns {String} current contents of the editor
     */
    getSource: function() {
      return this.data(pluginName).cm.getValue();
    },

    /**
     * @return {String|null} UUID of the source used for server-side
     * analysis
     */
     getSourceID: function() {
       var cm = this.data(pluginName).cm;

       if ( cm.state.prologHighlightServer ) {
	 return cm.state.prologHighlightServer.uuid;
       }
       return null;
     },

    /**
     * @param {String|Object} src becomes the new contents of the editor
     * @param {String} Object.data contains the data in the case that
     * `src` is an object.
     */
    setSource: function(src) {
      var options = this.data(pluginName);

      if ( typeof(src) == "string" )
	src = {data:src};

      this.data(pluginName).cm.setValue(src.data);

      if ( src.type == "new" ) {
	options.file = null;
	options.meta = null;
	updateHistory({url:config.http.locations.swish});
      }

      return this;
    },

    /**
     * Load document from the server.
     */
    load: function(file) {
      if ( file ) {
	var that = this;
	var options = this.data(pluginName);

	$.ajax({ url: config.http.locations.web_storage + "/" + file,
		 dataType: "text",
		 success: function(data) {
		   that.prologEditor('setSource', data);
		   options.file = file;
		 },
		 error: function(jqXHDR, textStatus) {
		   alert("Failed to load document: "+textStatus);
		 }
	       });
      }
      return this;
    },

    /**
     * Save the current document to the server
     * @param {Object} [meta] provides additional meta-information.
     * Currently defined fields are `author`, `email`,
     * `title`, `keywords` and `description`. Illegal fields are ignored
     * by the server.
     */
    save: function(meta) {
      var source  = this.prologEditor('getSource');
      var options = this.data(pluginName);
      var data    = { data: source, type: "pl" };
      var url     = config.http.locations.web_storage;
      var method  = "POST";

      if ( options.cm.isClean(options.cleanGeneration) ) {
	alert("No change");
	return this;
      }

      if ( meta )
	data.meta = meta;

      if ( options.file ) {
	url += "/" + encodeURI(options.file);
	method = "PUT";
      }

      $.ajax({ url: url,
               dataType: "json",
	       contentType: "application/json",
	       type: method,
	       data: JSON.stringify(data),
	       success: function(reply) {
		 if ( reply.error ) {
		   alert(JSON.stringify(reply));
		 } else {
		   options.url  = reply.url;
		   options.file = reply.file;
		   options.meta = reply.meta;
		   updateHistory(reply);
		 }
	       },
	       error: function() {
		 alert("Failed to save document");
	       }
	     });

      return this;
    },

    /**
     * Provide information about the current source in a modal
     * dialog.
     */
    info: function() {
      var options = this.data(pluginName);
      var meta = options.meta||{};

      form.showDialog({ title: "File info",
			body: function() {
			  this.append(form.fields.fileName(meta.name, meta.public));
			}
		      });

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
	       error: function() {
		 printWithIframe(pre);
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
     * Remove all inline messages from the editor
     */
    clearMessages: function() {
      return this.find(".source-msg").each(function() {
	$(this).data("cm-widget").clear();
      });
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
    }
  }; // methods

  function updateHistory(reply) {
    var cpath = window.location.pathname;

    if ( cpath != reply.url ) {
      window.history.pushState({location:reply.url},
			       "",
			       reply.url);
      document.title = "SWISH -- "+reply.file;
    }
  }

  window.onpopstate = function(e) {
    if ( e.state ) {
      if ( e.state.location ) {
	window.location =  e.state.location;
      }
    } else
      window.location.reload(true);
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
