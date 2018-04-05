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
 * Combine the SWISH components.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery",
	 "config",
	 "preferences",
	 "history",
	 "modal",
	 "chat",
	 "splitter",
	 "bootstrap",
	 "pane",
	 "tabbed",
	 "notebook",
	 "navbar",
	 "search",
	 "editor",
	 "query",
	 "runner",
	 "term",
	 "laconic",
	 "login",
	 "chatroom",
	 "d3",
	 "c3",
	 "svg-pan-zoom"
       ], function($, config, preferences, history, modal) {

preferences.setDefault("semantic-highlighting", true);
preferences.setDefault("emacs-keybinding", false);
preferences.setDefault("new-tab", true);
preferences.setDefault("preserve-state", true);
preferences.setInform("preserve-state", ".unloadable");

(function($) {
  var pluginName = 'swish';

  function glyph(name, func) {
    func.glyph = name;
    return func;
  }

  function icon(name, func) {
    func.typeIcon = name;
    return func;
  }

  var defaults = {
    menu: {
      "File":
      { "Save ...": glyph("cloud-upload", function() {
	  menuBroadcast("save", "as");
	}),
	"Info & history ...": glyph("info-sign", function() {
	  menuBroadcast("fileInfo");
	}),
	"Reload": glyph("refresh", function() {
	  menuBroadcast("reload");
	}),
	"Open recent": {
	  type: "submenu",
	  glyph: "paperclip",
	  action: function(ev) {
	    history.openRecent(ev, $(this).data('document'));
	  },
	  update: history.updateRecentUL
	},
	"Share": "--",
	"Follow ...": config.http.locations.follow_file_options ?
		      glyph("eye-open", function() {
	  menuBroadcast("follow-file");
	}) : undefined,
	"Start TogetherJS ...": icon("togetherjs", function() {
	  $("body").swish('collaborate');
	}),
	"Export": "--",
	"Download": glyph("floppy-save", function() {
	  menuBroadcast("download");
	}),
	"Print ...": glyph("print", function() {
	  menuBroadcast("print");
	})
      },
      "Edit":
      { "Clear messages": function() {
	  menuBroadcast("clearMessages");
	},
	"Changes": "--",
	"View changes": function() {
	  menuBroadcast("diff");
	},
	"Edit": "--",
	"Find (Ctrl-F)": function() {
	  menuBroadcast("edit-command", "find");
	},
	"Find and replace (Shift-Ctrl-F)": function() {
	  menuBroadcast("edit-command", "replace");
	},
	"Jump to line (Alt-G)": function() {
	  menuBroadcast("edit-command", "jumpToLine");
	},
	"Options": "--",
	"Semantic highlighting": {
	  preference: "semantic-highlighting",
	  type: "checkbox"
	},
	"Emacs Keybinding": {
	  preference: "emacs-keybinding",
	  type: "checkbox",
	  value: "false"
	},
	"Open document in new tab": {
	  preference: "new-tab",
	  type: "checkbox",
	  value: "true"
	},
	"Preserve state in browser": {
	  preference: "preserve-state",
	  type: "checkbox",
	  value: "true"
	}
      },
      "Examples": function(navbar, dropdown) {
	$("body").swish('populateExamples', navbar, dropdown);
      },
      "Help": function(navbar, dropdown) {
	$("body").swish('populateHelp', navbar, dropdown);
      }
    }
  }; // defaults;


  /** @lends $.fn.swish */
  var methods = {
    /**
     * Initialise SWISH on the page. At this moment, a page can only
     * contain one SWISH application and swish is normally initialised
     * on the body.  This might change.
     * @example $("body").swish();
     * {Object} options
     * {Boolean} options.show_beware If `true`, show a dialogue box
     * telling this is a limited version.
     */
    _init: function(options) {
      swishLogo();
      setupModal();
      setupPanes();
      setupResize();
      setupUnload();
      $("#search").search();

      options = options||{};
      this.addClass("swish");

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	$("#navbar").navbar(defaults.menu);
	$("#login").login();

	var  editor = $(".prolog-editor").prologEditor({save:true});
	data.runner = $(".prolog-runners").prologRunners();
	data.query  = $(".prolog-query").queryEditor(
          { source:   function() {
	      return elem.swish('prologSource');
	    },
	    sourceID: function() {
	      return editor.prologEditor('getSourceID');
	    },
	    examples: elem.swish('examples'),
	    runner:   data.runner,
	    editor:   editor[0]
	  });

	$(".notebook").notebook();

	if ( options.show_beware &&
	     !(swish.option && swish.option.show_beware == false) )
	  menuBroadcast("help", {file:"beware.html", notagain:"beware"});

	elem.data(pluginName, data);	/* store with element */

	if ( window.location.href.indexOf("&togetherjs=") > 0 )
	  elem.swish('collaborate');

	$("#chat").chat('');
	$("#broadcast-bell")
		.chatbell({
		  empty_title: "Click to open chat"
		});
	$("#chat-menu").on("click", "a", function(ev) {
	  var a = $(ev.target).closest("a");
	  switch ( a.data('action') ) {
	  case 'chat-shared':
	    $("body").swish('playFile', {
	      file: config.swish.hangout,
	      chat: 'large'
	    });
	    break;
	  case 'chat-about-file':
	    menuBroadcast("chat-about-file");
	  }
	});

	setInterval(function(){
	  $(".each-minute").trigger("minute");
	}, 60000);

	if ( elem[pluginName]('preserve_state') )
	  $(".unloadable").trigger("restore");
      });
    },

    /**
     * @return {Boolean} `true` when we should save and restore
     * the state to the browser local store.
     */
    preserve_state: function() {
      if ( swish.option.preserve_state == false )
	return false;
      if ( preferences.getVal("preserve-state") == false )
	return false;

      return true;
    },

    /**
     * Trigger a global event in SWISH.  Currently defined events are:
     *
     *   - `help`        -- show a modal help window
     *   - `source`      -- load a new source
     *   - `saveProgram` -- save the current program
     *
     * This method triggers all elements of class
     * `swish-event-receiver`.
     *
     * @param {String} name is the name of the trigger.
     * @param {Object|null} data provides additional data for the event.
     */
    trigger: function(name, data) {
      menuBroadcast(name, data);
      return this;
    },

    /**
     * Play a file from the webstore, loading it through ajax
     * @param {String|Object} options If a string, the name
     * of the file in the web storage
     * @param {String} options.file is the name of the file in the web
     * storage
     * @param {Number} [options.line] is the initial line number
     * @param {RegEx} [options.regex] search to highlight
     * @param {Boolean} [options.showAllMatches] Show other matches on
     * page.
     * @param {Boolean} [options.newTab] if `true`, open the file in
     * a new tab.
     * @param {Boolean} [options.noHistory] if `true`, do not push the
     * new document to the history.
     * @param {Object} [options.prompt] provided for trace events.  Must
     * be used to highlight the Prolog port at the indicated location.
     */
    playFile: function(options) {
      var elem = this;
      if ( typeof(options) == "string" )
	options = {file:options};

      var existing = this.find(".storage").storage('match', options);
      if ( existing && existing.storage('expose', "Already open") )
	return this;				/* FIXME: go to line */

      var url = config.http.locations.web_storage + options.file;
      $.ajax({ url: url,
	       type: "GET",
	       data: {format: "json"},
	       success: function(reply) {
		 reply.url = url;
		 reply.st_type = "gitty";

		 function copyAttrs(names) {
		   for(var i=0; i<names.length; i++) {
		     var name = names[i];
		     if ( options[name] )
		       reply[name] = options[name];
		   }
		 }

		 copyAttrs([ "line",
			     "regex", "showAllMatches",
			     "newTab", "noHistory",
			     "prompt", "chat"
			   ]);

		 elem.swish('setSource', reply);
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
	     });

      return this;
    },

    /**
     * Load file from a URL.  This fetches the data from the URL and
     * broadcasts a `"source"` event that is normally picked up by
     * the tabbed pane.
     * @param {Object}   options
     * @param {String}   options.url     URL to load.
     * @param {Integer} [options.line]   Line to go to.
     * @param {Regex}   [options.search] Text searched for.
     */
    playURL: function(options) {
      var elem = this;
      var existing = this.find(".storage").storage('match', options);

      if ( existing && existing.storage('expose', "Already open") )
	return this;				/* FIXME: go to line */

      $.ajax({ url: options.url,
	       type: "GET",
	       data: {format: "json"},
	       success: function(source) {
		 var msg;

		 if ( typeof(source) == "string" ) {
		   msg = { data: source };
		   msg.st_type = "external";
		 } else if ( typeof(source) == "object" &&
			     typeof(source.data) == "string" ) {
		   msg = source;
		   msg.st_type = "filesys";
		 } else {
		   alert("Invalid data");
		   return;
		 }

		 msg.url  = options.url;

		 function copyAttrs(names) {
		   for(var i=0; i<names.length; i++) {
		     var name = names[i];
		     if ( options[name] )
		       msg[name] = options[name];
		   }
		 }

		 copyAttrs([ "line",
			     "regex", "showAllMatches",
			     "newTab", "noHistory",
			     "prompt"
			   ]);

		 elem.swish('setSource', msg);
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
      });
    },

    /**
     * Open a source.  If we are in fullscreen mode and the current
     * object cannot be opened by the current fullscreen node, we
     * leave fullscreen mode.  Called by playFile and playURL.
     */
    setSource: function(src) {
      var st = this.swish('isFullscreen');

      if ( !(st && st.storage('setSource', src)) ) {
	if ( st )
	  this.swish('exitFullscreen');
	this.find(".tabbed").tabbed('tabFromSource', src);
      }

      return this;
    },


    /**
     * @param {Object} ex
     * @param {String} ex.title is the title of the example
     * @param {String} ex.file is the (file) name of the example
     * @param {String} ex.href is the URL from which to download the
     * program.
     * @returns {Function|String} function that loads an example
     */
    openExampleFunction: function(ex) {
      var swish = this;

      if ( ex.type == "divider" ) {
	return "--";
      } else if ( ex.type == "store" ) {
	return function() {
	  methods.playFile.call(swish, ex.file);
	};
      } else {
	return function() {
	  methods.playURL.call(swish, {url:ex.href});
	};
      }
    },

    /**
     * Populate the examples dropdown of the navigation bar. This
     * method is used by the navigation bar initialization.
     * @param {Object} navbar is the navigation bar
     * @param {Object} dropdown is the examples dropdown
     */
    populateExamples: function(navbar, dropdown) {
      var that = this;

      that.off("examples-changed")
	  .on("examples-changed", function() {
	     $("#navbar").navbar('clearDropdown', dropdown);
	     that.swish('populateExamples', navbar, dropdown);
	   });
      $.ajax(config.http.locations.swish_examples,
	     { dataType: "json",
	       success: function(data) {
		 for(var i=0; i<data.length; i++) {
		   var ex = data[i];
		   var title;
		   var options;

		   if ( ex == "--" || ex.type == "divider" ) {
		     title = "--";
		     options = "--";
		   } else {
		     var name = ex.file || ex.href;
		     title = ex.title;
		     options = that.swish('openExampleFunction', ex);
		     if ( name )
		       options.typeIcon = name.split('.').pop();
		   }

		   $("#navbar").navbar('extendDropdown', dropdown,
				       title, options);
		 }
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
	     });
      return this;
    },

    /**
     * Populate the help dropdown of the navigation bar. This
     * method is used by the navigation bar initialization.
     * @param {Object} navbar is the navigation bar
     * @param {Object} dropdown is the examples dropdown
     */
    populateHelp: function(navbar, dropdown) {
      var that = this;

      function openHelpFunction(help) {
	return function() {
	  menuBroadcast("help", {file:help.file});
	};
      }

      $.ajax(config.http.locations.swish_help_index,
	     { dataType: "json",
	       success: function(data) {
		 for(var i=0; i<data.length; i++) {
		   var help = data[i];
		   var title;
		   var options;

		   if ( help == "--" || help.type == "divider" ) {
		     title = "--";
		     options = "--";
		   } else {
		     var name = help.file;
		     title = help.title;
		     options = openHelpFunction(help);
		   }

		   $("#navbar").navbar('extendDropdown', dropdown,
				       title, options);
		 }
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
	     });
      return this;
    },


    /**
     * pick up all Prolog sources, preparing to execute a query. Currently
     * picks up:
     *
     *   - The `.text()` from all elements that match
     *   `".background.prolog.source"`
     *   - The source of the Prolog editor.  We need some notion of a
     *   _current_ Prolog editor.
     */
    prologSource: function() {
      var list = [];
      var src;

      if ( (src=$(".prolog-editor").prologEditor('getSource', "source")) )
	list.push(src);
      if ( (src=$(".background.prolog.source").text()) )
	list.push(src);

      return list.join("\n\n");
    },

    /**
     * Pick up all breakpoints.  Currently assumes a single source.
     * @param {String} pengineID is the pengine for which to set
     * the breakpoints.
     */
    breakpoints: function(pengineID) {
      return this.find(".prolog-editor")
                 .prologEditor('getBreakpoints', pengineID)||[];
    },

    /**
     * @param {Object} [options]
     * @param {Boolean} [options.active=false] If `true`, only return
     * info on the active tab
     */
    tabData: function(options) {
      options = options||{};
      if ( options.active ) {
	return this.find(".tab-pane.active .storage").storage('getData', options);
      } else {
	return this.find(".storage").storage('getData', options);
      }
    },

    /**
     * Extract examples from `$(".examples.prolog").text()`.  If this
     * does not exist, it returns a function that extracts the examples
     * from the current Prolog source editor.
     * @param {Boolean} [onlyglobal] if `true`, only extract globally
     * listed examples.
     * @returns {Array.String|null|Function}
     */
    examples: function(onlyglobal) {
      var text = $(".examples.prolog").text();

      if ( text ) {
	return $().prologEditor('getExamples', text, false);
      } else if ( onlyglobal != true ) {
	return function() {
	  return $(".prolog-editor").prologEditor('getExamples');
	};
      }
    },

    /**
     * Make DOM element fullscreen
     * @param {jQuery} node is the element to turn into fullscreen.
     * Currently this only works for a notebook.
     * @patam {jQuery} main is the node getting the `fullscreen
     * hamburger` class.
     */
    fullscreen: function(node, main) {
      var content = this.find(".container.tile-top");

      if ( !content.hasClass("fullscreen") ) {
	var data = this.data("fullscreen");
	if ( !data ) {
	  data = {};
	  this.data("fullscreen", data);
	}
	content.addClass("fullscreen");
	main = main||node;
	main.addClass("fullscreen hamburger");
	data.fullscreen_origin = node.parent()[0];
	data.fullscreen_main = main[0];
	$(content.children()[0]).hide();
	content.append(node);
	main.trigger('fullscreen', true);
      }

      return this;
    },

    /**
     * If some element is in fullscreen mode, revert
     * back to tabbed mode.
     * @return {Boolean} `true` if successful.
     */
    exitFullscreen: function() {
      var content = this.find(".container.tile-top");

      if ( content.hasClass("fullscreen") ) {
	var data = this.data("fullscreen");
	var node = $(content.children()[1]);
	var main = data.fullscreen_main;

	content.removeClass("fullscreen");
	$(data.fullscreen_main).removeClass("fullscreen hamburger");
	$(data.fullscreen_origin).append(node);
	data.fullscreen_origin = null;
	data.fullscreen_main = null;
	$(content.children()[0]).show();
	$(main).trigger('fullscreen', false);

	return true;
      }

      return false;
    },

    /**
     * Detect fullscreen mode
     * @return {jQuery} storage object that is running in fullscreen
     * mode.
     */
    isFullscreen: function() {
      var content = this.find(".container.tile-top");

      if ( content.hasClass("fullscreen") ) {
	var st = content.find(".storage");
	if ( st.length != 0 )
	  return st;
      }
    },

    /**
     * Open TogetherJS after lazy loading.
     */
    collaborate: function() {
      var elem = this;
      $(this).attr("data-end-togetherjs-html", "End collaboration");
      require([ "https://togetherjs.com/togetherjs-min.js"
	      ],
	      function() {
		TogetherJS(elem);
	      });
      return this;
    }
  }; // methods

  /**
   * General actions on SWISH are sent as triggers.  Any part of
   * the interface that is interested in events should add the class
   * `swish-event-receiver` and listen to the events in which it is
   * interested.
   */
  function menuBroadcast(event, data) {
    $(".swish-event-receiver").trigger(event, data);
  }

  /**
   * Turn elements with class `swish-logo` into the SWISH logo.
   */
  function swishLogo() {
    $(".swish-logo")
      .append($.el.b($.el.span({style:"color:darkblue"}, "SWI"),
		     $.el.span({style:"color:maroon"}, "SH")))
      .css("margin-left", "30px")
      .css("font-size", "24px")
      .addClass("navbar-brand");
  }

  /**
   * Setup modal actions.  Subsequently, modal dialogue windows
   * are opened by using the trigger `help`.
   * @example $("body").swish('action', 'help', {file:"about.html"});
   */
  function setupModal() {
    if ( $("#modal").length == 0 ) {
      $("body").append($.el.div({id:"modal"}));
      $("#modal").swishModal();
    }
  }

  /**
   * Setup the panes and allow for resizing them
   */
  function setupPanes() {
    $(".tile").tile();
    $(window).resize(function() { $(".tile").tile('resize'); });
    $(".tabbed").tabbed();
  }

  function setupResize() {
    $(window).resize(function() {
      $(".reactive-size").trigger('reactive-resize');
    });
  }

  function setupUnload() {
    $(window).bind("beforeunload", function(ev) {
      var rc;

      $(".unloadable").each(function() {
	var r = {};
	$(this).trigger("unload", r);
	rc = rc||r.rc;
      });

      return rc;
    });
  }

  /**
   * <Class description>
   *
   * @class swish
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.swish = function(method) {
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

}); // define()
