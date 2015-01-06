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
	 "jquery-ui",
	 "splitter",
	 "bootstrap",
	 "typeahead",
	 "pane",
	 "navbar",
	 "editor",
	 "query",
	 "runner",
	 "modal",
	 "term",
	 "laconic"
       ], function($, config, preferences) {

preferences.setDefault("semantic-highlighting", true);

(function($) {
  var pluginName = 'swish';

  var defaults = {
    newProgramText: "% Your program goes here\n\n\n\n"+
		     "/** <examples>\n\n\n"+
		     "*/",
    menu: {
      "File":
      { "New": function() {
	  menuBroadcast("source", { type: "new",
				    data: defaults.newProgramText
	                          });
	},
	"File group": "--",
	"Save": function() {
	  menuBroadcast("saveProgram");
	},
	"Save as ...": function() {
	  menuBroadcast("saveProgram", "as");
	},
	"Info ...": function() {
	  menuBroadcast("fileInfo");
	},
	"Share": "--",
	"Collaborate ...": function() {
	  $("body").swish('collaborate');
	},
	"Print group": "--",
	"Print ...": function() {
	  $(".prolog-editor").prologEditor('print');
	}
      },
      "Edit":
      { "Clear messages": function() {
	  menuBroadcast("clearMessages");
	},
	"Options": "--",
	"Semantic highlighting": {
	  preference: "semantic-highlighting",
	  type: "checkbox"
	}
      },
      "Examples": function(navbar, dropdown) {
	$("body").swish('populateExamples', navbar, dropdown);
      },
      "Help":
      { "About ...": function() {
	  menuBroadcast("help", {file:"about.html"});
	},
	"Topics": "--",
	"Help ...": function() {
	  menuBroadcast("help", {file:"help.html"});
	},
	"Runner ...": function() {
	  menuBroadcast("help", {file:"runner.html"});
	},
	"Background": "--",
	"Beware! ...": function() {
	  menuBroadcast("help", {file:"beware.html"});
	},
	"Caveats ...": function() {
	  menuBroadcast("help", {file:"caveats.html"});
	},
	"Background ...": function() {
	  menuBroadcast("help", {file:"background.html"});
	},
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
      setupSearch();

      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	$("#navbar").navbar(defaults.menu);

	data.editor = $(".prolog-editor").prologEditor();
	data.runner = $(".prolog-runners").prologRunners();
	data.query  = $(".prolog-query").queryEditor(
          { source:   function() {
	      return elem.swish('prologSource');
	    },
	    sourceID: function() {
	      return data.editor.prologEditor('getSourceID');
	    },
	    examples: elem.swish('examples'),
	    runner:   data.runner,
	  }).trigger("source");

	if ( options.show_beware )
	  menuBroadcast("help", {file:"beware.html", notagain:"beware"});

	elem.data(pluginName, data);	/* store with element */
      });
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
     * @param {String} ex is the name of the example
     * @returns {Function} function that loads an example
     */
    openExampleFunction: function(ex) {
      return function() {
	window.location = ex.href;
      };
    },

    /**
     * Populate the examples dropdown of the navigation bar. This
     * menthod is used by the navigation bar initialization.
     * @param {Object} navbar is the navigation bar
     * @param {Object} dropdown is the examples dropdown
     */
    populateExamples: function(navbar, dropdown) {
      var that = this;
      $.ajax(config.http.locations.swish_examples,
	     { dataType: "json",
	       success: function(data) {
		 for(var i=0; i<data.length; i++) {
		   $("#navbar").navbar('extendDropdown', dropdown,
				       data[i].title,
				       that.swish('openExampleFunction',
						  data[i]));
		 }
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

      if ( (src=$(".prolog-editor").prologEditor('getSource')) )
	list.push(src);
      if ( (src=$(".background.prolog.source").text()) )
	list.push(src);

      return list.join("\n\n");
    },

    /**
     * Extract examples from `$(".examples.prolog").text()`.  If this
     * does not exist, it returns a function that extracts the examples
     * from the current Prolog source editor.
     * @returns {Array.String|null|Function}
     */
    examples: function() {
      var text = $(".examples.prolog").text();

      if ( text )
	return $().prologEditor('getExamples', text, false);
      else
	return function() {
	  return $(".prolog-editor").prologEditor('getExamples');
	};
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
    $('body').on("click", "button.close-pane", function() {
      closePane($(this).parent());
    });
  }

  /**
   * Setup the search box.  We search for the following objects:
   *
   *   - Predicates (manual)		[TBD]
   *   - Source code (line)		[TBD]
   *   - Saved programs by		[TBD]
   *     - Name				[TBD]
   *     - Tag				[TBD]
   *     - Description			[TBD]
   */
  function setupSearch() {
    var elem = $("#search");

    var engine = new Bloodhound({
      name: "built-in",
      remote: config.http.locations.typeahead + "?q=%QUERY",
      datumTokenizer: function(d) {
	return Bloodhound.tokenizers.whitespace(d.name);
      },
      queryTokenizer: Bloodhound.tokenizers.whitespace
    });

    engine.initialize();

    elem.typeahead({ minLength: 1,
		     highlight: true
		   },
		   [ { name: "built-in",
		       displayKey: 'label',
		       source: engine.ttAdapter()
		     }
		   ])
	.on('typeahead:selected typeahead:autocompleted',
	    function(ev, datum, set) {
	      elem.data("target", {datum:datum, set:set});
	      console.log(elem.data("target"));
	    });

    elem.parents("form").submit(function(ev) {
      var data = elem.data("target");
      var str  = elem.val();

      if ( !(data && data.datum && data.datum.label == str) )
	data = str;

      elem.val("");
      elem.data("target", null);

      console.log(data);

      ev.preventDefault();
      return false;
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
