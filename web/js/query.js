/**
 * @fileOverview
 * Provide the query editing facilities.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires laconic
 * @requires editor
 */

define([ "jquery", "config", "preferences", "cm/lib/codemirror",
	 "laconic", "editor"
       ],
       function($, config, preferences, CodeMirror) {

(function($) {
  var pluginName = 'queryEditor';

  var defaults = {
    maxHistoryLength: 50
  };

  /** @lends $.fn.queryEditor */
  var methods = {
    /**
     * @param {Object} options
     * @param {Runner} options.runner an object that understands
     * run(source, query), where source and query are strings.
     * @param {Array.String|Function} [options.examples] called to
     * populate the _Examples_ menu. Must return an array of strings.
     * @param {Integer} [options.maxHistoryLength=50] is the max number
     * of entries recalled by the history menu.
     */
    _init: function(options) {
      return this.each(function() {
	var elem   = $(this);
	var data   = $.extend({}, defaults, options);
	var qediv  = $.el.div({class:"query",style:"height:100%"});
	var tabled = tableCheckbox(data);

        var content =
	  $.el.table({class:"prolog-query"},
		     $.el.tr($.el.td({class:"prolog-prompt"},
				     "?-"),
			     $.el.td({colspan:2, style:"height:100%"},
				     qediv),
			     $.el.td()),
		     $.el.tr($.el.td(),
			     $.el.td({class:"buttons-left"},
				     examplesButton(data),
				     historyButton(data),
				     aggregateButton(data)),
			     $.el.td({class:"buttons-right"},
				     tabled,
				     runButton(data))));

	elem.addClass("prolog-query-editor swish-event-receiver");
	elem.append(content);

	function tableSelected() {
	  return $(tabled).find("input").prop("checked");
	}

	$(qediv).append(elem.children("textarea"))
	        .prologEditor({ role: "query",
				sourceID: function() {
				  return data.sourceID();
				},
				prologQuery: function(q) {
				  elem.queryEditor('run', q, tableSelected());
				}
		              });

	elem.data(pluginName, data);

	if ( !$(qediv).prologEditor('getSource', "query") )
	{ if ( typeof(data.examples) == "object" ) {
	    if ( data.examples[0] )
	      $(qediv).prologEditor('setSource', data.examples[0]);
	  } else {
	    elem[pluginName]('setProgramEditor', $(data.editor), true);
	  }
	}

	elem.on("current-program", function(ev, editor) {
	  elem[pluginName]('setProgramEditor', $(editor));
	  elem[pluginName]('setAggregates', $(editor));
	});
	elem.on("program-loaded", function(ev, editor) {
	  if ( $(data.editor).data('prologEditor') ==
	       $(editor).data('prologEditor') ) {
	    var exl = data.examples();
	    elem.queryEditor('setQuery', exl && exl[0] ? exl[0] : "");
	  }
	});
      });
    },
        
    setAggregates: function(editor) {
      var codeType = editor.prologEditor('getCodeType', "source");
      var but = this.find("button.aggregate");
      
      if (codeType == "lpad")
        but.prop('disabled', true);
      else
        but.prop('disabled', false);
        
      /*if (codeType == "lpad")
        but.css("visibility", "hidden");
      else
        but.css("visibility", "visible");*/
      
    },

    
    /**
     * @param {jQuery} editor has become the new current program
     * editor.  Update the examples and re-run the query highlighting.
     */
    setProgramEditor: function(editor, force) {
      var data = this.data(pluginName);

      if ( data.editor == editor[0] && !force )
	return this;

      data.editor = editor[0];
      if ( data.editor ) {
	data.examples = function() {
	  var exl    = editor.prologEditor('getExamples')||[];
	  var global = editor.parents(".swish").swish('examples', true)||[];

	  if ( $.isArray(global) )
	  exl.concat(global);

	  return exl;
	};
	if ( editor.prologEditor('isPengineSource') ) {
	  data.source = function() {
	    var src = editor.prologEditor('getSource', "source");
	    var bg  = $(".background.prolog.source").text();

	    if ( bg )
	      src += '\n\n' + bg;

	    return src;
	  };
	  data.codeType = editor.prologEditor('getCodeType', "source");
	} else {
	  data.source = "";
	  data.codeType = "undef";
	}
	data.sourceID = function() {
	  return editor.prologEditor('getSourceID');
	};

	var exl = data.examples();
	if ( exl && exl[0] && this.queryEditor('isClean') ) {
	  this.queryEditor('setQuery', exl[0]);
	} else {
	  editor.prologEditor('refreshHighlight');
	}
      } else
      { data.examples = "";
      }
    },

    /**
     * @returns {jQuery} the associated program editor
     */
    getProgramEditor: function() {
      var data = this.data(pluginName);

      if ( data.editor )
	return $(data.editor);
      else
	return $();
    },

    /**
     * @param {Array.String} set or extend the contents of the
     * _Examples_ menu.
     * @param {Boolean} [clear=true] clear the list before adding the
     * new examples.
     */
    setExamples: function(list, clear) {
      var ul = this.find("ul.examples");

      if ( !list ) list  = [];
      if ( clear === undefined ) clear = true;

      if ( clear && sameExamples(list) )
	return this;			/* no change */

      function sameExamples(exs) {
	var ex0;

	if ( (ex0=ul.data('examples')) &&
	     ex0.length == exs.length ) {
	  for(var i=0; i<ex0.length; i++) {
	    if ( ex0[i] != exs[i] )
	      return false;
	  }
	  return true;
	}
	return false;
      }

      if ( clear === true )
	ul.html("");
      for(var i=0; i<list.length; i++) {
	ul.append($.el.li($.el.a(list[i])));
      }
      ul.data('examples', list.slice(0));


      return this;
    },

    /**
     * Add a query to the history menu. If it is already part of the
     * menu, move it to the bottom.  If the menu exceeds the value
     * of the option `maxHistoryLength`, remove the oldest entry.
     * @param {String} query query to add to the history menu.
     */
    addHistory: function(query) {
      var ul   = this.find("ul.history");
      var data = this.data('queryEditor');

      function findInHistory() {
	return ul.children().filter(function() {
	  return $(this).text() == query;
	});
      }

      if ( query ) {
	var li;

	if ( (li=findInHistory()) )
	  li.remove();
	if ( ul.children().length >= data.maxHistoryLength )
	  ul.children().first().remove();
	ul.append($.el.li($.el.a(query)));
      }

      return this;
    },

    /**
     * Set the current query and focus the editor.
     * @param {String} query the new value of the query
     */
    setQuery: function(query) {
      var data = this.data(pluginName);

      data.cleanGen =
	this.find(".query")
	    .prologEditor('setSource', query)
	    .focus()
	    .prologEditor('changeGen');

      return this;
    },

    isClean: function() {
      var data = this.data(pluginName);

      return ( !this.queryEditor('getQuery') ||
	       ( data.cleanGen &&
		 this.find(".query").prologEditor('isClean', data.cleanGen)
	       )
	     );
    },

    /**
     * @returns {String} the current query as Prolog text
     */
    getQuery: function() {
      return this.find(".query").prologEditor('getSource', "query");
    },

    /**
     * Collect source and query and submit them to the associated
     * `runner`.
     *
     * @param {String} [q] is the query to execute.  Default asks it
     * from the associated query editor.
     * @param {Boolean} [tabled=false] when `true`, present the results
     * as a table.
     */
    run: function(q, tabled) {
      var data  = this.data('queryEditor');

      if ( q === undefined ) q = this.queryEditor('getQuery');
      q = $.trim(q);

      if ( !q ) {
	$(".swish-event-receiver").trigger("help", {file:"query.html"});
	return this;
      }
      $(".swish-event-receiver").trigger("clearMessages");

      var query = { query:q, editor: data.editor };
      if ( typeof(data.source) == "function" )
	query.source = data.source(q);
      else if ( typeof(data.source) == "string" )
	query.source = data.source;
      if ( tabled )
	query.tabled = true;

      console.log("query.run");
      console.log(data);
      console.log(query);
      query.codeType=data.codeType;
      
      this.queryEditor('addHistory', q);
      data.runner.prologRunners('run', query);

      return this;
    }
  }; // methods


		 /*******************************
		 *	PRIVATE FUNCTIONS	*
		 *******************************/

  /* Allow for e.g. Q(part).method(...)
  */

  function Q(from) {
    return $(from).parents(".prolog-query-editor");
  }

  function dropup(cls, label, options) {
    var dropup = $.el.div(
      {class:"btn-group dropup"},
      $.el.button(
	{class:"btn btn-default btn-xs dropdown-toggle "+cls,
	 "data-toggle":"dropdown"},
	label,
	$.el.span({class:"caret"})),
      $.el.ul({class:"dropdown-menu "+cls}));

    $(dropup).on("click", "a", function() {
      Q(this).queryEditor('setQuery', $(this).text());
    });

    return dropup;
  }

  function examplesButton(options) {
    var el = dropup("examples", "Examples", options);
    var ul = $(el).find("ul");
    
    function updateExamples(options) {
      var list = options.examples();

      if ( $.isArray(list) )
	Q(el).queryEditor('setExamples', list, true);
    }

    if ( typeof(options.examples) == "function" ) {
      $(el).mousedown(function(ev) {
			if ( ev.which == 1 ) {
			  updateExamples(options);
			}
		      });
    } else if ( options.examples ) {
      var list = options.examples;

      for(var i=0; i<list.length; i++) {
	ul.append($.el.li($.el.a(list[i])));
      }
    }

    return el;
  }

  function historyButton(options) {
    return dropup("history", "History", options);
  }

  function aggregateButton(options) {
    var cls = "aggregate";
    var list = options.aggregates ||
      [ "Aggregate (count all)",
	"--",
	"Order by",
	"Distinct",
	"Limit",
	"--",
	"Time",
	"Debug (trace)"
      ];
    var ul;

    var dropup = $.el.div(
      {class:"btn-group dropup aggregate"},
      $.el.button(
	{class:"btn btn-default btn-xs dropdown-toggle "+cls,
	 "data-toggle":"dropdown"},
	"Solutions",
	$.el.span({class:"caret"})),
      ul=$.el.ul({class:"dropdown-menu "+cls}));

    for(var i = 0; i<list.length; i++) {
      var wrap = list[i];

      if ( wrap == "--" )
	$(ul).append($.el.li({class:"divider"}));
      else
	$(ul).append($.el.li($.el.a(wrap)));
    }

    $(dropup).on("click", "a", function() {
      Q(this).find(".query").prologEditor('wrapSolution', $(this).text());
    });

    return dropup;
  }

  function runButton(options) {
    var button =
      $.el.button(
	{class:"run-btn-query",
	 class:"btn btn-default btn-primary btn-xs"
	},
	"Run!");

    $(button).on("click", function() {
      Q(this).queryEditor('run', undefined, tableSelected(this));
    });

    return button;
  }

  function tableSelected(from) {
    return $(from).parent().find("input").prop("checked");
  }

  function tableCheckbox(options) {
    var checked = preferences.getVal("tabled_results");
    var attr    = {type:"checkbox", name:"table"};

    if ( checked === undefined ) {
      checked = config.swish.tabled_results;
    }
    if ( checked )
      attr.checked = "checked";

    var input = $.el.input(attr);
    var checkbox = $.el.span({class:"run-chk-table"},
			     input, " table results");
    $(input).on("change", function(ev) {
      preferences.setVal("tabled_results",
			 $(ev.target).prop("checked"));
    });

    return checkbox;
  }

  /**
   * <Class description>
   *
   * @class queryEditor
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.queryEditor = function(method) {
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

});
