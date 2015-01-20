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

define([ "jquery", "laconic", "editor" ],
       function() {

(function($) {
  var pluginName = 'queryEditor';

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
	var data   = $.extend({maxHistoryLength: 50}, options);
	var qediv  = $.el.div({class:"query",style:"height:100%"});
	var tabled = tableCheckbox(options);

        var content =
	  $.el.table({class:"prolog-query"},
		     $.el.tr($.el.td({class:"prolog-prompt"},
				     "?-"),
			     $.el.td({colspan:2, style:"height:100%"},
				     qediv),
			     $.el.td()),
		     $.el.tr($.el.td(),
			     $.el.td({class:"buttons-left"},
				     examplesButton(options),
				     historyButton(options),
				     clearButton(options)),
			     $.el.td({class:"buttons-right"},
				     tabled,
				     runButton(options))));

	elem.addClass("prolog-query-editor swish-event-receiver");
	elem.append(content);

	function tableSelected() {
	  return $(tabled).find("input").prop("checked");
	}

	$(qediv).append(elem.children("textarea"))
	        .prologEditor({ role: "query",
				sourceID: options.sourceID,
		                placeholder: "Your query goes here ...",
				lineNumbers: false,
				lineWrapping: true,
				prologQuery: function(q) {
				  elem.queryEditor('run', q, tableSelected());
				}
		              });

	if ( typeof(options.examples) == "object" &&
	     options.examples[0] &&
	     !$(qediv).prologEditor('getSource') )
	  $(qediv).prologEditor('setSource', options.examples[0]);

	elem.on("source", function(src) {
	  if ( typeof(data.examples) == "function" ) {
	    exl = data.examples();
	    elem.queryEditor('setQuery', (exl && exl[0]) ? exl[0] : "");
	  }
	});

	elem.data(pluginName, data);
      });
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
      return this.find(".query")
	         .prologEditor('setSource', query)
		 .focus();
    },

    /**
     * @returns {String} the current query as Prolog text
     */
    getQuery: function() {
      return this.find(".query").prologEditor('getSource');
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

      var query = { query:q };
      if ( typeof(data.source) == "function" )
	query.source = data.source(q);
      else if ( typeof(data.source) == "string" )
	query.source = source;
      if ( tabled )
	query.tabled = true;

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

  function clearButton(options) {
    var button =
      $.el.button(
	{class:"clear-btn-query btn btn-default btn-xs"},
	"Clear");

    $(button).on("click", function() {
      Q(this).queryEditor('setQuery', "");
    });

    return button;
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
    var checkbox =
      $.el.span({class:"run-chk-table"},
		$.el.input({type:"checkbox", name:"table"}),
		" table results");

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
