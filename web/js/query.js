/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2018, VU University Amsterdam
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
 * Provide the query editing facilities.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires laconic
 * @requires editor
 */

define([ "jquery", "config", "preferences", "cm/lib/codemirror", "modal",
	 "utils",
	 "laconic", "editor"
       ],
       function($, config, preferences, CodeMirror, modal, utils) {

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
	var qediv  = $.el.div({class:"query"});
	var tabled = tableCheckbox(data);

	elem.addClass("prolog-query-editor swish-event-receiver reactive-size " +
		      "unloadable");

	elem.append(qediv,
		    $.el.div({class:"prolog-prompt"}, "?-"),
		    $.el.div({class:"query-buttons"},
			     $.el.span({class:"buttons-left"},
				       examplesButton(data),
				       historyButton(data),
				       aggregateButton(data)),
			     $.el.span({class:"buttons-right"},
				       tabled,
				       runButton(data))));

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
	});
	elem.on("program-loaded", function(ev, options) {
	  var query = options.query;

	  if ( query != null ) {		/* null: keep */
	    if ( query == undefined ) {
	      if ( $(data.editor).data('prologEditor') ==
		   $(options.editor).data('prologEditor') ) {
		var exl = data.examples();
		query = exl && exl[0] ? exl[0] : "";
	      }
	    }
	    elem.queryEditor('setQuery', query);
	  }
	});
	elem.on("unload", function(ev, rc) {
	  if ( elem.closest(".swish").swish('preserve_state') ) {
	    var state = elem[pluginName]('getState');
	    if ( state )
	      localStorage.setItem("query", JSON.stringify(state));
	  }
	});
	elem.on("restore", function(ev, rc) {
	  if ( elem[pluginName]('getQuery') == "" ) {
	    var state;
	    // called with explicit query
	    // TBD: not save in this case?
	    try {
	      var str = localStorage.getItem("query");
	      state = JSON.parse(str);
	    } catch(err) {
	    }

	    if ( typeof(state) == "object" ) {
	      elem[pluginName]('setState', state);
	    }
	  }
	});
	elem.on("preference", function(ev, pref) {
	  if ( pref.name == "preserve-state" &&
	       pref.value == false ) {
	    localStorage.removeItem("query");
	  }
	});
      });
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
	      src += '\n%@background@\n' + bg;

	    return src;
	  };
	} else {
	  data.source = "";
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
      ul.find("li.add-example, li.divider").remove();
      for(var i=0; i<list.length; i++) {
	ul.append($.el.li($.el.a(list[i])));
      }
      ul.data('examples', list.slice(0));
      ul.append($.el.li({class:"divider"}));
      ul.append($.el.li({class:'add-example'},
			$.el.a("Add current query to examples")));

      return this;
    },

    /**
     * Add the current query to the examples in the program
     */
    addExample: function()
    { var query	= this.find(".query").prologEditor('getSource');

      if ( query.trim() != "" ) {
	$(".swish-event-receiver:visible")
	     .trigger("addExample",
		      this.find(".query").prologEditor('getSource'));
      } else
      { modal.alert("The query window is empty");
      }

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
	var a;

	if ( (li=findInHistory()) )
	  li.remove();
	if ( ul.children().length >= data.maxHistoryLength )
	  ul.children().first().remove();
	ul.append($.el.li(a=$.el.a(query)));
	$(a).data('time', (new Date().getTime())/1000);
      }

      return this;
    },

    /**
     * @return {Array} An arrayt of strings representing the
     * current history.
     */
    getHistory: function() {
      var ul   = this.find("ul.history");
      var h = [];

      ul.children().each(function() {
	var a =	$(this).find("a");
	h.push({
	  query: a.text(),
	  time:  a.data('time')
	});
      });

      return h;
    },

    restoreHistory: function(h) {
      var ul   = this.find("ul.history");

      ul.html("");
      for(var i=0; i<h.length; i++) {
	var a;
	ul.append($.el.li(a= $.el.a(h[i].query)));
	$(a).data('time', h[i].time);
      }
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

    getState: function() {
      return {
        query:   this[pluginName]('getQuery'),
        history: this[pluginName]('getHistory')
      };
    },

    setState: function(state) {
      this[pluginName]('restoreHistory', state.history||[]);
      this[pluginName]('setQuery', state.query||"");
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

      var query = { query: q,
		    editor: data.editor,
		    query_editor: this.find(".query")
		  };

      if ( typeof(data.source) == "function" )
	query.source = data.source(q);
      else if ( typeof(data.source) == "string" )
	query.source = data.source;
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
    return $(from).closest(".prolog-query-editor");
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
      var li = $(this).closest("li");

      if ( li.hasClass("add-example") )
	Q(this).queryEditor('addExample');
      else
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
      var copy = $.extend({}, options);
      $(el).mousedown(function(ev) {
			if ( ev.which == 1 ) {
			  updateExamples(copy);
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
    var menu = dropup("history", "History", options);

    $(menu).on("mouseenter", "li", function(ev) {
      var a = $(ev.target).closest("li").find("a");
      a.attr("title", utils.ago(a.data('time')));
    });

    return menu;
  }

  function aggregateButton(options) {
    var cls = "aggregate";
    var list = options.aggregates ||
      [ "Aggregate (count all)",
	"--",
	"Projection",
	"Order by",
	"Distinct",
	"Limit",
	"--",
	"Time",
	"Debug (trace)"
      ];
    var ul;

    var dropup = $.el.div(
      {class:"btn-group dropup"},
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
