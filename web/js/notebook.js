/**
 * @fileOverview
 * Manage the cell structure of a notebook modelled after IPython
 * NoteBook.  The nodebook consists of a toolbar with a series of
 * buttons and manages a list of cells.  The file defines two plugins
 * `notebook`, implementing the overall notebook and `nbCell`,
 * implementing a single cell.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define([ "jquery", "config", "laconic" ],
       function($, config) {

var cellTypes = {
  "program":  { label:"Program" },
  "query":    { label:"Query" },
  "markdown": { label:"Markdown" },
};

(function($) {
  var pluginName = 'notebook';
  var clipboard = null;

  /** @lends $.fn.notebook */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	var toolbar;

	elem.addClass("notebook");

	elem.append($.el.div(
	    {class:"nb-titlebar"},
	    $.el.span({class:"nb-title"}, "Notebook")));
	elem.append(toolbar = $.el.div(
            {class:"nb-toolbar"},
	    glyphButton("floppy-save", "checkpoint", "Checkpoint"),
	    sep(),
	    glyphButton("trash", "delete", "Delete cell"),
	    glyphButton("copy", "copy", "Copy cell"),
	    glyphButton("paste", "paste", "Paste cell below"),
	    sep(),
	    glyphButton("chevron-up", "up", "Move cell up"),
	    glyphButton("chevron-down", "down", "Move cell down"),
	    sep(),
	    glyphButton("plus", "insertBelow", "Insert cell below"),
	    sep(),
	    glyphButton("play", "run", "Run")
	    ));
	elem.append($.el.div({class:"nb-content"}));

	$(toolbar).on("click", "a.btn", function(ev) {
	  var action = $(ev.target).closest("a").data("action");
	  elem.notebook(action);
	  ev.preventDefault();
	  return false;
	});

	elem.focusin(function(ev) {
	  elem.notebook('active', $(ev.target).closest(".nb-cell"));
	});

	elem.data(pluginName, data);	/* store with element */
      });
    },

		 /*******************************
		 *	  BUTTON ACTIONS	*
		 *******************************/

    checkpoint: function() {
      alert("Checkpoint safe not yet implemented");
    },

    delete: function(cell) {
      cell = cell||currentCell(this);
      if ( cell ) {
	this.notebook('active', cell.next());
	cell.remove();
      }
      return this;
    },

    copy: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	clipboard = cell;
    },

    paste: function() {
      if ( clipboard ) {
	var newcell = clipboard.clone(true,true);
	this.notebook('insert', { where:"below", cell:newcell });
      } else {
	alert("Clipboard is empty");
      }
    },

    up: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.insertBefore(cell.prev());
      return this;
    },

    down: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.insertAfter(cell.next());
      return this;
    },

    insertAbove: function() {
      return this.notebook('insert', { where:"above" });
    },

    insertBelow: function() {
      return this.notebook('insert', { where:"below" });
    },

    run: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.nbCell("run");
    },

    cellType: function(cell, type) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.nbCell('type', type);
    },

		 /*******************************
		 *	 CELL MANAGEMENT	*
		 *******************************/

    active: function(cell, focus) {
      if ( cell && cell.length == 1 )
      { this.children(".nb-cell.active").removeClass("active");
	cell.addClass("active");
	if ( focus )
	  cell.focus();
      }
    },

    /**
     * Insert a new cell
     * @param {Object} [options]
     * @param {String} [options.where] defines where the cell is
     * inserted relative to the cell with the current focus.
     */
    insert: function(options) {
      options   = options||{};
      var relto = currentCell(this);
      var cell  = options.cell || $.el.div({class:"nb-cell"});

      if ( relto ) {
	if ( options.where == 'above' ) {
	  $(cell).insertBefore(relto);
	} else {
	  $(cell).insertAfter(relto);
	}
      } else {
	this.append(cell);
      }

      if ( !options.cell )
	$(cell).nbCell();
      this.notebook('active', $(cell));
    }
  }; // methods

  // <private functions>

  function glyphButton(glyph, action, title) {
    var btn = $.el.a({href:"#", class:"btn btn-info btn-sm",
		      title:title, "data-action":action},
		     $.el.span({class:"glyphicon glyphicon-"+glyph}));

    return btn;
  }

  function sep() {
    return $.el.span({class:"thin-space"}, " ");
  }

  /**
   * @returns {Object|null} cell that is focussed and inside our
   * notebook.
   * @param {Object} nb is the notebook
   */
  function currentCell(nb) {
    var active = $(nb).find(".nb-cell.active");

    if ( active.length == 1 )
      return active.first();

    return null;
  }

  /**
   * <Class description>
   *
   * @class notebook
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.notebook = function(method) {
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
		 *	    PLUGIN nbCell	*
		 *******************************/

(function($) {
  var pluginName = 'nbCell';

  /** @lends $.fn.nbCell */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	var g;

	elem.data(pluginName, data);	/* store with element */

	elem.attr("tabIndex", -1);
	elem.append($.el.div({class:"nb-type-select"},
			     $.el.label("Create a "),
			     g=$.el.div({class:"btn-group",role:"group"}),
			     $.el.label("cell here.")));

	for(var k in cellTypes) {
	  if ( cellTypes.hasOwnProperty(k) )
	    $(g).append($.el.button({ type:"button",
				      class:"btn btn-default",
				      "data-type":k
				    },
				    cellTypes[k].label));
	}

	$(g).on("click", ".btn", function(ev) {
	  elem.nbCell('type', $(ev.target).data('type'));
	});
      });
    },

    type: function(type) {
      var data = this.data(pluginName);
      if ( data.type != type ) {
	switch(type) {
	  case "program":
	    makeEditor(this, {});
	    break;
	  case "markdown":
	    makeEditor(this, {mode:"markdown"});
	    break;
	}
	data.type = type;
	this.addClass(type);
      }
    },

    /**
     * Run the current cell
     */
    run: function() {
      if ( this.hasClass("runnable") ) {
	var data = this.data(pluginName);

	return methods.run[data.type].apply(this, arguments);
      } else {
	alert("Cell is not runnable");
      }
    }
  }; // methods

		 /*******************************
		 *	    RUN BY TYPE		*
		 *******************************/

  methods.run.markdown = function() {
    var cell = this;
    var markdownText = cellText(this);

    function makeEditable(ev) {
      var cell = $(ev.target).closest(".nb-cell");
      var text = cell.data('markdownText');
      makeEditor(cell, { mode:"markdown", value:text });
      cell.off("dblclick", makeEditable);
    }

    $.get(config.http.locations.markdown,
	  { text: markdownText
	  },
	  function(data) {
	    cell.html(data);
	    cell.removeClass("runnable");
	    cell.data('markdownText', markdownText);
	    cell.on("dblclick", makeEditable);
	  });
  };
  methods.run.program = function() {
    alert("Please define a query to run this program");
  };

		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

  function makeEditor(cell, options) {
    var editor;

    cell.html("");
    cell.append(editor=$.el.div({class:"editor"}));
    $(editor).prologEditor(options);
    cell.addClass("runnable");
  }

  function cellText(cell) {
    return cell.find(".editor").prologEditor('getSource');
  }

  /**
   * <Class description>
   *
   * @class nbCell
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.nbCell = function(method) {
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
