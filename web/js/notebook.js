/**
 * @fileOverview
 * Manage the cell structure of a notebook modelled after IPython
 * NoteBook.  The nodebook consists of a toolbar with a series of
 * buttons and manages a list of cells.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "laconic" ],
       function() {

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
	    glyphButton("circle-arrow-up", "insertAbove", "Insert cell above"),
	    glyphButton("circle-arrow-down", "insertBelow", "Insert cell below"),
	    sep(),
	    glyphButton("play", "run", "Run"),
	    glyphButton("stop", "interrupt", "Interrupt"),
	    sep(),
	    typeDropDown()
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

    run: function() {
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

  function typeDropDown() {
    function item(name) {
      return $.el.li({role:"presentation"},
		     $.el.a({role:"menuitem", href:"#"},
			    name));

    }

    var dd = $.el.div({class:"dropdown cell-type"},
		      $.el.button({class:"btn btn-sm dropdown-toggle",
		                  type:"button",
				  "data-toggle":"dropdown"},
				  "Cell type ",
				  $.el.span({class:"caret"})),
		      $.el.ul({class:"dropdown-menu"},
			      item("Source"),
			      item("Query"),
			      item("Markdown"),
			      item("Heading 1"),
			      item("Heading 2"),
			      item("Heading 3"),
			      item("Heading 4")));
    return dd;
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
  var id = 0;

  /** @lends $.fn.nbCell */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.attr("tabIndex", -1);
	elem.append("Cell "+id+++" ");
	elem.append($.el.input());

	elem.data(pluginName, data);	/* store with element */
      });
    }
  }; // methods

  // <private functions>

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
