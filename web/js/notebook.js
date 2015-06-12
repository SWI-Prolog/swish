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

  /** @lends $.fn.notebook */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.append($.el.div(
	    {class:"nb-titlebar"},
	    $.el.span({class:"nb-title"}, "Notebook")));
	elem.append($.el.div(
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
	    glyphButton("circle-arrow-up", "insert_above", "Insert cell above"),
	    glyphButton("circle-arrow-down", "insert_below", "Insert cell below"),
	    sep(),
	    glyphButton("play", "run", "Run"),
	    glyphButton("stop", "interrupt", "Interrupt"),
	    sep(),
	    typeDropDown()
	    ));
	elem.append($.el.div({class:"nb-content"}));

	elem.data(pluginName, data);	/* store with element */
      });
    }
  }; // methods

  // <private functions>

  function glyphButton(glyph, className, title) {
    var btn = $.el.a({href:"#", class:"btn btn-info btn-sm", title:title},
		     $.el.span({class:"glyphicon glyphicon-"+glyph}));

    if ( className )
      $(btn).addClass(className);

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

});
