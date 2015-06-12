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
	    "Notebook"));
	elem.append($.el.div(
            {class:"nb-toolbar"},
	    glyphicon("floppy-save"),
	    sep(),
	    glyphicon("trash"),
	    glyphicon("copy"),
	    glyphicon("paste"),
	    sep(),
	    glyphicon("chevron-up"),
	    glyphicon("chevron-down"),
	    sep(),
	    glyphicon("circle-arrow-up"),
	    glyphicon("circle-arrow-down"),
	    sep(),
	    glyphicon("play"),
	    glyphicon("stop")
	    ));
	elem.append($.el.div({class:"nb-content"}));

	elem.data(pluginName, data);	/* store with element */
      });
    }
  }; // methods

  // <private functions>

  function glyphicon(glyph, className) {
    var span = $.el.span({class:"glyphicon glyphicon-"+glyph});

    if ( className )
      $(span).addClass(className);

    return span;
  }

  function sep() {
    return $.el.span({class:"thin-space"}, " ");
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
