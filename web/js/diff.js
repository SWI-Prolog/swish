/**
 * @fileOverview
 * View diffs between versions
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "difflib", "diffview" ],
       function() {

(function($) {
  var pluginName = 'diff';

  /** @lends $.fn.diff */
  var methods = {
    /**
     * Render diff between two strings in the target element (must
     * be a `<div>`).
     *
     * @param {Object} [options]
     * @param {String} [base] Old version
     * @param {String} [head] New version
     * @param {String} [baseName="Base text"] Name for old version
     * @param {String} [headName="Current text"] Name for current version
     * @param {Number} [context=3] Number of context lines
     */
    _init: function(options) {
      return this.each(function() {
	var base        = difflib.stringAsLines(options.base);
	var newtxt      = difflib.stringAsLines(options.head);
	var sm          = new difflib.SequenceMatcher(base, newtxt);
	var opcodes     = sm.get_opcodes();
	var contextSize = options.contextSize == undefined
				? 3 : options.contextSize;

	this.appendChild(diffview.buildView(
	  { baseTextLines: base,
	    newTextLines: newtxt,
	    opcodes: opcodes,
	    baseTextName: options.baseName || "Base text",
	    newTextName:  options.headName || "Current text",
	    contextSize: contextSize,
	    viewType: $("inline").checked ? 1 : 0
	  }));
      });
    }
  }; // methods

  /**
   * This class is a jQuery wrapper around
   *
   * @class diff
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.diff = function(method) {
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
