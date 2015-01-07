/**
 * @fileOverview
 * Support the SWISH search box.  This we want to find:
 *
 *   - Predicates (manual)		[TBD]
 *   - Source code (line)		[TBD]
 *   - Saved programs by		[TBD]
 *     - Name				[TBD]
 *     - Tag				[TBD]
 *     - Description			[TBD]
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "typeahead" ],
       function($, config) {

(function($) {
  var pluginName = 'search';

  /** @lends $.fn.search */
  var methods = {
    /**
     * Turn Bootstrap search input into a typeahead widget
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);

	var builtIn = new Bloodhound({
			name: "built-in",
			remote: config.http.locations.typeahead +
				"?set=built_in&q=%QUERY",
			datumTokenizer: function(d) {
			  return Bloodhound.tokenizers.whitespace(d.label);
			},
			queryTokenizer: Bloodhound.tokenizers.whitespace
	               });
	builtIn.initialize();

	var files = new Bloodhound({
			name: "built-in",
			remote: config.http.locations.typeahead +
				"?set=file&q=%QUERY",
			datumTokenizer: function(d) {
			  return Bloodhound.tokenizers.whitespace(d.label);
			},
			queryTokenizer: Bloodhound.tokenizers.whitespace
	               });
	files.initialize();

	elem.typeahead({ minLength: 1,
			 highlight: true
		       },
		       [ { name: "files",
			   displayKey: 'label',
			   source: files.ttAdapter()
		         },
			 { name: "built-in",
			   displayKey: 'label',
			   source: builtIn.ttAdapter()
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

	  elem.search('q', data);

	  ev.preventDefault();
	  return false;
	});
      });
    },

    /**
     * Search for the a given query.
     *
     * @param {String|Object} q specifies the search target. If it is a
     * string, no autocompletion was performed.  If it is an object, it
     * is the object returned by Bloodhound
     */
    search: function(q) {
      console.log(q);
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class search
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.search = function(method) {
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
