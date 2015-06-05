/**
 * @fileOverview
 * This file deals with tabbed panes.  It implements dynamic tabs on top
 * if Bootstrap.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "laconic" ],
       function() {

(function($) {
  var pluginName = 'tabbed';

  /** @lends $.fn.tabbed */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.tabbed('makeTabbed');

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * Turn the pane into a tabbed pane
     */
    makeTabbed: function() {
      var children = this.children();
      var ul = $.el.ul({ class:"nav nav-tabs",
			 role:"tablist"
		       });

      this.prepend(ul);

      for(var i=0; i<children.length; i++) {
	var id = 'tab-'+i;
	$(ul).append($.el.li({ role:"presentation", class:"active" },
			     $.el.a({href:"#"+id},
				    "Source "+i)));
	wrapInTab($(children[i]), id, true);
      }
    }
  }; // methods

  /**
   * Wrap a content element in a Bootstrap tab content.
   * @param dom is the object that must be wrapped
   * @param id is the identifier to give to the new content
   * @param active sets the tab to active if `true`
   */
  function wrapInTab(dom, id, active) {
    dom.wrap('<div role="tabpanel" class="tab-pane" id="'+id+'"></div>');
    var wrapped = dom.parent();

    if ( active )
      wrapped.addClass("active");

    wrapped.wrap('<div class="tab-content"></div>');
  }

  /**
   * <Class description>
   *
   * @class tabbed
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tabbed = function(method) {
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
