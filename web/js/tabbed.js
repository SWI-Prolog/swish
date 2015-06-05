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
      var contents = $.el.div({class:"tab-content"});

      this.prepend(contents);
      this.prepend(ul);

      $(ul).on("click", "span.xclose", function(ev) {
	var id = $(ev.target).parent().attr("data-id");
	console.log($(ev.target).parents(".tabbed"));
	$(ev.target).parents(".tabbed").first().tabbed('removeTab', id);
	ev.preventDefault();
      });
      $(ul).on("click", "a", function(ev) {
	$(ev.target).tab('show');
	ev.preventDefault();
      });

      for(var i=0; i<children.length; i++) {
	var id = 'tab-'+i;
	var li = this.tabbed('tabLabel', id, "Source");
	$(li).addClass("active");
	$(ul).append(li);
	$(contents).append(wrapInTab($(children[i]), id, true));
      }

      var create = $.el.a({class: "tab-new compact"}, glyphicon("plus"));
      $(ul).append($.el.li({ role:"presentation" }, create));
      $(create).on("click", function(ev) {
	var tabbed = $(ev.target).parents(".tabbed").first();

	var e = $("<div>Hello World</div>");
	tabbed.tabbed('addTab', e);
      });
    },

    /**
     * Add a new tab using content
     */
    addTab: function(content, id, active) {
      var ul = this.tabbed('navTabs');
      var id = "tab-new";

      this.tabbed('navContent').append(wrapInTab(content, id, active));

      var li  = this.tabbed('tabLabel', id, "Source "+id);

      var create = ul.find("a.tab-new");
      if ( create.length == 1 )
	$(li).insertBefore(create.first().parent());
      else
	ul.append(li);

      $(li).find("a").first().tab('show');
    },

    /**
     * Remove tab with given Id. Make the previous tab active, or if
     * there is no previous, the next.
     * @param id is the id of the tab to destroy
     */
    removeTab: function(id) {
      var li = this.tabbed('navTabs').find("a[data-id='"+id+"']").parent();
      var new_active = li.prev() || li.next();

      li.remove();
      $("#"+id).remove();
      if ( new_active )
	new_active.find("a").first().tab('show');
    },

    tabLabel: function(id, name, close) {
      var a1 = $.el.a({class:"compact", href:"#"+id, "data-id":id},
		      name, glyphicon("remove", "xclose"));
      var li = $.el.li({role:"presentation"}, a1);

      return li;
    },

    /**
     * Get the UL list that represents the nav tabs
     */
    navTabs: function() {
      return this.find("ul.nav-tabs").first();
    },

    navContent: function() {
      return this.find("div.tab-content").first();
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

    return wrapped;
  }

  function glyphicon(glyph, className) {
    var span = $.el.span({class:"glyphicon glyphicon-"+glyph});

    if ( className )
      $(span).addClass(className);

    return span;
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
