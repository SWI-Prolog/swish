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
var tabbed = {
  tabTypes: {}
};

(function($) {
  var pluginName = 'tabbed';
  var tabid = 0;

  /** @lends $.fn.tabbed */
  var methods = {
    /**
     * Turn the current element into a Bootstrap tabbed pane. All
     * children of the current element are changed into tabs.  The
     * child can control the mapping using:
     *
     *   - `data-label = "Label"`
     *   - `data-close = "disabled"`
     */
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	data.newTab   = options.newTab;
	data.tabTypes = options.tabTypes || tabbed.tabTypes;

	elem.addClass("tabbed");
	elem.tabbed('makeTabbed');
	// Current tab could not handle source, create a new one
	elem.on("source", function(ev, src) {
	  elem.tabbed('tabFromSource', src);
	});

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
	$(ev.target).parents(".tabbed").first().tabbed('removeTab', id);
	ev.preventDefault();
      });
      $(ul).on("click", "a", function(ev) {
	$(ev.target).closest("a").tab('show');
	ev.preventDefault();
      });

			/* Turn children into tabs */
      for(var i=0; i<children.length; i++) {
	var child = $(children[i]);
	var id = genId();
	var label = child.attr("data-label") || "Unknown";
	var close = child.attr("data-close") != "disabled";
	var active = (i == children.length-1);	/* activate last */

	var li = this.tabbed('tabLabel', id, label, close);
	if ( active )
	  $(li).addClass("active");
	$(ul).append(li);
	$(contents).append(wrapInTab($(children[i]), id, active));
      }

			/* Create and handle "+" button */
      var create = $.el.a({ class: "tab-new compact",
			    title: "Open a new tab"
			  },
			  glyphicon("plus"));
      $(ul).append($.el.li({ role:"presentation" }, create));
      $(create).on("click", function(ev) {
	var tabbed = $(ev.target).parents(".tabbed").first();

	tabbed.tabbed('newTab');
	ev.preventDefault();
	return false;
      });

			/* Handle tab-switching */
      $(ul).on("shown.bs.tab", "a", function(ev) {
	var newContentID  = $(ev.target).data("id");
	$("#"+newContentID+" .swish-event-receiver").trigger("activate-tab");
      });
    },

    /**
     * Add an empty new tab from the "+" button.  This calls
     * options.newTab() to return a DOM element for the new
     * tab.
     * @param {HTMLElement} [content] Content for the new tab
     * If omitted, it calls `options.newTab` or uses the method
     * `tabSelect`.
     * @return {jQuery} object representing the created tab
     */
    newTab: function(dom) {
      var data = this.data(pluginName);

      if ( dom == undefined ) {
	if ( data.newTab ) {
	  dom = data.newTab();
	} else {
	  dom = this.tabbed('tabSelect');
	}
      }

      return this.tabbed('addTab', dom, {active:true,close:true});
    },

    /**
     * Add a new tab from the provided source
     */
    tabFromSource: function(src) {
      var tab = this.tabbed('newTab', $("<span></span>"));
      if ( !this.tabbed('setSource', tab, src) ) {
	this.tabbed('removeTab', tab.attr("id"));
      }
      return this;
    },

    /**
     * Transform the new tab into a tab that can hold the requested
     * source.
     * @return {Boolean} `true` if a suitable type was found
     */
    setSource: function(tab, src) {
      if ( typeof(src) == "object" &&
	   ((src.meta && src.meta.name) || src.url) )
      { var name = (src.meta && src.meta.name) ? src.meta.name : src.url;
	var ext  = name.split('.').pop();

	for(var k in tabbed.tabTypes) {
	  if ( tabbed.tabTypes.hasOwnProperty(k) &&
	       tabbed.tabTypes[k].dataType == ext )
	  { var tabType = tabbed.tabTypes[k];
	    var content = $.el.div();

	    tab.html("");
	    tab.tabbed('title', tabType.label, tabType.dataType);
	    tab.append(content);
	    tabType.create(content);
	    $(content).trigger("source", src);
	    return true;
	  }
	}
      }
      return false;
    },

    /**
     * Add a new tab using content
     * @param {Object} content is the DOM node to use as content for the
     * tab.
     * @param {Object} options
     * @param {Boolean} [options.active] if `true`, make the new tab
     * active
     * @param {Boolean} [options.close] if `true`, allow closing the new
     * tab.
     * @return {jQuery} the created tab element
     */
    addTab: function(content, options) {
      var ul  = this.tabbed('navTabs');
      var id  = genId();
      var tab =	wrapInTab(content, id, options.close);

      this.tabbed('navContent').append(tab);

      var li  = this.tabbed('tabLabel', id, "New tab", close, "select");

      var create = ul.find("a.tab-new");
      if ( create.length == 1 )
	$(li).insertBefore(create.first().parent());
      else
	ul.append(li);

      if ( options.active )
	$(li).find("a").first().tab('show');

      return tab;
    },

    /**
     * Remove tab with given Id. If the tab is the active tab, make the
     * previous tab active, or if there is no previous, the next. If the
     * tabbed environment becomes empty, add a virgin tab.
     *
     * @param {String} id is the id of the tab to destroy
     */
    removeTab: function(id) {
      var li = this.tabbed('navTabs').find("a[data-id='"+id+"']").parent();
      var new_active;

      if ( $("#"+id).is(":visible") )
	new_active = li.prev() || li.next();
      li.remove();
      $("#"+id).remove();
      if ( new_active && new_active.length > 0 ) {
	new_active.find("a").first().tab('show');
      } else if ( this.tabbed('navContent').children().length == 0 ) {
	this.tabbed('newTab');
      }
    },

    /**
     * Create a label (`li`) for a new tab.
     * @param {String} id is the identifier of the new tab
     * @param {String} label is the textual label of the new tab
     * @param {Boolean} close determines whether or nor a close button
     * is added to the tab.
     * @param {String} [type="pl"] indicates the type of the tab. This
     * is used for associating an icon with the tab.
     */
    tabLabel: function(id, label, close, type) {
      var close_button;

      if ( close )
      { close_button = glyphicon("remove", "xclose");
	$(close_button).attr("title", "Close tab");
      }
      type = type||"pl";

      var a1 = $.el.a({class:"compact", href:"#"+id, "data-id":id},
		      $.el.span({class:"tab-icon "+type}),
		      $.el.span({class:"tab-title"}, label),
		      close_button);
      var li = $.el.li({role:"presentation"}, a1);

      return li;
    },

    /**
     * This method is typically _not_ called on the tab, but on some
     * inner element of the tab.  It changes the title of the tab.
     * @param {String} title is the new title for the tab.
     * @param {String} [type="pl"] is the new type for the tab.
     */
    title: function(title, type) {
      var tab    = this.closest(".tab-pane");
      var tabbed = tab.closest(".tabbed");
      var id     = tab.attr("id");
      var ul	 = tabbed.tabbed('navTabs');
      var a      = ul.find("a[data-id="+id+"]");

      a.find(".tab-title").text(title);
      if ( type ) {
	var icon = a.find(".tab-icon");
	icon.removeClass();
	icon.addClass("tab-icon "+type);
      }

      return tabbed;
    },

    /**
     * Default empty tab content that allows the user to transform
     * the tab into the desired object.
     * @return {Object} containing content for the new tab
     */
    tabSelect: function() {
      var data = this.data(pluginName);
      var dom = $.el.div({class:"tabbed-select"},
			 $.el.label("Create a new "),
			 g=$.el.div({class:"btn-group",role:"group"}),
			 $.el.label("here."));
      for(var k in data.tabTypes) {
	if ( data.tabTypes.hasOwnProperty(k) )
	  $(g).append($.el.button({ type:"button",
				    class:"btn btn-default",
				    "data-type":k
				  },
				  data.tabTypes[k].label));
      }

      $(g).on("click", ".btn", function(ev) {
	var type    = $(ev.target).data('type');
	var tab     = $(ev.target).closest(".tab-pane");
	var content = $.el.div();
	var options = tabbed.tabTypes[type];

	tab.html("");
	tab.tabbed('title', options.label, options.dataType);
	tab.append(content);
	tabbed.tabTypes[type].create(content);
      });
      $(g).addClass("swish-event-receiver");
      $(g).on("source", function(ev, src) {
	var tab = $(ev.target).closest(".tab-pane");
	if ( tab.is(":visible") &&
	     tab.closest(".tabbed").tabbed('setSource', tab, src) ) {
	  ev.stopPropagation();
	}
      });

      return dom;
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
   * @param {Object} dom is the object that must be wrapped
   * @param {String} id is the identifier to give to the new content
   * @param {Boolean} active sets the tab to active if `true`
   * @return {jQuery} `div` object of class `tab-pane` and the
   * passed `id`.
   */
  function wrapInTab(dom, id, active) {
    $(dom).wrap('<div role="tabpanel" class="tab-pane" id="'+id+'"></div>');
    var wrapped = $(dom).parent();

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

  function genId()
  { return "tabbed-tab-"+tabid++;
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

  return tabbed;
});
