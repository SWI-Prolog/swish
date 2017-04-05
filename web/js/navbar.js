/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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
 * Embed the navigation bar
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */


define([ "jquery", "preferences", "form", "laconic" ],
       function($, preferences, form) {

(function($) {
  var pluginName = 'navbar';

  /** @lends $.fn.navbar */
  var methods = {
    /**
     * Initialize a navigation bar.  For example:
     *
     *     $("#navbar").navbar(
     *       { "File":
     *         { "New": function() { ... },
     *           "Save": function() { ... }
     *         },
     *         "Edit":
     *         { "Undo": function() { ... }
     *         }
     *       });
     *
     * @param {Object} actions Nested object mapping menu labels to
     * submenus or functions.
     */
    _init: function(actions) {

      return this.each(function() {
	var elem = $(this);
	var data = {};

	for(var p in actions) {
	  if ( actions.hasOwnProperty(p) ) {
	    elem.navbar('appendDropdown', p);
	    elem.navbar('populateDropdown', p, actions[p]);
	  }
	}

	elem.on("click", "a", function(ev) { runMenu(this, ev); } );
      });
    },

    /**
     * @param {String} label Name of new dropdown to add
     */
    appendDropdown: function(label) {
      var ul1 = this.children(".nav.navbar-nav.menubar");
      var ul2 = $.el.ul({name:label, class:"dropdown-menu"});
      var li  = $.el.li({class:"dropdown"},
			$.el.a({class:"dropdown-toggle",
				"data-toggle":"dropdown"
			       },
			       label,
			       $.el.b({class:"caret"})),
			ul2);

      ul1.append(li);

      return this;
    },

    /**
     * @param {String} name Name of the dropdown to populate
     * @param {Object|function} actions Object relating dropdown menu
     * labels to functions.  If a function is passed, it is called with
     * two arguments: the navbar and the name of the dropdown to
     * populate.
     */
    populateDropdown: function(name, actions) {
      if ( typeof(actions) == "function" ) {
	actions(this, name);
      } else {
	var ul = dropDownUL(this, name);

	for(var p in actions) {
	  if ( actions.hasOwnProperty(p) ) {
	    appendDropdown(ul, p, actions[p]);
	  }
	}
      }
    },

    /**
     * @param {String} name is the name of the dropdown to clear
     */
    clearDropdown: function(name) {
      var ul = dropDownUL(this, name);

      ul.html("");
      return this;
    },

    /**
     * @example
     * $("#navbar").navbar('extendDropdown', 'File',
     *                     'Save as', function () { ... });
     * @param {String} name is the dropdown to extend
     * @param {String} label is the label to add
     * @param {function} onclick is the action to perform
     */
    extendDropdown: function(name, label, onclick) {
      var ul = dropDownUL(this, name);

      appendDropdown(ul, label, onclick);
    }
  }; // methods

  /**
   * Append something to a navbar dropdown.
   *
   * @param {Object} dropdown is the jQuery object representing the
   * dropdown.
   * @param {String} label is the label for the menu entry
   * @param {any} options defines the action.  There are many variations:
   *   - The string `"--"` creates a _divider_
   *   - A function creates a normal menu entry that calls the function
   *     when selected
   *   - An object with `.type == "checkbox"` creates a checkbox.  In
   *     addition
   *	 - if `.preference` exists, the checkbox is associated with the
   *	   named preference, otherwise
   *	 - if `.action' exists, it is called on change with the new
   *	   value as argument.
   *   - An object with `.type == "submenu" creates a submenu.
   *   - An object with `.typeIcon` gets an icon indicating the type
   */
  function appendDropdown(dropdown, label, options) {
    function glyph(name) {
      if ( name ) {
	return $.el.span({
	  class:"dropdown-icon glyphicon glyphicon-" + name});
      }
    }

    if ( options == undefined ) {
      // ignored
    } else if ( options == "--" ) {
      dropdown.append($.el.li({class:"divider"}));
    } else if ( typeof(options) == "function" ) {	/* Simple action */
      var a;
      var i;

      if ( options.typeIcon ) {
	a = $.el.a(form.widgets.typeIcon(options.typeIcon),
		   label);
      } else if ( options.glyph ) {
	a = $.el.a(glyph(options.glyph), label);
      } else if ( (i=label.indexOf("(")) > 0 ) {
	var accell = label.substr(i);
	a = $.el.a({class:"accelerated"},
		   label.substr(0,i).trim(),
		   $.el.span({class:"accell-spacer"},accell),
		   $.el.span({class:"accell-text"},accell));
      } else {
	a = $.el.a(label);
      }

      $(a).data('navbar-action', options);
      if ( options.name )
	$(a).attr("id", options.name);

      dropdown.append($.el.li(a));
    } else {						/* Checkbox item */
      if ( options.type == "checkbox" ) {
	var cb = $($.el.input({type:"checkbox"}));

	if ( options.preference !== undefined ) {
	  cb.addClass("swish-event-receiver");
	  if ( preferences.getVal(options.preference) )
	    cb.prop("checked", true);
	  cb.on("click", function() {
	    preferences.setVal(options.preference, $(this).prop("checked"));
	  });
	  cb.on("preference", function(pref) {
	    if ( pref.name == options.preference )
	      cb.prop("checked", pref.value);
	  });
	} else {
	  if ( options.checked )
	    cb.prop("checked", options.checked);

	  cb.on("click", function() {
	    options.action($(this).prop("checked"));
	  });
	}
        dropdown.append($.el.li({class:"checkbox"},
				cb[0],
				$.el.span(label)));
      } else if ( options.type == "submenu" ) {		/* Submenu */
	var submenu = $.el.ul({class:"dropdown-menu sub-menu"});

	dropdown.append($.el.li($.el.a({class:"trigger right-caret"},
				       glyph(options.glyph),
				       label),
				submenu));
	if ( options.action )
	  $(submenu).data('navbar-action', options.action);
	if ( options.items ) {
	  for(var i=0; i<options.items.length; i++) {
	    $(submenu).append($.el.li($.el.a(options.items[i])));
	  }
	}
	if ( options.update ) {
	  $(submenu).on("update", function(ev) {
	    options.update.call(ev.target);
	  });
	}
      } else {
	alert("Unknown navbar item");
      }
    }
  }

  function dropDownUL(nb, name) {
    return nb.find(".dropdown-menu").filter(function() {
      return $(this).attr("name") == name;
    });
  }

  function runMenu(a, ev) {
    if ( $(a).hasClass("trigger") ) {
      clickSubMenu.call(a, ev);
    } else {
      var action = ($(a).data('navbar-action') ||
		    $(a).parents("ul").data('navbar-action'));

      clickNotSubMenu.call(a, ev);

      if ( action ) {
	ev.preventDefault();
	action.call(a, ev);
      } else if ( $(a).hasClass("trigger") ) {
	clickSubMenu.call(a, ev);
      }

      return false;
    }
  }

  /**
   * Bootstrap 3 extension to provide submenus.  Inspired by
   * http://jsfiddle.net/chirayu45/YXkUT/16/
   * Triggers an `update` event to the submenu's <ul> just
   * before opening it.
   */
  function clickSubMenu(ev) {
    var current = $(this).next();		 /* the submenu <ul> */
    var grandparent = $(this).parent().parent(); /* the main menu <ul> */

    if ( $(this).hasClass('left-caret') ||
	 $(this).hasClass('right-caret') )
      $(this).toggleClass('right-caret left-caret');

    grandparent.find('.left-caret')
	       .not(this)
	       .toggleClass('right-caret left-caret');
    grandparent.find(".sub-menu:visible")
	       .not(current).hide();

    current.trigger("update");
    current.toggle();
    ev.stopPropagation();
  }

  function clickNotSubMenu(ev) {
    var root = $(this).closest('.dropdown');

    root.find('.left-caret').toggleClass('right-caret left-caret');
    root.find('.sub-menu:visible').hide();
  }

/* invoke is merged in general menu callback above
  $(function() {
    $(".dropdown-menu > li > a.trigger").on("click", clickSubMenu);
    $(".dropdown-menu > li > a:not(.trigger)").on("click", clickNotSubMenu);
  });
*/

  /**
   * navbar jQuery plugin populates the application navigation bar using
   * bootstrap styling.  Menu entries are added from JavaScript objects,
   * which directly assocuate a label with a function.  It also allows
   * populating a dropdown from a callback.
   *
   * @class navbar
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} method Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.navbar = function(method) {
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

