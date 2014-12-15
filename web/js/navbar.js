/**
 * @fileOverview
 * Embed the navigation bar
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */


define([ "jquery", "preferences", "laconic" ],
       function($, preferences) {

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
      var ul1 = this.children(".nav.navbar-nav");
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

  function appendDropdown(dropdown, label, onclick) {
    if ( onclick == "--" ) {
      dropdown.append($.el.li({class:"divider"}));
    } else if ( typeof(onclick) == "function" ) {
      var a = $.el.a(label);

      $(a).data('action', onclick);
      if ( onclick.name )
	$(a).attr("id", onclick.name);

      dropdown.append($.el.li(a));
    } else {
      if ( onclick.type == "checkbox" ) {
	var cb = $($.el.input({type:"checkbox"}));

	if ( onclick.preference !== undefined ) {
	  cb.addClass("swish-event-receiver");
	  if ( preferences.getVal(onclick.preference) )
	    cb.prop("checked", true);
	  cb.on("click", function() {
	    preferences.setVal(onclick.preference, $(this).prop("checked"));
	  });
	  cb.on("preference", function(pref) {
	    if ( pref.name == onclick.preference )
	      cb.prop("checked", pref.value);
	  });
	} else {
	  if ( onclick.checked )
	    cb.prop("checked", onclick.checked);

	  cb.on("click", function() {
	    onclick.action($(this).prop("checked"));
	  });
	}
        dropdown.append($.el.li({class:"checkbox"},
				cb[0],
				$.el.span(label)));
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
    var action = $(a).data('action');

    if ( action ) {
      ev.preventDefault();
      action.call(a);
    }

    return false;
  }

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

