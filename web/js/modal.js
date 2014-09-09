/**
 * @fileOverview
 * Show modal windows
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "config", "jquery", "laconic", "bootstrap" ],
       function(config) {

(function($) {
  var pluginName = 'swishModal';

  /** @lends $.fn.modal */
  var methods = {
    /**
     * Initialize the widget and listen for "help" events.
     * @param {Object} options currently ignored
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);

	elem.addClass("swish-event-receiver");
	elem.on("help", function(ev, data) {
	  elem.swishModal('showHelp', data);
	});
      });
    },

    /**
     * Show a help file.  The help file is a normal HTML document.  The
     * `<title>` element is used for the title, while the `<body>`
     * carries the content of the help file.
     * @param {String} file help file.
     */
    showHelp: function(file) {
      var that = this;

      $.ajax({ url: config.http.locations.help + "/" + file,
	       dataType: "html",
	       success: function(data) {
		 var container = $("<div>");
		 container.html(data);
		 that.swishModal('show', { title: container.find("title").text(),
					   body:  container
	                                 });
	       }
             });
    },

    /**
     * Show a modal dialog.
     * @param {Object} options
     * @param {String} options.title HTML rendered as title
     * @param {String} options.body  HTML rendered as body
     */
    show: function(options) {
      var content = $.el.div({class:"modal-body"});
      var title   = $.el.h2();
      var modalel = $.el.div({class:"modal fade", id:"ajaxModal",
			      tabindex:-1, role:"dialog"
			     },
			     $.el.div({class:"modal-dialog"},
				      $.el.div({class:"modal-content"},
					       $.el.div({class:"modal-header"},
							closeButton(),
							title),
					       content)));
      $(content).html(options.body);
      $(title).html(options.title);
      $(modalel).modal({show: true});

      return this
    }
  }; // methods

  function closeButton() {
    var button = $.el.button({ type:"button", class:"close",
			       "data-dismiss":"modal"
                             });
    $(button).html("&times;");
    return button;
  }

  /**
   * This class is a small layer around bootstrap $.modal that isolates
   * us from bootstrap and provides most of the intermediate divs
   * needed to create a nice modal window.  In addition, it listens to
   * `"help"` events.
   *
   * @class swishModal
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.swishModal = function(method) {
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

