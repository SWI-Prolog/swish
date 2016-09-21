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
 * Show modal windows
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "config", "preferences", "links", "jquery", "laconic", "bootstrap" ],
       function(config, preferences, links) {

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
	elem.on("pldoc", function(ev, data) {
	  elem.swishModal('showPlDoc', data);
	});
	elem.on("form", function(ev, data) {
	  elem.swishModal('showForm', data);
	});
	elem.on("dialog", function(ev, data) {
	  elem.swishModal('show', data);
	});
	elem.on("error", function(ev, data) { /* still needed? */
	  elem.swishModal('show', data);
	});
	elem.on("alert", function(ev, str) {
	  var icon = "<span class='glyphicon glyphicon-warning-sign'></span>";
	  elem.swishModal('show', {title: icon, body:str});
	});
	elem.on("ajaxError", function(ev, jqXHR) {
	  elem.swishModal('showAjaxError', jqXHR);
	});
	elem.on("feedback", function(ev, options) {
	  elem.swishModal('feedback', options);
	});
      });
    },

    /**
     * Show a help file.  The help file is a normal HTML document.  The
     * `<title>` element is used for the title, while the `<body>`
     * carries the content of the help file.
     * @param {Object} options
     * @param {String} options.file file help file.
     * @param {String} options.notagain Identifier to stop this dialog
     */
    showHelp: function(options) {
      var that = this;

      if ( options.notagain && preferences.notagain(options.notagain) )
	return;

      $.ajax({ url: config.http.locations.help + "/" + options.file,
	       dataType: "html",
	       success: function(data) {
		 var container = $("<div>");
		 container.html(data);
		 that.swishModal('show',
				 $.extend(
				   { title: container.find("title").text(),
				     body:  container
				   }, options));
	       }
             });
    },

    /**
     * Show a form.  The form is an HTML document.
     * @param {Object} options
     * @param {String} options.file file help file.
     * @param {String} options.notagain Identifier to stop this dialog
     */
    showForm: function(options) {
      var that = this;

      $.ajax({ url: config.http.locations.form + "/" + options.file,
	       dataType: "html",
	       success: function(data) {
		 var container = $("<div>");
		 container.html(data);
		 that.swishModal('show',
				 $.extend(
				   { title: container.find("legend").text(),
				     body:  container
				   }, options));
	       }
             });
    },

    /** Show PlDoc manual page
     * @param {Object} options
     * @param {String} options.name is the name of the predicate to show
     * @param {String} options.arity arity of the predicate
     * @param {String} [options.module] module of the predicate
     */
    showPlDoc: function(options) {
      function docURL(options) {
	var term = "("+options.name+")/"+options.arity;
	if ( options.module )			/* FIXME: must be valid Prolog term */
	  term = options.module+":"+term;
	return   config.http.locations.pldoc_doc_for
	       + "?header=false&object="
	       + encodeURIComponent(term);
      }

      function docBody(content, url) {
	content.parents("div.modal-dialog").addClass("swish-embedded-manual");
	return "<iframe class='swish-embedded-manual' " +
		       "onload='javascript:resizeIframe(this);' " +
                       "src='"+url+"'>" +
	       "</iframe>";
      }

      var data = { title: "SWI-Prolog manual",
                   body:  function() {
		     return docBody(this, docURL(options))
		   }
                 };

      return this.swishModal('show', data);
    },

    /**
     * Show a modal dialog.
     * @param {Object} options
     * @param {String} options.title HTML rendered as title
     * @param {String|function} options.body  If this is a string the
     * content is set using `$.html()`, else the function is called,
     * where `this` refers to the jQuery content element and the
     * function result is added to the content using `$.append()`.
     * @param {String} options.notagain Identifier to stop this dialog
     * showing
     */
    show: function(options) {
      var content = $.el.div({class:"modal-body"});
      var title   = $.el.h2();
      var md      = $.el.div({class:"modal-content"},
			     $.el.div({class:"modal-header"},
				      notAgain(options),
				      closeButton(),
				      title),
			     content);
      var modalel = $.el.div({class:"modal fade", id:"ajaxModal",
			      tabindex:-1, role:"dialog"
			     },
			     $.el.div({class:"modal-dialog"},
				      md));
      if ( options.notagain && preferences.persistent() ) {
	$(md).append($.el.div(
	  {class:"modal-footer"},
	  notAgain(options)));
      }
      content = $(content);
      if ( typeof(options.body) == "function" ) {
	var c = options.body.call(content);
	if ( c )
	  content.append(c);
      } else {
	content.html(options.body);
      }
      $(title).html(options.title);
      $(modalel).modal({show: true})
		.on("click", "a", links.followLink)
	        .on("shown.bs.modal", initTagsManagers)
	        .on("hidden.bs.modal", function() {
		  $(this).remove();
		});

      return this
    },

    /**
     * Display information about an ajax error
     */
    showAjaxError: function(jqXHR) {
      var dom = $.el.div();

      $(dom).html(jqXHR.responseText);
      var h1 = $(dom).find("h1");
      var title = h1.text() || "Server error";
      h1.remove();

      var data = { title: title,
		   body: dom
		 };

      this.swishModal('show', data);
    },

    /**
     * Display briefly a feedback message
     * @param {Object} options
     * @param {String} options.html defines the HTML content that is
     * rendered.
     * @param {Number} [options.duration=1500] number of milliseconds
     * that the message is visible.
     * @param {Object} [options.owner=$("body")] is the DOM element to
     * which the feedback window is added.
     */
    feedback: function(options) {
      var win = $.el.div({class:"feedback "+options.type||""});
      $(win).html(options.html);

      $(options.owner||"body").append(win);
      setTimeout(function() {
	$(win).hide(400, function() {
	  $(win).remove();
	});
      }, options.duration||1500);
      return this;
    }
  }; // methods

  function closeButton() {
    var button = $.el.button({ type:"button", class:"close",
			       "data-dismiss":"modal"
                             });
    $(button)
	.html("&times;")
	.on("click", function(ev) {
	  var modalel = $(this).parents(".modal");
	  var input   = modalel.find("[data-notagain]");
	  ev.preventDefault();
	  if ( input && input.prop('checked') ) {
	    var id = input.attr("data-notagain");
	    preferences.setNotAgain(id);
	  }
	});

    return button;
  }

  function notAgain(options) {
    if ( options.notagain && preferences.persistent() ) {
      return $.el.label($.el.input({ type:"checkbox",
				     'data-notagain':options.notagain,
				     name:"dismiss"
				   }),
			" Don't show again!");
    } else {
      return "";
    }
  }

  /**
   * Tags managers must be initialised after the DOM is complete.
   * This cooperates with `tagInput()` from `form.js`
   */
  function initTagsManagers() {
    var set = $(this).find(".tm-input");

    set.each(function() {
      var elem = $(this);
      var tags = elem.data("prefilled");
      var options = {};

      if ( tags ) options.prefilled = tags;

      elem.tagsManager(options);
    });
  }

  /**
   * See http://stackoverflow.com/questions/9975810/make-iframe-automatically-adjust-height-according-to-the-contents-without-using
   */
  window.resizeIframe = function(iframe) {
    iframe.style.height = 0;
    iframe.style.height = iframe.contentWindow.document.body.scrollHeight+20
                          + 'px';
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

  return {
    ajaxError: function(jqXHR) {
      $(".swish-event-receiver").trigger("ajaxError", jqXHR);
    },
    feedback: function(options) {
      $(".swish-event-receiver").trigger("feedback", options);
    },
    alert: function(options) {
      $(".swish-event-receiver").trigger("alert", options);
    }
  };
});

