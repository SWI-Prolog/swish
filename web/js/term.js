/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2022, VU University Amsterdam
			      CWI Amsterdam
			      SWI-Prolog Solutions b.v.
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
 * Make Prolog terms responsive
 *
 * @version 0.5.0
 * @author Jan Wielemaker, jan@swi-prolog.org
 * @requires jquery
 */

define([ "jquery" ],
       function($) {

(function($) {
  var pluginName = 'pl_term';

  var themenu = null;
  var prepared = false;

  /** @lends $.fn.pl_term */
  var methods = {
    _init: function(options) {

      if ( !prepared ) {
	prepare(this);
	prepared = true;
      }

      return this.each(function() {
      });
    },

    fold: function(options) {
      return this.each(function() {
	var el = $(this);

	el.removeClass("vertical");
	el.addClass('fold');

	if ( !el.children(".pl-ellipsis").length ) {
	  if ( el.hasClass("pl-compound") ) {
	    el.children(".pl-functor")
              .after('<span class="pl-ellipsis">...)</span>');
	  } else if (  el.hasClass("pl-list") ) {
	    el.children(".pl-list-open")
	      .after('<span class="pl-ellipsis">...</span>');
	  } else if (  el.hasClass("pl-dict") ) {
	    el.children(".pl-dict-open")
	      .after('<span class="pl-ellipsis">...}</span>');
	  }
	}
      });
    },

    unfold: function(options) {
      return this.each(function() {
	var el = $(this);

	if ( el.hasClass('fold') ) {
	  el.removeClass('fold');
	  el.children(".pl-ellipsis").remove();
	}
      });
    },

    getLayout: function() {
      el = $(this);

      if ( el.hasClass("fold") )
	return "ellipsis";
      if ( el.hasClass("vertical") )
	return "vertical";

      return "horizontal";
    },

    /**
     * Change the layout of a `.pl-adaptive` element.  The code handles
     * the SWISH bindings special. The `ellipsis` mode hides the whole
     * value, also if it is not an _adaptive_ value.   As a result we
     * must forward `vertical` to the adaptive node because this is
     * not propagating.
     */

    layout: function(options) {

      if ( typeof(options) == "string" )
      { options = { layout: options };
      } else
      { options = options||{};
      }

      if ( options.propagate == undefined ) {
	if ( options.layout == "horizontal" )
	  options.propagate = true;
      }

      return this.each(function() {
	var el = $(this);

	if ( el.hasClass("pl-binding") )
	  el = el.children(".pl-binding-value");

	if ( options.propagated && !options.no_ellipsis &&
	     el[pluginName]('getLayout') == "ellipsis" )
	  return;

	if ( options.layout == "vertical" )
	{ el[pluginName]('unfold');
	  if ( el.hasClass("pl-binding-value") ) {
	    el = el.children(".pl-adaptive");
	    el[pluginName]('layout', options);
	  } else {
	    el.addClass("vertical");
	  }
	} else if ( options.layout == "horizontal" )
	{ el[pluginName]('unfold');
	  el.removeClass("vertical");
	} else if ( options.layout == "ellipsis" )
	{ el[pluginName]('fold');
	} else if ( options.layout == "auto" )
	{ el[pluginName]('layout', 'horizontal');
	  autoLayout(el, options);
	}

	if ( options.propagate ) {
	   var opts = $.extend({}, options, {propagate:false, propagated:true});

	   el.find(".pl-adaptive")[pluginName]('layout', opts);
	}
      });
    },

    /**
     * Called on `.pl-trigger` to open a menu that allows
     * changing the layout of a .pl-compound
     */

    menu: function(options) {
      var funct = $(this);
      var el = funct.closest(".pl-adaptive");
      var title;

      if ( el.hasClass("pl-compound") ) {
	title = el.data('name')+"/"+el.data('arity');
      } else if ( el.hasClass("pl-list") ) {
	title = el.data('length') + " elements";
	if ( el.data('partial') )
	  title += " (partial)";
      } else if ( el.hasClass("pl-dict") ) {
	title = "Dict";
      } else if ( el.hasClass("pl-binding") ) {
	title = "Binding";
      }

      function item(icon, action, title) {
	return (
	    "<li data-action='"+action+"'>"+
	       "<span class='glyphicon glyphicon-"+icon+"'></span>"+title+
	    "</li>"
	);
      }

      var menu = $("<ul class='pl-compound-menu'>" +
		     "<li><span class='pl-title'>"+title+"</span></li>" +
		     item("heart", "auto", "Smart") +
		     item("triangle-right", "horizontal", "Horizontal") +
		     item("triangle-bottom", "vertical", "Vertical") +
		     item("option-horizontal", "ellipsis", "Ellipsis") +
		     item("copy", "copy", "Copy") +
		   "</ul>"
		  );

      function clickHandler(ev) {
	ev.stopPropagation();
	if ( $(ev.target).closest(".pl-compound-menu").length ) {
	  var action = $(ev.target).data('action');
	  if ( action == "copy" ) {
	    // Avoid ellipsis and possible change in vertical layout
	    var text = el.clone(false)[pluginName]('layout',
						   { layout: 'horizontal',
						     no_ellipsis:true,
						     propagate:true }).text();

	    navigator.clipboard.writeText(text);
	  } else {
	    el[pluginName]('layout', action);
	    el[pluginName]('fit');
	  }
	}

	menu.remove();
	themenu = null;
	$(document).off('click', clickHandler);
      }
      $(document).on('click', clickHandler);

      var offset = funct.offset();
      offset.top += funct.height();

      $("body").append(menu);
      menu.offset(offset);
      themenu = menu;
    },


    activate: function(act) {
      if ( act )
	this.addClass("pl-active");
      else
	this.removeClass("pl-active");
    },

    fit: function() {
      var el = $(this);
      var paren = el.closest(".pl-compound.pl-level-0")
	            .find(".pl-embrace");

      paren[pluginName]('fit_parenthesis');
    },

    /**
     * Called on `.pl-embrace` to adjust the size of the parenthesis
     */

    fit_parenthesis: function() {
      return this.each(function() {
	var el = $(this);
	var paren = el.children(".pl-parenthesis");
	var em = el.children(".pl-embraced");

	if ( em.find(".vertical").length ) {
	  el.addClass("vertical");
	  paren.css("font-size", em.height()*0.58+"px");
	} else {
	  el.removeClass("vertical");
	  paren.css("font-size", "100%");
	}

      });
    }

  }; // methods


  function prepare(el) {
    var active = "span.pl-trigger";

    el.on('click', active, function() {
      if ( !themenu )
	$(this)[pluginName]('menu');
    });

    el.on("mouseover", active, function(e) {
      $(this).parent()[pluginName]('activate', true);
      e.stopPropagation();
    });

    el.on("mouseout", active, function(e) {
      $(this).parent()[pluginName]('activate', false);
    });
  }

  function autoLayout(el, options) {
    el.children().each(function() {
      autoLayout($(this), options);
    });

    if ( el.is(".pl-adaptive") )
    { var layout;

      if ( (layout=el.data('layout')) ) {
	el[pluginName]('layout', layout);
      } else {
	if ( el.width() > (options.width||400) )
	  el[pluginName]('layout', 'vertical');
      }
    }
  }

  /**
   * <Class description>
   *
   * @class pl_term
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.pl_term = function(method) {
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

  $(document).pl_term();
});
