/**
 * @fileOverview
 * Render a single Prolog answer.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "laconic" ],
       function() {

		 /*******************************
		 *	RENDER AN ANSWER	*
		 *******************************/

(function($) {
  var pluginName = 'prologAnswer';

  /** @lends $.fn.prologAnswer */
  var methods = {
    /**
     * Represent the binding of one or more variables to exactly the
     * same (==) Prolog term.
     *
     * @typedef {Object} Binding
     * @property {Array.String} variables represents the names of the
     * variables.  This array is at least one long.
     * @property {String} value contains the HTML that describes the
     * binding of the variable.
     */

    /**
     * Represent the binding of a single variable used to represent
     * sharing, an in particular cyclic terms
     *
     * @typedef {Object} Subsitution
     * @property {String} var name of the variable
     * @property {String} value contains the HTML that describes the
     * binding of the variable.
     */

    /**
     * Represent an answer as represented by the pengines `json-html`
     * format.
     * @typedef {Object} Answer
     * @property {Array.Binding} variables represents the variable
     * bindings.
     * @property {Array.Subsitution} [substitutions] represents substitutions
     * needed to break cyclic terms.
     * @property {Array.String} [residuals] represents residual goals as HTML
     * strings.
     */

    /**
     * Render a single answer as returned by pengines `json-html` format
     * as an HTML string.
     *
     * to HTML escaping issues
     * @param {Answer} answer represents an answer to a Prolog query
     */
  _init: function(answer) {
      return this.each(function() {
	var elem = $(this);

	if ( answerHasOutput(answer) ) {
	  elem.append(renderAnswer(answer));
	  elem.find(".render-multi").renderMulti();
	} else
	  elem.append($.el.span({class: "prolog-true"}, "true"));
      });
    }
  };

  function answerHasOutput(answer) {
    return answer.variables.length > 0 || answer.residuals;
  }

  function renderAnswer(answer) {
    var html = [];
    var bindings = answer.variables;
    for (var i = 0; i < bindings.length; i++) {
        var vars = bindings[i].variables;
        for (var v = 0; v < vars.length - 1; v++) {
	    html.push("<span class='pl-ovar'>", vars[v], "</span> = ",
		      "<span class='pl-var'>", vars[v + 1], "</span>, ");
        }
	html.push("<span class='pl-ovar'>", vars[vars.length - 1],
		  "</span> = ", bindings[i].value);
        if (bindings[i].substitutions) {
            var substs = bindings[i].substitutions;
	    html.push(', <span class="pl-comment">% where</span><br/>');
            for (var s = 0; s < substs.length; s++) {
	        html.push('<span class="where-binding">',
			  "<span class='pl-var'>", substs[s].var+"</span> = ",
			  substs[s].value, '</span>');
                if (s < substs.length - 1)
		    html.push(",<br/>");
            }
        }
        if (i < bindings.length - 1 || answer.residuals)
	  html.push(",<br/>");
    }
    if ((residuals = answer.residuals)) {
        for (var i = 0; i < residuals.length; i++) {
            html.push(residuals[i]);
            if (i < residuals.length - 1)
                html.push(",<br/>");
        }
    }
    return html.join("");
  }

  /**
   * Render a single Prolog answer. This class is the entry point for
   * more flexible answer rendering.
   *
   * @class prologAnswer
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} answer Either a method name or the jQuery
   * plugin initialization object, which is the answer to a Prolog query
   * in pengines "json-html" format
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.prologAnswer = function(method) {
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

		 /*******************************
		 *	   RENDER TERMS		*
		 *******************************/

(function($) {
  var pluginName = 'renderMulti';

  /** @lends $.fn.renderMulti */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = { current: 0};		/* private data */
	var select = ["<form class='render-select' style='display:none'>" +
		      "<label>View as</label><br>"
		     ];
	var i = 0;

	elem.children().each(function() {
	  var r = $(this);
	  var name = r.attr("data-render") || ("unknown "+i);

	  r.wrap("<div class='render-wrapper'></div>");
	  var wrapper = r.parent();
	  if ( i == 0 ) {
	    wrapper.css("display", r.css("display"));
	    elem.css("display", r.css("display"));
	  } else {
	    select.push("<br>");
	    wrapper.css("display", "none");
	  }
	  select.push("<input type='radio' name='render' value='", i, "'");
	  if ( i == 0 ) select.push(" checked");
	  select.push("> ", name);
	  i++;
	});
	select.push("</form");
	elem.append(select.join(""));
	elem.hover(function(ev) { elem.renderMulti('showSelect', ev); },
		   function()   { elem.renderMulti('hideSelect'); });
	elem.find(".render-select").on("click", function() {
	  var r = $("input[name=render]:checked",
		    elem.find(".render-select")).val();
	  elem.renderMulti('select', parseInt(r));
	});

	elem.data(pluginName, data);	/* store with element */
      });
    },

    showSelect: function(ev) {
      this.find(".render-select").css("display", "block");
    },

    hideSelect: function() {
      this.find(".render-select").css("display", "none");
    },

    /**
     * Select the i-th (0-based) rendering alternative
     * @param {Integer} i denotes the alternative
     */
    select: function(i) {
      var data  = this.data(pluginName);
      var child = this.children();
      var how   = $($(child[i]).children()[0]).css("display");

      $(child[data.current]).css("display", "none");
      $(child[i]).css("display", how);
      this.css("display", how);

      data.current = i;
      this.renderMulti('hideSelect');
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class renderMulti
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.renderMulti = function(method) {
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
