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

	if ( answerHasOutput(answer) )
	  elem.append(renderAnswer(answer));
	else
	  elem.append($.el.span({class: "prolog-true"}, "true"));
      });
    }
  };

  function answerHasOutput(answer) {
    return answer.variables.length > 0 || answer.residuals;
  }

  // FIXME: use laconic to make this more readable and less vulnerable
  function renderAnswer(answer) {
    var html = "";
    var bindings = answer.variables;
    for (var i = 0; i < bindings.length; i++) {
        var vars = bindings[i].variables;
        for (var v = 0; v < vars.length - 1; v++) {
            html += "<span class='pl-ovar'>" + vars[v] + "</span> = " +
                "<span class='pl-var'>" + vars[v + 1] + "</span>, ";
        }
        html += "<span class='pl-ovar'>" + vars[vars.length - 1] + "</span> = ";
        html += bindings[i].value;
        if (bindings[i].substitutions) {
            var substs = bindings[i].substitutions;
            html += ', <span class="pl-comment">% where</span><br/>';
            for (var s = 0; s < substs.length; s++) {
                html += '<span class="where-binding">';
                html += "<span class='pl-var'>" + substs[s].var+"</span> = ";
                html += substs[s].value;
                html += '</span>';
                if (s < substs.length - 1) html += ",<br/>";
            }
        }
        if (i < bindings.length - 1 || answer.residuals) html += ",<br/>";
    }
    if ((residuals = answer.residuals)) {
        for (var i = 0; i < residuals.length; i++) {
            html += residuals[i];
            if (i < residuals.length - 1)
                html += ",<br/>";
        }
    }
    return html;
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
});
