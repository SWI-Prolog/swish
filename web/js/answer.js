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
	  if ( elem.is("table") ) {
	    var row = $.el.tr();
	    elem.append(row);
	    row.innerHTML = renderTabledAnswer(answer, elem);
	    evalScripts($(row));
	    $(row).find(".render-multi").renderMulti();
	  } else {
	    elem[0].innerHTML = renderAnswer(answer);
	    evalScripts(elem);
	    elem.find(".render-multi").renderMulti();
	  }
	} else
	  elem.append($.el.span({class: "prolog-true"}, "true"));
      });
    }
  };

  function answerHasOutput(answer) {
    return answer.variables.length > 0 || answer.residuals;
  }

  function renderSubstitutions(substs, html) {
    html.push(', <span class="pl-comment">% where</span><br/>');
    for (var s = 0; s < substs.length; s++) {
      html.push('<span class="where-binding">',
		"<span class='pl-var'>", substs[s].var+"</span> = ",
		substs[s].value, '</span>');
      if (s < substs.length - 1)
	html.push(",<br/>");
    }
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
	renderSubstitutions(bindings[i].substitutions, html);
      }
      if (i < bindings.length - 1 || answer.residuals)
	html.push(",<br/>");
    }

    var residuals;
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
   * Render answer as a new row to the answer table.
   * @param {Answer} answer represents an answer to a Prolog query
   * @param {Table} table is the jQuery table to which the answer must
   * be added.
   */
  function renderTabledAnswer(answer, table) {
    var html = [];

    function findBinding(name) {
      var bindings = answer.variables;
      for (var i = 0; i < bindings.length; i++) {
	var vars = bindings[i].variables;
	for (var v = 0; v < vars.length; v++) {
	  if ( vars[v] == name )
	    return bindings[i];
	}
      }
      return null;
    }

    for(var i = 0; i<answer.projection.length; i++) {
      var vname  = answer.projection[i];
      var binding = findBinding(vname);

      html.push("<td>");
      if ( binding ) {
	html.push(binding.value);
	if ( binding.substitutions )
	  renderSubstitutions(binding.substitutions, html);
      } else {
	html.push("<span class='pl-var'>", vname, "</span>");
      }
      html.push("</td>");
    }

    function ensureResidualColumn() {
      if ( table.find("tr.projection th.residuals").length == 0 ) {
	$("<th class='residuals'>Residual goals</th>").insertBefore(
	   table.find("tr.projection th.answer-nth"));
	$("<td></td>").insertBefore(
	   table.find("tr td.answer-nth"));
      }
    }

    var residuals;
    if ((residuals = answer.residuals)) {
      ensureResidualColumn();
      html.push("<td>");
      for (var i = 0; i < residuals.length; i++) {
	html.push(residuals[i]);
	if (i < residuals.length - 1)
	  html.push(",<br/>");
      }
      html.push("</td>");
    }

    if ( answer.nth )
      html.push("<td class='answer-nth'>", answer.nth, "</td>");

    return html.join("");
  }

  /**
   * Execute scripts that are embedded in the jQuery object elem.
   * While executing a script, the property `$.ajaxScript` points
   * to the executing script to enable the script to find elements
   * in the ajax DOM extension in which the script is embedded.
   * @param {jQuery} elem is the set in which scripts are searched
   * and executed.
   */
  function evalScripts(elem) {
    elem.find("script").each(function() {
      var type = this.getAttribute('type')||"text/javascript";
      if ( type == "text/javascript" ) {
	$.ajaxScript = $(this);
	eval(this.textContent);
      }
    });
    if ( $.ajaxScript )
      delete $.ajaxScript;
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
  var timeout = 0;
  var hovering = false;

  /** @lends $.fn.renderMulti */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {current: 0};		/* private data */
	var display = [];
	var selector = $.el.div({class: "render-multi-active"});

	var i = 0;
	elem.children().each(function() {
	  var how = $(this).css("display");

	  display.push(how);
	  if ( i++ == 0 ) {
	    elem.css("display", how);
	    $(this).attr('draggable', false);
	  } else {
	    $(this).hide();
	  }
	});
	data.display = display;
	elem.append(selector);

	$(selector).hover(function(ev) { elem.renderMulti('showSelect', ev); },
			  function(ev) { elem.renderMulti('hideSelect', ev); });
	elem.attr('draggable', true)
            .bind('dragstart', dragStart);

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * @returns {String} holding HTML with a radio button to select a
     * rendering
     */
    selectMenu: function() {
      var data = this.data(pluginName);
      var select = ["<label>View as</label><br>"];
      var children = this.children();

      function downloadButton(i, name) {
	var title, glyph;

	if ( name == "Prolog term" ) {
	  title = "Copy";
	  glyph = "copy";
	} else {
	  title = "Download";
	  glyph = "download";
	}

	btn = '<a href="#" class="btn btn-style btn-sm" '+
	      'data-nr="'+i+'" data-action="'+glyph+'" title="'+title+'">' +
	      '<span class="glyphicon glyphicon-'+glyph+'"></span></a>';

	return btn;
      }

      var i = 0;
      for(var i=0; i<data.display.length; i++) {
	var r = $(children[i]);
	var name = r.attr("data-render");

	if ( !name ) {
	  if ( i == 0 )
	    name = "Default rendered";
	  else
	    name = "Alt rendered ["+(i+1)+"]";
	}

	select.push("<div class='render-item'>",
		    downloadButton(i, name),
		    "<input type='radio' name='render' value='", i, "'");
	if ( i == data.current ) select.push(" checked");
	select.push("> ", name, "</div>");
      }

      select.push("</form");
      return select.join("");
    },

    showSelect: function(ev) {
      var elem = this;
      var menu = selectMenu();
      var pos  = this.offset();
      var target;

      hovering = true;
      if ( timeout ) {
	clearTimeout(timeout);
	timeout = 0;
      }

      if ( (target=menu.data("target")) )
	target.removeClass("render-selecting");
      menu.data("target", elem);

      menu.html(this.renderMulti('selectMenu'));
      menu.css({ top:      pos.top + 5 + "px",
                 left:     pos.left + 5 + "px"
               }).show(400);

      this.addClass("render-selecting");
    },

    hideSelect: function(ev) {
      resetHover();
    },

    /**
     * Select the i-th (0-based) rendering alternative
     * @param {Integer} i denotes the alternative
     */
    select: function(i) {
      var data  = this.data(pluginName);

      if ( data.current != i ) {
	var child = this.children();
	var how   = data.display[i];

	$(child[data.current]).hide(400);
	$(child[i]).show(400, function() { $(this).css("display", how); });
	this.css("display", how);
	if ( $(child[i]).is("span.render-as-prolog") ) {
	  this.attr("draggable", false);
	} else {
	  this.attr("draggable", true);
	}

	data.current = i;
      }

      closeSelectMenu();
    },

    copy: function(i) {
      var child = this.children();
      var data  = this.data(pluginName);
      var old   = data.current;

      function selectElementText(el) {
	var range = document.createRange();
	range.selectNodeContents(el);
	var selection = window.getSelection();
	selection.removeAllRanges();
	selection.addRange(range);
      }

      this.renderMulti('select', i);
      selectElementText(child[i]);
      try {
	document.execCommand("copy");
      } catch(e) {
	alert("Sorry, cannot copy text with this browser");
      }
      this.renderMulti('select', old);

      return this;
    },

    /**
     * Download a rendered object.  The renderer can interact with this
     * code by setting a class `export-dom` and an event-handler for the
     * event `export-dom`. This handler is passed a plain object, for
     * which is must set the properties `element`, `extensions` and
     * `contentType`
     */
    download: function(i) {
      var child = this.children();
      var node  = $(child[i]);
      var ext   = "html";
      var data;

      function aSupportsDownload() {
	return $("<a>")[0].download != undefined;
      }

      if ( node.hasClass("export-dom") ) {
	var r = {};
	node = node.trigger("export-dom", r);
	if ( r.element ) {
	  data = r.element.outerHTML;
	  ext  = r.extension||"html";
	  type = r.contentType||"text/html";
	} else {
	  alert("Failed to export rendered result");
	}
      } else if ( node.find("svg").length == 1 ) {
	var svg = node.find("svg");
	if ( !svg.attr("xmlns") )
	  svg.attr("xmlns", "http://www.w3.org/2000/svg");
	data = svg[0].outerHTML
	ext  = "svg";
	type = "image/svg+xml";
      } else {
	data = node.html();
	type = "text/html";
      }

      if ( !aSupportsDownload() )
	type = "application/octet-stream";

      var href	= "data:"+type+";charset=UTF-8,"
		+ encodeURIComponent(data);

      var a = $.el.a({ href:href,
		       download:"swish-rendered."+ext
		     });
      this.append(a);
      a.click();
      $(a).remove();

      return this;
    },

    /**
     * @return {String} native Prolog text for a multi-rendered block
     */
    prologText: function() {
      return this.find("span.render-as-prolog").text();
    }
  }; // methods


  function selectMenu() {
    var menu = $("#render-select");

    if ( !menu[0] ) {
      menu = $($.el.form({ id:"render-select",
                           style:"display:none"
		         }));

      menu.on("click", "a", function(ev) {
	var a = $(ev.target).closest("a");
	var i = a.data("nr");

	menu.data("target").renderMulti(a.data("action"), i);
	return false;
      });
      menu.on("click", function() {
	var r = $("input[name=render]:checked", $(this)).val();
	menu.data("target").renderMulti('select', parseInt(r));
      });
      menu.hover(function() { hovering = true; startMenuTimeout(); },
		 function() { resetHover(); });

      $("body").append(menu);
    }

    return menu;
  }

  function closeSelectMenu() {
    if ( !hovering ) {
      var menu = selectMenu();
      var target = menu.data("target");

      if ( target ) {
	target.removeClass("render-selecting");
	menu.data("target", null);
      }
      menu.hide(400);
    }
  }

  function startMenuTimeout() {
    timeout = setTimeout(function() {
      closeSelectMenu();
    }, 400);
  }

  function resetHover() {
    hovering = false;
    startMenuTimeout();
  }

  function dragStart(ev) {
    var dt = ev.originalEvent.dataTransfer;
    dt.setData("Text", $(ev.target).renderMulti('prologText'));
    return true;
  }

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
