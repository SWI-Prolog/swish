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
 * List available sources.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "form", "laconic" ],
       function($, config, form) {

(function($) {
  var pluginName = 'sourcelist';

  /** @lends $.fn.sourcelist */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem[pluginName]('update');

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * Post an update query and process the result
     */
    update: function(query) {
      var elem = this;

      $.ajax({
        url: config.http.locations.source_list,
	data: query||{},
	dataType: "json",
	success: function(reply) {
	  elem.sourcelist('fill', reply);
	},
	error: function(jqXHDR) {
	  modal.ajaxError(jqXHR);
	}
      });
    },

    fill: function(data) {
      var body;

      function h(title) {
	return $.el.th(title);
      }

      function humanize(stamp) {
	var d = new Date(stamp*1000);
	var s = d.toISOString();

	return s.slice(0, 10) + " " + s.slice(11,19);
      }

      body = this.find("tbody");
      if ( body.length == 0 ) {
	this.append($.el.div({class:"search-form input-group"}),
		    table =
		    $.el.table({class:"table table-striped table-hover "+
				      "table-condensed"},
			       $.el.thead($.el.tr(h("Type"),
						  h("Name"),
						  h("Tags"),
						  h("User"),
						  h("Modified"))),
			       body = $.el.tbody()));
	this[pluginName]('search_form');
	body = $(body);
      } else {
	$(body).html("");
      }

      for(var i=0; i<data.matches.length; i++)
      { var match = data.matches[i];
	var ext   = match.name.split(".").pop();
	var base  = match.name.slice(0, -(ext.length+1));

	body.append($.el.tr({"data-name":match.name},
			    $.el.td(form.widgets.typeIcon(ext)),
			    $.el.td(base),
			    $.el.td((match.tags||[]).join(" ")),
			    $.el.td(match.author),
			    $.el.td(humanize(match.time))));
      }

      this.find("table").on("click", "tr", function(ev) {
	var tr = $(ev.target).closest("tr");
	$("body").swish('playFile', { file:tr.attr("data-name") });
      });
    },

    search_form: function() {
      var elem = this;
      var div = this.find("div.search-form");
      var submit;

      function btn(title, members) {
	var ul;
	var div = $.el.div({class:"btn-group"},
		    $.el.button({ type:"button",
			          class:"btn btn-default dropdown-toggle",
			          'data-toggle':"dropdown",
			          'aria-haspopup': true,
			          'aria-expanded': false
			        },
				title, " ",
				$.el.span({class:"caret"})),
		    ul=$.el.ul({class:"dropdown-menu"}));

	function add(item) {
	  if ( title == "Type" )
	    return $.el.a({'data-tag':"type", 'data-value':item},
			  form.widgets.typeIcon(item), " ."+item);
	  else
	    return $.el.a({'data-tag':item}, item);
	}

	for(var i=0; i<members.length; i++) {
	  $(ul).append($.el.li(add(members[i])));
	}

	return div;
      }

      div.append(
	$.el.input({ type: "text",
		     class: "form-control search",
		     placeholder: "Search sources"
		   }),
	$.el.div({ class: "input-group-btn" },
		 btn("Filter", ["name", "user", "tag"]),
		 btn("Type",   ["pl", "swinb", "lnk"]),
		 submit=
		 $.el.button({class:"btn btn-default", type:"submit"},
			     $.el.i({class:"glyphicon glyphicon-search"}))));

      div.on("click", "a", function(ev) {
	var a = $(ev.target).closest("a");

	function tag(tag, value) {
	  var input = div.find("input");
	  var val = input.val();

	  if ( val.trim() == "" )
	    val = tag+":";
	  else
	    val = val.trim() + " " + tag + ":";

	  val += value||"";

	  input.val(val);
	}

	tag(a.data('tag'), a.data('value'));
      });
      $(submit).on("click", function(ev) {
	var q = elem.find("input").val();
	ev.preventDefault();
	elem[pluginName]('update', {q:q});
	return false;
      });
    }
  }; // methods


  /**
   * List available sources.
   *
   * @class sourcelist
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.sourcelist = function(method) {
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
