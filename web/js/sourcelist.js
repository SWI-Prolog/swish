/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2018, VU University Amsterdam
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

define([ "jquery", "config", "form", "modal", "laconic" ],
       function($, config, form, modal) {

(function($) {
  var pluginName = 'sourcelist';

  var current_query = {q:"user:\"me\""};
  var current_profile;
  var query_cache = [];
  var pending = [];
  var qid = 0;

  /** @lends $.fn.sourcelist */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.data(pluginName, data);	/* store with element */
					/* populate search page */
	elem[pluginName]('fill', undefined, current_query);
	elem[pluginName]('check_cache');
	elem[pluginName]('update', current_query);
	elem.on("login", function() {
	  if ( elem[pluginName]('check_cache') )
	    elem[pluginName]('update', current_query);
	});
      });
    },

    check_cache: function() {
      var profile = $("#login").login('get_profile',
				      [ "display_name", "avatar"
				      ]);
      if ( !(current_profile &&
	     current_profile.display_name == profile.display_name &&
	     current_profile.avatar == profile.avatar) ) {
	query_cache = [];
	current_profile = profile;
	return true;
      } else {
	if ( !current_profile )
	  current_profile = profile;
	return false;
      }
    },

    /**
     * Post an update query and process the result
     */
    update: function(query) {
      var elem = this;
      var reply;

      this[pluginName]('check_cache');

      if ( (reply = from_cache(query_cache, query)) ) {
	$.ajax({
	  url: config.http.locations.source_modified,
	  dataType: "json",
	  success: function(json) {
	    if ( json.modified < reply.modified+10 ) {
	      elem.sourcelist('fill', reply, query);
	    } else {
	      query_cache = [];
	      elem[pluginName]('update', query);
	    }
	  },
	  error: function(jqXHDR) {
	    modal.ajaxError(jqXHDR);
	  }
	});
      } else {
	query = query||{};

	$.extend(query, current_profile);
	query.q = query.q||"";
	query.offset = query.offset||0;
	query.limit  = query.limit||10;
	query.qid    = qid++;

	pending.push(query);
	elem[pluginName]('busy', true);

	$.ajax({
	  url: config.http.locations.source_list,
	  data: query,
	  dataType: "json",
	  success: function(reply) {
	    reply.query = query;
	    pending.pop();		/* should match qid */
	    if ( pending.length == 0 )
	      elem[pluginName]('busy', false);
	    add_to_cache(query_cache, reply);
	    elem.sourcelist('fill', reply, query);
	  },
	  error: function(jqXHDR) {
	    pending.pop();
	    modal.ajaxError(jqXHDR);
	  }
	});
      }
    },

    /**
     * Go to a page
     */

    page: function(move) {
      var data = this.data(pluginName);

      if ( data && data.page ) {
	var q = $.extend({}, data.page.query);

	if ( q.offset == undefined )
	  q.offset = 0;

	function roundUp(v, n) {
	  return Math.floor((v+(n-1))/n) * n;
	}

	switch(move) {
	  case "first": q.offset  = 0; break;
	  case "prev":  q.offset -= data.page.size; break;
	  case "next":  q.offset += data.page.size; break;
	  case "last":  q.offset  = roundUp(data.page.total, data.page.size) -
				    data.page.size; break;
	  default: return;
	}

	q.offset = Math.max(0, q.offset);
	this[pluginName]('update', q);
      }
    },

    /**
     * Fill the result table
     */
    fill: function(results, query) {
      var data = this.data(pluginName);
      var body;

      if ( !data )				/* has gone */
	return this;

      if ( results ) {
	current_query = query;
	data.page = { query:  query,
		      offset: query.offset,
		      size:   query.limit,
		      total:  results.total
		    };
      }

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
		    $.el.div({class:"search-results"},
		      table =
		      $.el.table({class:"table table-striped table-hover "+
					"table-condensed"},
				 $.el.thead($.el.tr(h("Type"),
						    h("Name"),
						    h("Tags"),
						    h("User"),
						    h("Modified"))),
				 body = $.el.tbody()),
		      $.el.div({class:"search-no-results"}),
		      $.el.div({class:"loading search"})),
		    $.el.div({class:"search-footer"}));
	this[pluginName]('search_form');
	body = $(body);
	body.on("click", "tr", function(ev) {
	  var tr = $(ev.target).closest("tr");
	  $("body").swish('playFile', { file:tr.attr("data-name") });
	});
      } else {
	$(body).html("");
      }

      // set the query, unless we are typing one
      var input = this.find("input.search");
      if ( !input.is(":focus") ) {
	input.val(results ? results.query.q : query ? query.q : "");
	input.trigger("propertychange", false);
      }

      if ( results ) {
	var i = query.offset - results.query.offset;
	var e = Math.min(i+query.limit, results.matches.length);

	if ( i<e )
	  $(table).show();
	else
	  $(table).hide();

	for(; i<e; i++)
	{ var match = results.matches[i];
	  var ext   = match.name.split(".").pop();
	  var base  = match.name.slice(0, -(ext.length+1));

	  body.append($.el.tr({"data-name":match.name},
			      $.el.td(form.widgets.typeIcon(ext)),
			      $.el.td(base),
			      $.el.td((match.tags||[]).join(" ")),
			      $.el.td(match.author),
			      $.el.td(humanize(match.time))));
	}
	this[pluginName]('search_footer', results, query);
      }
    },

    search_footer: function(results, query) {
      var footer = this.find("div.search-footer");
      var noresults = this.find("div.search-no-results");
      var bopts = {};

      function btn(action, dir, icon) {
	bopts.action = action;
	bopts.class  = "btn-primary "+dir;
	return form.widgets.glyphIconButton(icon, bopts);
      }

      if ( footer.find(".f-total").length == 0 ) {
	footer.append(btn("first", "backward", "fast-backward"),
		      btn("prev",  "backward", "step-backward"),
		      $.el.button({class:"btn btn-default"},
				  $.el.span({class: "f-from"}),
				  $.el.label("to"),
				  $.el.span({class: "f-to"}),
				  $.el.label("from"),
				  $.el.span({class: "f-total"})),
		      btn("next", "forward", "step-forward"),
		      btn("last", "forward", "fast-forward"));

	footer.on("click", "button", function(ev) {
	  var b   = $(ev.target).closest("button");
	  var act = b.data('action');

	  if ( act )
	    b.closest("div.sourcelist")[pluginName]("page", act)
	});
      }

      var end = Math.min(query.offset+query.limit, results.total);

      if ( results.total == 0 ) {
	if ( noresults.find("div").length == 0 ) {
	  var a;
	  noresults.append(
	    $.el.div($.el.span({class:"no-search-results-warning"},
			       form.widgets.glyphIcon("alert"),
			       " No matching files"), $.el.br(),
		     "If you are a new user you may",
		     $.el.ul($.el.li("Use the Examples menu from the navigation bar"),
			     $.el.li("Use the Program or Notebook button above")),
		     $.el.div(a=$.el.a({href:"#"}, "help on search"))));
	  $(a).on("click", function() {
	    console.log("help");
	    modal.help({file:"sourcelist.html"});
	  });
	}
	noresults.show();
	footer.hide();
      } else
      { noresults.hide();

	if ( query.offset > 0 || end < results.total ) {
	  footer.show();
	  if ( query.offset == 0 ) {
	    footer.find(".backward").attr("disabled", "disabled");
	  } else {
	    footer.find(".backward").removeAttr("disabled");
	  }
	  if ( end >= results.total ) {
	    footer.find(".forward").attr("disabled", "disabled");
	  } else {
	    footer.find(".forward").removeAttr("disabled");
	  }
	  footer.find(".f-from") .text(""+query.offset);
	  footer.find(".f-to")   .text(""+end);
	  footer.find(".f-total").text(""+results.total);
	} else {
	  footer.hide();
	}
      }
    },

    search_form: function() {
      var data = this.data(pluginName);
      var elem = this;
      var div = this.find("div.search-form");
      var btnsubmit;

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
	  var a;

	  if ( typeof(item) == "string" ) {
	    return $.el.a({'data-tag':item}, item);
	  } else if ( item.i) {
	    a = $.el.a({'data-tag':item.t, 'data-value':item.v},
		       form.widgets.typeIcon(item.i), " "+item.l);
	  } else {
	    a = $.el.a({'data-tag':item.t, 'data-value':item.v},
		       item.l);
	  }
	  $(a).data('quote', item.q == undefined ? "\"" : item.q);

	  return a;
	}

	for(var i=0; i<members.length; i++) {
	  $(ul).append($.el.li(add(members[i])));
	}

	return div;
      }

      function resettimeout(set) {
	if ( data.tmo ) {
	  clearTimeout(data.tmo);
	  data.tmo = undefined;
	}
	if ( set == true )
	  set = 1000;
	if ( set )
	  data.tmo = setTimeout(submit, set);
      }

      function submit(ev) {
	if ( ev )
	  ev.preventDefault();
	resettimeout();
	var q = elem.find("input").val();
	elem[pluginName]('update', {q:q});
	return false;
      }

      div.append(
	$.el.div({class:"form-group has-feedback has-clear"},
		 $.el.input({
		   type: "text",
		   class: "form-control search",
		   placeholder: "Find files"
		 }),
		 $.el.span({class:"form-control-clear glyphicon "+
				  "glyphicon-remove form-control-feedback "+
				  "hidden"})),
	$.el.div({ class: "input-group-btn" },
		 btn("Filter", [{t:"user", l:"My files",        v:"me", q:"\""},
				{t:"user", l:"By user",         v:"",   q:"\""},
				{t:"user", l:"By user (regex)", v:"",   q:"/"},
				{t:"tag",  l:"By tag",          v:"",   q:"\""},
				{t:"tag",  l:"By tag (regex)",  v:"",   q:"/"},
				{t:"name", l:"By name",         v:"",   q:"\""},
				{t:"name", l:"By name (regex)", v:"",   q:"/"}
			       ]),
		 btn("Type",   [{t:"type", l:"Program",   i:"pl",    v:"pl",    q:""},
				{t:"type", l:"Notebook",  i:"swinb", v:"swinb", q:""},
				{t:"type", l:"Permalink", i:"lnk",   v:"lnk",   q:""}
			       ]),
		 btnsubmit=
		 $.el.button({class:"btn btn-default", type:"submit"},
			     $.el.i({class:"glyphicon glyphicon-search"}))));

      form.dyn_clear(div, submit);

      div.on("click", "a", function(ev) {
	var a = $(ev.target).closest("a");

	function tag(tag, value, q) {
	  var input = div.find("input");
	  var val = input.val();
	  var tagv = tag + ":" + q + (value||"") + q;

	  if ( val.trim() == "" ) {
	    val = tagv;
	  } else {
	    if ( value && RegExp("\\b"+tag+":").test(val) ) {
	      val = val.replace(RegExp("\\b"+tag+":(\\S*|\\s*\"[^\"]*\")"), tagv);
	    } else {
	      val = val.trim() + " " + tagv;
	    }
	  }

	  input.val(val).trigger('propertychange');
	  if ( value || tag == "tag" )
	    submit();
	}

	tag(a.data('tag'), a.data('value'), a.data('quote'));
      });

      $(btnsubmit).on("click", function(ev) {
	return submit(ev);
      });

      var inputel = elem.find("input");
      inputel.keydown(function(ev) {
	if ( ev.which == 13 )
	  return submit(ev);
      }).on("input propertychange", function(ev, propagate) {
	if ( propagate != false ) {
	  if ( from_cache(query_cache, inputel.val()) ) {
	    resettimeout(200);
	  } else
	    resettimeout(true);
	}
      });
    },

    busy: function(busy) {
      var div = this.find("div.loading");

      if ( busy )
	div.show();
      else
	div.hide();
    }
  }; // methods


  /**
   * Cache management.  These functions should eventually merge results
   * and select sub-results without contacting the server.
   */
  function from_cache(cache, query) {
    function qmatch(entry) {
      var e = entry.query;
      if ( query.q == e.q ) {
	if ( query.offset >= e.offset &&
	     (query.offset+query.limit <= e.offset + entry.matches.length ||
	      e.offset + entry.matches.length == entry.total) )
	  return e;
	}
    }

    if ( query != undefined ) {
      query.offset = query.offset || 0;
      query.limit  = query.limit  || 10;

      for(var i=cache.length-1; i>=0; i--) {
	var entry = cache[i];
	if ( qmatch(entry) )
	  return entry;
      }
    }
  }

  function add_to_cache(cache, result) {
    var qr = result.query;

    qr.offset = qr.offset || 0;
    qr.limit  = qr.limit  || 10;

    for(var i=cache.length-1; i>=0; i--) {
      var entry = cache[i];
      var qc = entry.query;

      if ( qc.q == qr.q ) {
	if ( qc.offset + entry.matches.length == qr.offset ) {
	  for(var i=0; i<result.matches.length; i++)
	    entry.matches.push(result.matches[i]);
	  return;
	}
      }
    }

    cache.push(result);
  }

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
