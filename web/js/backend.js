/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2023, SWI-Prolog Solutions b.v.
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
 * Ajax communication with the backend.  This file will allow choosing
 * a different backend and implement fail-over if the current backend
 * is non-functional.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, jan@swi-prolog.org
 * @requires jquery
 */

define([ "jquery", "config", "form", "modal",
	 "laconic"
       ],
       function($, config, form, modal) {

  var backend = {
    backend: config.swish.redis_consumer,
    url: "",

    ajax: function(options) {
      options.url = backend.url + options.url;
      return $.ajax(options);
    },

    selectBackend: function() {
      let body;
      if ( !form )		// cyclic dependency
	form = require("form");
      if ( !modal )		// cyclic dependency
	modal = require("modal");

      function backendBody() {
	let div;
	this.append(div=$.el.div($.el.table(
	  { class: "table table-striped table-condensed",
	    'data-click-to-select': true,
	    'data-single-select': true
	  },
	  $.el.tr($.el.th("Backend"),
		  $.el.th("URL"),
		  $.el.th("Users"),
		  $.el.th("Alive")),
	  body=$.el.tbody())));

	let btn;
	if ( false )		// currently we do not support backend selection
	{ div.append(btn=$.el.button({ name:"select-backend",
				       class:"btn btn-primary disabled"
				     },
				     "Select backend"));
	  $(btn).on("click", function() {
	    const sel = $(body).find("tr.success");
	    if ( sel.length == 1 ) {
	      if ( sel.consumer == config.swish.redis_consumer ) {
		backend.backend = sel.consumer;
		backend.url = ""
	      } else {
		const url = (sel.data("url")||"").replace(/\/+$/, "");

		if ( url ) {
		  backend.backend = sel.consumer;
		  backend.url = url;
		}
	      }
	    }
	    $("#ajaxModal").modal('hide');
	  });
	}

	$(body).on("click", "tr", function(ev) {
	  var tr = $(ev.target).parents("tr");
	  $(body).find("tr.success").removeClass("success");
	  tr.addClass("success");
	  if ( btn )
	  { if ( tr.data("backend") != backend.backend )
	      $(btn).removeClass("disabled");
	    else
	      $(btn).addClass("disabled");
	  }
	});

	function alive(stats) {
	  const ago = new Date().getTime()/1000 - stats.time;
	  if ( ago < 30 )
	    return "\u2714";
	  else if ( ago < 60 )
	    return "\u2b55";
	  else
	    return "\u274c";
	}

	backend.ajax(
	  { url: config.http.locations.backends,
	    success: function(data) {
	      for(let consumer in data)
	      { const stats = data[consumer];
		let row;
		body.append(
		  row = $.el.tr($.el.td(consumer),
				$.el.td(stats.url ? $.el.a({href:stats.url}, stats.url) : "?"),
				$.el.td(stats.local_visitors),
				$.el.td(alive(stats))));
		if ( consumer == backend )
		  $(row).addClass("success");
		$(row).data("backend", consumer);
		$(row).data("url", stats.url);
	      }
	    },
	    error: function(jqXHDR) {
	      modal.ajaxError(jqXHR);
	    }
	  });
      }

      form.showDialog(
	{ title: "SWISH backends",
	  body: backendBody
	});
    }
  }

  return backend;
});
