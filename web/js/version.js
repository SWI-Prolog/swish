/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2018, VU University Amsterdam
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
 * Provide version and release info
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "utils", "laconic" ],
       function($, config, utils) {

(function($) {
  var pluginName = 'version';

  /** @lends $.fn.version */
  var methods = {
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	if ( config.http.locations.versions ) {
	  elem.append($.el.div({class:"version"},
			       $.el.div({class:"v-swish"}),
			       $.el.div({class:"v-changelog"},
					$.el.table()),
			       $.el.div({class:"v-prolog"})));

	  elem[pluginName]('update');
	  if ( options.commit )
	    elem[pluginName]('changelog', options);
	}

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * Update the SWISH and Prolog versions.
     */
    update: function() {
      if ( config.http.locations.versions ) {
	elem = this;

	$.get(config.http.locations.versions,
	      function(data) {
		if ( !data.swish || !data.prolog ) {
		  console.log(data);
		  return;
		}

		var swishversion;

		if ( elem.hasClass("v-compact") )
		  swishversion = $.el.a({title: "View recent changes"},
					data.swish.version);
		else
		  swishversion = $.el.span(data.swish.version);

		elem.find(".v-swish")
		    .append($.el.span($.el.a({class:"v-product",
					      href:"https://swish.swi-prolog.org"},
					     "SWISH"),
				      " version ",
				      swishversion));
		elem.find(".v-prolog")
		    .append($.el.span("Running on ",
				      $.el.a({class:"v-product",
					      href:"http://www.swi-prolog.org/"},
					     data.prolog.brand),
				      " version " +
				      data.prolog.version));
		if ( elem.hasClass("v-compact") ) {
		  $(swishversion).on("click", function(ev) {
		    if ( elem.hasClass("v-compact") ) {
		      elem[pluginName]('versionDetails');
		      ev.preventDefault();
		      return false;
		    }
		  });
		}
	      });
      }
    },

    versionDetails: function() {
      var body = this.closest(".modal-body");

      if ( body ) {
	this.closest(".modal-content").find("h2").html("SWISH ChangeLog");

	this.detach();
	body.empty();
	body.append(this);
	this.removeClass("v-compact");
	this[pluginName]('changelog');
      }
    },

    /**
     * Get a changelog
     */
    changelog: function(options) {
      var that = this;
      options = options||{};
      var params = {};

      params.show = options.show || "all";
      if ( options.commit ) {
	params.commit = options.commit;
      } else {
	params.last = options.last || 20;
      }

      this.find(".v-changelog > table").html("");
      $.get(config.http.locations.changelog,
	    params,
	    function(data) {

	      for(var i=0; i<data.changelog.length; i++) {
		that[pluginName]('addChange', data.changelog[i], i);
	      }
	    });
    },

    addChange: function(ch, i) {
      var desc = $.el.td({class:"v-description", colspan:3});
      $(desc).html(ch.message);

      var cls = (i%2 == 0 ? "even" : "odd");

      this.find(".v-changelog > table")
	  .append($.el.tr({class:"v-change-header "+cls},
			  $.el.td({class:"v-author"}, ch.author),
			  $.el.td({class:"v-commit"}, ch.commit.slice(0,7)),
			  $.el.td({class:"v-date"}, ch.committer_date_relative)),
		  $.el.tr({class:"v-change-body "+cls},
			  desc));
    },

    /**
     * Check whether the server was updated since the last time we
     * viewed the changes.
     */
    checkForUpdates: function() {
      if ( !config.http.locations.versions )
	return;

      var str = localStorage.getItem("last-version");

      function saveCheckpoint(data) {
	var last = { commit:data.commit, date: data.date };
	localStorage.setItem("last-version", JSON.stringify(last));
      }

      if ( str && (last = JSON.parse(str)) && last.commit ) {
	var title = "SWISH updates since " + utils.ago(last.date||0);

	$.get(config.http.locations.changes,
	      {commit:last.commit},
	      function(data) {
		if ( data.changes ) {
		  $("#swish-updates")
		    .css("display", "inline-block")
		    .attr("title", "SWISH has received " +
				   data.changes + " updates\n" +
			           "Click for details")
		    .on("click", function(ev) {
		      $(ev.target).closest(".swish")
			          .swish('showUpdates',
					 { title:  title,
					   commit: last.commit,
					   show:   "tagged"
					 });
		      saveCheckpoint(data);
		      $("#swish-updates").hide();
		    });
		}
	      });
      } else {
	$.get(config.http.locations.changes,
	      function(data) {
		saveCheckpoint(data);
	      });
      }
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class version
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.version = function(method) {
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
