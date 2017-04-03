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
 * Dialog components to interact with the gitty store.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "form", "modal", "laconic" ],
       function($, config, form, modal) {

(function($) {
  var pluginName = 'gitty';

  /** @lends $.fn.gitty */
  var methods = {
    /**
     * @param {Object} options
     * @param {Object.meta} provides the gitty meta-data
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName)||{};
	var meta = options.meta;
	var tabs;

	data.commits = [];
	data.commits[meta.commit] = meta;
	data.commit  = meta.commit;
	data.editor  = options.editor;

	function tab(label, active, id, disabled) {
	  var attrs = {role:"presentation"};
	  var classes = [];
	  if ( active   ) classes.push("active");
	  if ( disabled ) classes.push("disabled");
	  if ( classes != [] )
	    attrs.class = classes.join(" ");
	  var elem =
	  $.el.li(attrs, $.el.a({href:"#"+id, 'data-toggle':"tab"}, label));
	  return elem;
	}

	henabled = !Boolean(meta.previous);
	tabs     = $($.el.div({class:"tab-content"}));

	elem.append($.el.ul(
	  {class:"nav nav-tabs"},
	  tab("Meta data", true,  "gitty-meta-data"),
	  tab("History",   false, "gitty-history",  henabled),
	  tab("Changes",   false, "gitty-diff",     henabled)));
	elem.append(tabs);

	/* meta-data tab */
	tabs.append($.el.div({ class:"tab-pane fade in active gitty-meta-data",
	                       id:"gitty-meta-data"}));
	elem.find('[href="#gitty-meta-data"]').on("show.bs.tab", function(ev) {
	  elem.gitty('showMetaData');
	});

	/* history tab */
	tabs.append($.el.div({ class:"tab-pane fade gitty-history",
	                       id:"gitty-history"}));
	elem.find('[href="#gitty-history"]').on("show.bs.tab", function(ev) {
	  elem.gitty('showHistory');
	});

	/* diff/changes tab */
	tabs.append($.el.div({ class:"tab-pane fade gitty-diff",
	                       id:"gitty-diff"}));
	elem.find('[href="#gitty-diff"]').on("show.bs.tab", function(ev) {
	  elem.gitty('showDiff');
	});

	elem.data(pluginName, data);

	elem.gitty('showMetaData');
      });
    },

    /**
     * @param is the gitty meta-object
     * @return {DOM} node holding the title
     */
    title: function(meta) {
      var title = $.el.span("File ", $.el.span({class:"filename"}, meta.name));
      if ( meta.symbolic != "HEAD" && meta.commit )
	$(title).append("@", $.el.span({class:"sha1 abbrev"},
				       meta.commit.substring(0,7)));

      return title;
    },


		 /*******************************
		 *	     META DATA		*
		 *******************************/

    /**
     * Show meta data for the current version.  If this is the HEAD,
     * allow updating the meta-data
     */
    showMetaData: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName);
	var tab  = elem.find(".gitty-meta-data");
	var formel;
	var meta = data.commits[data.commit];

	if ( data.metaData == data.commit )
	  return;
	data.metaData = data.commit;

	tab.html("");
	formel = $.el.form({class:"form-horizontal"},
		      form.fields.fileName(meta.name, meta.public, meta.example,
					   true), // disabled
		      form.fields.title(meta.title),
		      form.fields.author(meta.author),
		      form.fields.date(meta.time, "Date", "date"),
		      form.fields.tags(meta.tags));

	if ( meta.symbolic == "HEAD" ) {
	  $(formel).append(
	      form.fields.buttons(
		{ label: "Update meta data",
		  action: function(ev, newMetaData) {
		    data.editor.storage('save', newMetaData, "only-meta-data");
		    return false;
		  }
		}));
	}

	tab.append(formel);
      });
    },


		 /*******************************
		 *	     COMMIT LOG		*
		 *******************************/

    /**
     * Fill the commit log tab
     */
    showHistory: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName);
	var tab  = elem.find(".gitty-history");
	var meta = data.commits[data.commit];
	var playButton;

	if ( data.history )
	  return;

	tab.html("");
	tab.append($.el.table(
	  { class:"table table-striped table-condensed gitty-history",
	    'data-click-to-select':true,
	    'data-single-select':true
	  },
	  $.el.tr($.el.th("Comment"),
		  $.el.th("Date"),
		  $.el.th("User"),
		  $.el.th("Changed")),
	  $.el.tbody()));

	playButton = form.widgets.glyphIconButton(
           "play",
	   { title:"Open the highlighted version in SWISH",
	     class:"btn-primary"
	   });
	tab.append(playButton);
	$(playButton).on("click", function(ev) {
	  var row = elem.find("tr.success");
	  if ( row.length == 1 ) {
	    var commit = row.data('commit');

	    if ( data.commits[commit].symbolic == "HEAD" )
	      file = data.commits[commit].name;
	    else
	      file = commit;

	    elem.parents(".swish").swish('playFile', file);
	    $("#ajaxModal").modal('hide');
	  }
	  return false;
	});

	var url  = config.http.locations.web_storage
		 + encodeURI(meta.name);

	$.ajax({ url: url,
		 contentType: "application/json",
		 type: "GET",
		 data: { format: "history",
		         depth: 6,		/* might skip last */
		         to: data.commit
		       },
		 success: function(reply) {
		   elem.gitty('fillHistoryTable', reply);
		   data.history = data.commit;
		 },
		 error: function(jqXHDR) {
		   modal.ajaxError(jqXHR);
		 }
	       });
      });
    },

    /**
     * Fill the history table
     */
    fillHistoryTable: function(history) {
      var gitty = this;
      var data  = this.data(pluginName);
      var table = this.find(".table.gitty-history tbody");

      for(var i=0; i<history.length; i++) {
	var h = history[i];

	if ( !data.commits[h.commit] )
	  data.commits[h.commit] = h;
      }

      function changedAttributes(m1) {
	var m2, diff;
	var elem = $.el.span();

	if ( m1.previous ) {
	  if ( (m2 = data.commits[m1.previous]) &&
	       (diff = diffMeta(m1, m2)) ) {
	    var change = 0;

	    for( var d in diff ) {
	      if ( diff.hasOwnProperty(d) ) {
		var ch = (d == "name" ? "forked "+m2.name : d);
		$(elem).append((change++ == 0 ? undefined : ", "),
			       $.el.span({class:"change-type"}, ch));
	      }
	    }
	  }
	} else {
	  $(elem).append("initial");
	}

	return elem;
      }

      for(var i=0; i<history.length; i++) {
	var h = history[i];
	var tr;

	if ( i == history.length-1 &&
	     h.previous && !data.commit[h.previous] )
	  break;

	var attrs = {'data-commit':h.commit};
	if ( data.commit == h.commit )
	  attrs.class = "success";

	tr = $.el.tr(attrs,
		     $.el.td({class:"commit-message"},
			     h.commit_message||"No comment"),
		     $.el.td({class:"date"},
			     new Date(h.time*1000).toLocaleString()),
		     $.el.td({class:"author"},
			     h.author||"No author"),
		     $.el.td({class:"changes"},
			     changedAttributes(h)));
	table.append(tr);
      }

      table.on("click", "tr", function(ev) {
	var tr = $(ev.target).parents("tr");
	var commit = tr.data('commit');

	gitty.gitty('setCommit', commit);
      });
    },

    /**
     * Select a row in the table and set the title.
     * @param {String} version is the SHA1 of the new version
     */

    setCommit: function(commit) {
      var data = this.data(pluginName);	/* private data */
      var h2   = this.parent(".modal-content").find("h2");

      h2.html("");
      h2.append(this.gitty('title', data.commits[commit]));
      this.find("tr.success").removeClass("success");
      this.find("tr[data-commit="+commit+"]").addClass("success");
      data.commit = commit;

      return this;
    },

		 /*******************************
		 *	       DIFFS		*
		 *******************************/

    /**
     * Show diff of a given file
     * @param {Object} options
     * @param {String} options.file is the file for which to show diffs
     * @param {String} [options.base] is the base SHA1 (defaults to
     * HEAD^)
     */

    showDiff: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName);

	if ( data.diff == data.commit )
	  return;

	elem.find(".gitty-diff").html("");
	var url  = config.http.locations.web_storage
		 + encodeURI(data.commit);

	$.ajax({ url: url,
		 contentType: "application/json",
		 type: "GET",
		 data: { format: "diff"
		 },
		 success: function(reply) {
		   elem.gitty('fillDiff', reply);
		   data.diff = data.commit;
		 },
		 error: function(jqXHR) {
		   modal.ajaxError(jqXHR);
		 }
	       });
      });
    },

    fillDiff: function(diff) {
      if ( diff.tags ) this.gitty('diffTags', diff.tags);
      if ( diff.data ) this.gitty('udiffData', diff.data);
    },

    diffTags: function(diff) {
      var tab  = this.find(".gitty-diff");
      var div = $($.el.div({class:"diff-tags"},
			    $.el.label("Tags")));
      var span = $($.el.span({class:"diff-tags"}));

      div.append(span);

      function addTag(tag, className) {
	span.append($.el.span({class: "diff-tag "+className}, tag));
      }

      if ( diff.deleted.length ) {
	span.append("Removed: ");
	for(var i=0; i<diff.deleted.length; i++)
	  addTag(diff.deleted[i], "deleted");
      }
      if ( diff.added.length ) {
	span.append(diff.deleted.length ? ", " : "", "Added: ");
	for(var i=0; i<diff.added.length; i++)
	  addTag(diff.added[i], "added");
      }

      tab.append(div);

      return this;
    },

    udiffData: function(diff) {
      var tab  = this.find(".gitty-diff");
      var lines = diff.split("\n");
      var pre = $($.el.pre({class:"udiff"}));

      for(var i=0; i<lines.length; i++) {
	var line = lines[i];
	var classmap = { '@': 'udiff-hdr',
			 ' ': 'udiff-ctx',
			 '+': 'udiff-add',
			 '-': 'udiff-del'
		       };
	pre.append($.el.span({class:classmap[line.charAt(0)]}, line),
		   $.el.br());
      }

      tab.append(pre);
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class gitty
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.gitty = function(method) {
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

  /**
   * Diff meta data
   * @returns {Object|null}, where object holds `author`, `title` and/or
   * `tags`
   */

  function diffMeta(m1, m2) {
    var diff = {};

    function diffAttr(a) {
      if ( (m1[a] || m2[a]) && m1[a] != m2[a] )
	diff[a] = {from: m1[a], to: m2[a]};
    }

    diffAttr("author");
    diffAttr("title");
    diffAttr("data");
    diffAttr("public");
    diffAttr("example");
    diffAttr("name");

    if ( (d=diffTags(m1.tags, m2.tags)) )
      diff.tags = d;

    return $.isEmptyObject(diff) ? null : diff;
  }

  function reduceMeta(meta, old) {
    var r = {};

    for( var k in meta ) {
      if ( meta.hasOwnProperty(k) ) {
	switch(typeof(meta[k])) {
	  case "object":
	    if ( $.isArray(meta[k]) ) {
	      if ( !diffTags(meta[k], old[k]) )
		continue;
	    }
	    break;
	  case "string":
	  case "boolean":
	    if ( old[k] == meta[k] )
	      continue;
	}

	r[k] = meta[k];
      }
    }

    return r;
  }

  /**
   * Diff two tag arrays (arrays of strings)
   * @returns {Object|null}, where object.added is an array with new
   * tags and object.deleted contains deleted tags.
   */
  function diffTags(t1, t2) {
    var d, diff = {};

    t1 = t1||[];
    t2 = t2||[];

    function added(t1, t2) {
      var a = [];

      for(var i=0; i<t2.length; i++) {
	if ( t1.indexOf(t2[i]) < 0 )
	  a.push(t2[i]);
      }

      return a;
    }

    if ( (d=added(t1,t2)).length > 0 ) diff.added = d;
    if ( (d=added(t2,t1)).length > 0 ) diff.deleted = d;

    return $.isEmptyObject(diff) ? null : diff;
  }

  return {
    diffMeta:   diffMeta,
    reduceMeta: reduceMeta,
    diffTags:   diffTags
  };
});
