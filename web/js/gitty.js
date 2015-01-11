/**
 * @fileOverview
 * Dialog components to interact with the gitty store.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "form", "laconic" ],
       function($, config, form) {

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

	data.meta = meta;
	elem.data(pluginName, data);

	elem.gitty('showMetaData');
      });

      return this;
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
	var meta = data.meta;

	if ( data.metaData == meta.commit )
	  return;
	data.metaData = meta.commit;

	formel = $.el.form({class:"form-horizontal"},
		      form.fields.fileName(meta.name, meta.public,
					   true), // disabled
		      form.fields.title(meta.title),
		      form.fields.author(meta.author),
		      form.fields.date(meta.time, "Date", "date"),
		      form.fields.tags(meta.tags));

	if ( meta.symbolic == "HEAD" ) {
	  $(formel).append(
	      form.fields.buttons(
		{ label: "Update meta data",
		  action: function(ev,data) {
		    console.log(data);
		    data.name = options.file;
		    editor.prologEditor('save', data, "only-meta-data");
		    return false;
		  }
		}));
	}

	tab.append(formel);
      });

      return this;
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
	var url  = config.http.locations.web_storage
		 + "/" + encodeURI(data.meta.name);
	var tab  = elem.find(".gitty-history");

	if ( data.history == "shown" )
	  return;

	tab.html("");
	tab.append($.el.table({class:"table table-striped table-condensed gitty-history",
			       'data-click-to-select':true,
			       'data-single-select':true
			      },
			      $.el.tr($.el.th("Changed"),
				      $.el.th("Date"),
				      $.el.th("Author"),
				      $.el.th("Actions"))));

	$.ajax({ url: url,
		 contentType: "application/json",
		 type: "GET",
		 data: { format: "history"
		 },
		 success: function(reply) {
		   elem.gitty('fillHistoryTable', reply);
		 },
		 error: function() {
		   alert("Failed to fetch history");
		 }
	       });
      });
    },

    /**
     * Fill the history table
     */
    fillHistoryTable: function(history) {
      gitty = this;
      var table = this.find(".table.gitty-history");

      function versionActions(h) {
	return $.el.span(form.widgets.glyphIconButton("glyphicon-zoom-in",
						      {action:"setVersion",
						       title:"Examine version"}),
			 form.widgets.glyphIconButton("glyphicon-play",
						      {action:"play",
						       title:"Open in SWISH"}));
      }

      for(var i=0; i<history.length; i++) {
	var h = history[i];
	var tr;

	tr = $.el.tr({'data-commit':h.commit},
		     $.el.td({class:"commit-message"},
			     h.commit_message||"No comment"),
		     $.el.td({class:"date"},
			     new Date(h.time*1000).toLocaleString()),
		     $.el.td({class:"author"},
			     h.author||"No author"),
		     $.el.td(versionActions(h)));
	$(tr).data('meta', h);
        table.append(tr);
      }

      table.on("click", "button", function(ev) {
	var button = $(ev.target);
	var meta   = button.parents("tr").data('meta');
	var action = button.data("action");

	if ( action == "play" ) {
	  window.location = config.http.locations.web_storage + "/" + commit;
	} else if ( action == "setVersion" ) {
	  gitty.gitty('setVersion', meta);
	}
      });
    },

    /**
     * Select a row in the table and set the title.
     * @param {Object} version is the history object that describes the
     * version.
     */

    setVersion: function(version) {
      var data = this.data(pluginName);	/* private data */
      var h2   = this.parent(".modal-content").find("h2");

      h2.html("");
      h2.append(this.gitty('title', version));
      this.find("tr.success").removeClass("success");
      this.find("tr[data-commit="+version.commit+"]").addClass("success");

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
	var url  = config.http.locations.web_storage
		 + "/" + encodeURI(data.meta.commit);

	if ( data.diff == data.meta.commit )
	  return;

	data.diff = data.meta.commit;

	elem.find(".gitty-diff").html("");
	$.ajax({ url: url,
		 contentType: "application/json",
		 type: "GET",
		 data: { format: "diff"
		 },
		 success: function(reply) {
		   elem.gitty('fillDiff', reply);
		 },
		 error: function() {
		   alert("Failed to fetch diff");
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
			    $.el.label("Edited tags")));

      function addTag(tag, className) {
	div.append($.el.span({class: "diff-tag "+className}, tag));
      }

      for(var i=0; i<diff.deleted.length; i++)
	addTag(diff.deleted[i], "deleted");
      for(var i=0; i<diff.added.length; i++)
	addTag(diff.added[i], "added");

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

  // <private functions>

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
});
