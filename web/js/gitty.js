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
     * Initialize gitty store vizualization.
     * @param {Object} options
     * @param {String} options.file is the file name in the gitty store
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName)||{};	/* private data */
	var url  = config.http.locations.web_storage
		 + "/" + encodeURI(options.file);

	if ( data.file == options.file )
	  return;
	data.file = options.file;

	elem.html("");
	elem.append($.el.table({class:"table table-striped table-condensed gitty-history"},
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

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * Fill the history table
     */
    fillHistoryTable: function(history) {
      var table = this.find(".table.gitty-history");

      function versionActions(h) {
	return $.el.span(form.widgets.glyphIconButton("glyphicon-zoom-in",
						      {action:"diff",
						       title:"Show changes"}),
			 form.widgets.glyphIconButton("glyphicon-play",
						      {action:"play",
						       title:"Open in SWISH"}));
      }

      for(var i=0; i<history.length; i++) {
	var h = history[i];

	table.append($.el.tr({"data-commit":h.commit},
			     $.el.td({class:"commit-message"},
				     h.commit_message||"No comment"),
			     $.el.td({class:"date"},
				     new Date(h.time*1000).toLocaleString()),
			     $.el.td({class:"author"},
				     h.author||"No author"),
			     $.el.td(versionActions(h))));
      }

      table.on("click", "button", function(ev) {
	var button = $(ev.target);
	var commit = button.parents("tr").data("commit");
	var action = button.data("action");

	if ( action == "play" ) {
	  window.location = config.http.locations.web_storage + "/" + commit;
	}

	console.log(action, commit);
      });
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
