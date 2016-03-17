/**
 * @fileOverview
 *
 * Manage application history. This file supports  two types of history:
 * plugin for the browser history  and  keep   track  of  issues such as
 * recently used files.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define(["jquery", "preferences"],
       function($, preferences) {
  var history = {

		 /*******************************
		 *	 BROWSER HISTORY	*
		 *******************************/

    /**
     * Push a new entry to the browser history.
     * FIXME: Safe history menu of the query window.
     * FIXME: Deal with unsafed data.
     */
    push: function(reply) {
      var cpath = window.location.pathname;

      if ( cpath != reply.url ) {
	var state = {location:reply.url};

	if ( reply.meta )
	  state.meta = reply.meta;

	window.history.pushState(state, "", reply.url);
	document.title = "SWISH -- "
                       + (reply.file ? reply.file
			             : "SWI-Prolog for SHaring");
      }
    },

    /**
     * Restore a previous browser history state
     */
    pop: function(e) {
      if ( e.state ) {
	if ( e.state.meta && e.state.meta.name ) {
	  $(".swish").swish('playFile', e.state.meta.name);
	} else if ( e.state.location ) {
	  window.location =  e.state.location;
	}
      }
    },

		 /*******************************
		 *	  RECENT DOCUMENTS	*
		 *******************************/

    recentMaxLength: 10,

    /**
     * Add/refresh document to list of recent documents.
     * @param {Object} doc
     * @param {String} doc.id is the document _identifier_
     * @param {String} [doc.label] is the document label for
     * the _Open recent_ menu.  Default is the `id`.
     * @param {String} doc.type is the type of document.
     * A document of a specific type is opened by calling
     * `history.openRecent.type.call(event, doc)`
     */

    addRecent: function(doc) {
      var recent = preferences.getVal("recentDocuments")||[];

      function equalDocument(d1, d2) {
	return d1.type == d2.type && d1.id == d2.id;
      }

      for(var i=0; i<recent.length; i++) {
	if ( equalDocument(doc, recent[i]) ) {
	  recent.splice(i,1);
	  break;
	}
      }
      while ( recent.length+1 > history.recentMaxLength )
	recent.pop();
      recent.splice(0,0,doc);

      preferences.setVal("recentDocuments", recent);
    },

    openRecent: function(ev, doc) {
      return history.openRecent[doc.st_type](ev, doc);
    },

    /**
     * Fill a (navbar) <ul> with <li><a> elements, where
     * each <a> carries the related entry as `data('document')`
     */
    updateRecentUL: function() {
      var ul = $(this);
      var recent = preferences.getVal("recentDocuments")||[];

      ul.html("");
      for(var i=0; i<recent.length; i++) {
	var e = recent[i];
	var a = $.el.a(e.label||e.id);

	$(a).data('document', e);
	ul.append($.el.li(a));
      }
    }
  };

  /**
   * Open recent "gitty" document
   */
  history.openRecent.gitty = function(ev, doc) {
    $(ev.target).parents(".swish").swish('playFile', doc.id);
  };

  window.onpopstate = history.pop;

  return history;
});
