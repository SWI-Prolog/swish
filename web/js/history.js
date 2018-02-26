/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015-2016, VU University Amsterdam
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
 *
 * Manage application history. This file supports  two types of history:
 * plugin for the browser history  and  keep   track  of  issues such as
 * recently used files.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define(["jquery", "preferences", "form", "utils"],
       function($, preferences, form, utils) {
  var history = {

		 /*******************************
		 *	 BROWSER HISTORY	*
		 *******************************/

    /**
     * Push a new entry to the browser history.  Since we have tabs,
     * there isn't much reason for a back button.  We merely use the
     * history to switch the location bar to the current document.
     */
    push: function(options) {
      var cpath = window.location.pathname;

      if ( cpath != options.url ) {
	var state = {location: options.url, reason: options.reason};

	window.history.pushState(state, "", options.url);
	document.title = "SWISH -- "
                       + (options.url ? utils.basename(options.url)
			              : "SWI-Prolog for SHaring");
      }
    },

    /**
     * Restore a previous browser history state.  simply ignores.
     * See push() for details.
     */
    pop: function(e) {
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

	if ( e.id ) {
	  var a = $.el.a(form.widgets.typeIcon(e.id.split(".").pop()),
			 e.label||e.id);

	  $(a).data('document', e);
	  ul.append($.el.li(a));
	}
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
