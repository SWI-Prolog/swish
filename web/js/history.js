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

define(["jquery"],
       function($) {
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
	window.history.pushState({location:reply.url},
				 "",
				 reply.url);
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
	if ( e.state.location ) {
	  window.location =  e.state.location;
	}
      }
    }
  };

  window.onpopstate = history.pop;

  return history;
});
