/**
 * @fileOverview
 *
 * RequireJS module to get server configuration information. This module
 * fetches "config.json" relative  to  the   main  document.  The Prolog
 * server emits a  JSON  object  that   provides  the  location  of  all
 * explicitly  identified  HTTP  handlers.  These    are   intended  for
 * (typically) AJAX calls:
 *
 * ```
 *   $.ajax({ url: config.http.locations.swish_examples,
 *            ...
 * ```
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery" ],
       function($) {

/* Configuration of various server components.  We provide
   defaults for the case that these files are served from
   a non-Prolog server.
*/

var config = { http:
	       { locations:
		 { pengines: "/pengine",
		   swish_examples: "/swish/list_examples"
		 }
	       }
	     };

$.ajax("config.json",
       { dataType: "json",
	 async: false,
	 success: function(data) {
	   config = data;
	 }
       });

return config;
});
