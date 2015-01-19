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
var KEY = "SWISHCONFIG";

/* Configuration of various server components.  We provide
   defaults for the case that these files are served from
   a non-Prolog server.
*/

var config;

function getCachedConfig() {
  if ( typeof(Storage) !== "undefined" && window.swish.config_hash ) {
    var str;

    if ( (str = localStorage.getItem(KEY)) ) {
      value = JSON.parse(str);
      if ( value.hash == window.swish.config_hash )
	return value.config;
    }
  }
}

function setCachedConfig(config) {
  if ( typeof(Storage) !== "undefined" && window.swish.config_hash ) {
    localStorage.setItem(KEY, JSON.stringify(
      { hash: window.swish.config_hash,
        config: config
      }));
  }
}

if ( !config ) {
  if ( !(config = getCachedConfig()) ) {
    $.ajax("swish_config.json",
	   { dataType: "json",
	     async: false,
	     success: function(data) {
	       config = data;
	       setCachedConfig(config);
	     },
	     error: function() {
	       alert("Failed to fetch configuration from server");
	     }
	   });
  }
}

return config;
});



