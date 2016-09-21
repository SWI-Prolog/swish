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



