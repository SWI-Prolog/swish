/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2022, SWI-Prolog Solutions b.v.
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
 * Demo file to  illustrate  adjusting   the  SWISH  interface,  notably
 * modifying   the   navbar   menu.   This    code   is   loaded   using
 * swish_config:web_plugin/1           as           defined           in
 * config_available/plugin_menu.pl. It defines a  JavaScript module that
 * is executed by require.js after the  dependencies are loaded. We want
 * to post-configure the SWISH menu and thus  need to run after SWISH is
 * initialized. This is done using the trigger `post-config`.
 *
 * The navbar is edited using the  jQuery plugin `navbar`, notably using
 * `clearDropdown` which removes a complete  menu   or  a specific entry
 * from a menu and `populateDropdown` which adds  items to a pulldown of
 * the navigation bar. The added object is a function that may have some
 * properties that defines how exactly it is added. Notably:
 *
 *   - `glyph` is the name of a Bootstrap-3 glyph icon
 *   - `after` or `before` indicates the label of another item which
 *     is used to define the new location in the pulldown.
 *
 * Finally, we extend  the  `storage`  plugin.   This  is  normally  not
 * possible. The plugin is represented using a function `$.fn.<plugin>`,
 * but the methods are not exposed. We  have added a methods property to
 * the function of some of the plugins to allow modifying the methods.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, jan@swi-prolog.org
 */

define([ "jquery", "jswish", "navbar", "storage", "config", "modal" ],
       function($, swish, navbar, storage, config, modal) {

	 swish.swish.on("post-config", function() {
	   // define our new menu item
	   const saveToApp = function() {
	     swish.trigger("storage", "saveToApp");
	   };
	   saveToApp.glyph = "cloud-upload";    // Bootstrap 3 glyph name
	   saveToApp.after = null;		// insert as first

	   // Remove an item and add our own
	   $("#navbar").
	     navbar('clearDropdown', "File", "Save").
	     navbar('populateDropdown',
		    "File",
		    { "Save to my app": saveToApp
		    });

	   // Add our new method to the jQuery storage plugin

	   $.fn.storage.methods.saveToApp = function() {
	     var elem = this;
	     var data = this.data("storage");
	     var post = { data: data.getValue(),
			  url: data.url
			};

	     $.ajax({ url: config.http.locations.save_to_app,
		      type: "POST",
		      data: JSON.stringify(post),
		      contentType: "application/json",
		      dataType: "json",
		      success: function(data) {
			if ( data.status == true ) {
			  if ( data.url ) {
			    elem.storage('kill', true);
			    window.location = data.url;
			  }
			} else {
			  elem.prologEditor('highlightError', {
			    data: $(`<span class="error">${data.message}</span>`),
			    location: data.location || {line: 1, ch:0}
			  }, true);
			}
		      },
		      error: function(jqXHDR) {
			modal.ajaxError(jqXHDR);
		      }
		    });
	   }
	 });
});
