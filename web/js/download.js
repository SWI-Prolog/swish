/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam
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
 * Provide download from the application
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "laconic" ],
       function() {

(function($) {
  var pluginName = 'downloader';

  /** @lends $.fn.downloader */
  var methods = {
    /**
     * @param {Object} options
     * @param {String} options.data Content to be sent.
     * @param {String} [options.filename] (base) name of the downloaded
     * file.
     * @param {String} [options.type] MIME type (default
     * `application/octet-stream`)
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = $.extend({
	  name:"swish-download",
	  ext:"dat"
	}, options);

	var type = data.content_type;
	var name = data.filename || "swish-download.dat";
	var chs  = data.charset  || "charset=UTF-8";

	function aSupportsDownload() {
	  return $("<a>")[0].download != undefined;
	}

	if ( !aSupportsDownload() || !type )
	  type = "application/octet-stream";

	var href      = "data:"+type+";"+chs+",";
        href += (chs == "base64" ? data.data : encodeURIComponent(data.data));

	elem.attr("download", name);
	elem.attr("href", href);
	elem.attr("title", "Download (use menu for save link as)");
	elem.text(name);

	elem.addClass("btn btn-primary download");
	elem.append($.el.span({class:"glyphicon glyphicon-download"}));
      });
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class downloader
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.downloader = function(method) {
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
