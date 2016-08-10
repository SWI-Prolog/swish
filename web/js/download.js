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
