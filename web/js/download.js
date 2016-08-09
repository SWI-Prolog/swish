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

	elem.addClass("btn btn-primary download");
	elem.append($.el.span({class:"filename"},
			      data.name+"."+data.ext),
		    $.el.span({class:"glyphicon glyphicon-download",
		               title:"Download"}));
	elem.on("click", function(ev) {
	  elem[pluginName]('download');
	});

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * Actually download the data
     */
    download: function() {
      var data = this.data(pluginName);
      var type = data.type;
      var name = data.name || "swish-download";
      var ext  = data.ext || "dat";
      var chs  = data.charset || "charset=UTF-8";

      function aSupportsDownload() {
	return $("<a>")[0].download != undefined;
      }

      if ( !aSupportsDownload() || !type )
	type = "application/octet-stream";

      var href	= "data:"+type+";"+chs+",";
      href += (chs == "base64" ? data.data : encodeURIComponent(data.data));

      var a = $.el.a({ href:href,
		       download:name+"."+ext
		     });
      $("body").append(a);
      a.click();
      $(a).remove();
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
