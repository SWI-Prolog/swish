/**
 * @fileOverview
 * Support the SWISH search box.  This we want to find:
 *
 *   - Predicates (manual)		[TBD]
 *   - Source code (line)		[TBD]
 *   - Saved programs by
 *     - Name				[OK]
 *     - Tag				[OK]
 *     - Description			[TBD]
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "typeahead" ],
       function($, config) {

(function($) {
  var pluginName = 'search';

  /** @lends $.fn.search */
  var methods = {
    /**
     * Turn Bootstrap search input into a typeahead widget
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);

	var files = new Bloodhound({
			name: "files",
			remote: config.http.locations.typeahead +
				"?set=file&q=%QUERY",
			datumTokenizer: fileTokenizer,
			queryTokenizer: Bloodhound.tokenizers.whitespace
	               });
	files.initialize();

	function fileTokenizer(f) {
	  return (f.tags||[]).push(f.name);
	}

	function renderFile(f) {
	  var str = "<div class=\"tt-match file\">"
		  + "<span class=\"tt-label\">"
		  + htmlEncode(f.name);
	          + "</span>";

	  if ( f.tags ) {
	    str += "<span class=\"tt-tags\">";
	    for(var i=0; i<f.tags.length; i++) {
	      var tag = f.tags[i];
	      str += "<span class=\"tt-tag\">"
		   + htmlEncode(tag)
		   + "</span>";
	    }
	    str += "</span>";
	  }

	  if ( f.title )
	    str += "<div class=\"tt-title file\">"
		 + htmlEncode(f.title)
		 + "</div>";
	  str += "</div>";

	  return str;
	}

	var predicates = new Bloodhound({
			name: "predicates",
			remote: config.http.locations.typeahead +
				"?set=predicates&q=%QUERY",
			datumTokenizer: predicateTokenizer,
			queryTokenizer: Bloodhound.tokenizers.whitespace
	               });
	predicates.initialize();

	function predicateTokenizer(p) {
	  return p.name.split("_");
	}

	function renderPredicate(p) {
	  var str = "<div class=\"tt-match predicate";

	  if ( p.type ) str += " " + p.type;

	  str += "\">"
               + "<span class=\"tt-label\">"
	       + htmlEncode(p.name)
	       + "/"
	       + p.arity
	       + "</span>";

	  if ( p.iso ) {
	    str += "<span class=\"tt-tags\">";
	    if ( p.iso )
	      str += "<span class=\"tt-tag\">ISO</span>";
	    str += "</span>";
	  }

	  if ( p.summary )
	    str += "<div class=\"tt-title file\">"
		 + htmlEncode(p.summary)
		 + "</div>";
	  str += "</div>";


	  str += "</div>";

	  console.log(str);

	  return str;
	}

	elem.typeahead({ minLength: 1,
			 highlight: true
		       },
		       [ { name: "files",
			   source: files.ttAdapter(),
			   templates: { suggestion: renderFile }
		         },
			 { name: "predicates",
			   source: predicates.ttAdapter(),
			   templates: { suggestion: renderPredicate }
		         }
		       ])
	  .on('typeahead:selected typeahead:autocompleted',
	      function(ev, datum, set) {
		if ( datum.url ) {
		  window.location = datum.url;
		} else if ( datum.arity !== undefined ) {
		  $(".swish-event-receiver").trigger("pldoc", datum);
		} else {
		  elem.data("target", {datum:datum, set:set});
		  console.log(elem.data("target"));
		}
	      });

	elem.parents("form").submit(function(ev) {
	  var data = elem.data("target");
	  var str  = elem.val();

	  if ( !(data && data.datum && data.datum.label == str) )
	    data = str;

	  elem.val("");
	  elem.data("target", null);

	  elem.search('q', data);

	  ev.preventDefault();
	  return false;
	});
      });
    },

    /**
     * Search for the a given query.
     *
     * @param {String|Object} q specifies the search target. If it is a
     * string, no autocompletion was performed.  If it is an object, it
     * is the object returned by Bloodhound
     */
    search: function(q) {
      console.log(q);
    }
  }; // methods

  function htmlEncode(html) {
    if ( !html ) return "";
    return document.createElement('a')
                   .appendChild(document.createTextNode(html))
		   .parentNode
		   .innerHTML;
  };

  /**
   * <Class description>
   *
   * @class search
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.search = function(method) {
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
