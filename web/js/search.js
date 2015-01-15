/**
 * @fileOverview
 * Support the SWISH search box.  This we want to find:
 *
 *   - Predicates based on templates we also use for template completion
 *   - Source code (line)		[TBD]
 *   - Saved programs by
 *     - Name
 *     - Tag
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
	var query;			/* current query */

		 /*******************************
		 *	 FILE COMPLETION	*
		 *******************************/

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

		 /*******************************
		 *    PREDICATE COMPLETION	*
		 *******************************/

	function predicateMatcher(q, cb) {
	  var templates = config.swish.templates;
	  var matches = [];
	  var ql = q.split(" ");
	  var pl = [];

	  for(var i=0; i<ql.length; i++)
	    pl.push({prefix:ql[i], regex:new RegExp("_"+ql[i])});

	  for(var i=0; i<templates.length; i++) {
	    var templ = templates[i];

	    if ( templ.arity !== undefined ) {
	      for(var j=0, match=true; j<pl.length && match; j++) {
		if ( !(templ.name.startsWith(pl[j].prefix) ||
		       templ.name.match(pl[j].regex)) )
		  match=false;
	      }
	      if ( match )
	        matches.push(templ);
	    }
	  }

	  cb(matches);
	}


	function renderPredicate(p) {
	  var str = "<div class=\"tt-match predicate";

	  if ( p.type ) str += " " + p.type;
	  if ( p.mode ) str += "\" title=\""
                             + p.mode;

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

	  return str;
	}

		 /*******************************
		 *	   SEARCH SOURCE	*
		 *******************************/

	var sourceRE;

	function sourceMatcher(q, cb) {
	  query = q;
	  if ( q.length < 2 ) return [];

	  var matches = [];
	  var re = new RegExp("\\b"+q, "g");
	  sourceRE = re;

	  $(".prolog-editor").each(function() {
	    var editor = this;
	    var m = $(editor).prologEditor('search', re, {max: 7});

	    for(var i=0; i<m.length; i++) {
	      m[i].editor = editor;
	      m[i].regex  = sourceRE;
	      matches.push(m[i]);
	    }
	  });

	  cb(matches);
	}


	function renderSourceMatch(hit) {
	  var text = hit.text;
	  var i;

	  if ( (i=text.search(sourceRE)) > 20 )
	    text = "..."+text.slice(i-17);
	  if ( text.length > 80 )
	    text = text.substring(0,80);

	  var str = "<div class=\"tt-match source\">"
	          + "<span class=\"tt-line\">"
		  + "<span class=\"tt-lineno\">"
		  + hit.line
		  + "</span>"
		  + "<span class=\"tt-text\">"
		  + htmlEncode(text)
	          + "</span>"
	          + "</span>";

	  return str;
	}


		 /*******************************
		 *	     TYPEAHEAD		*
		 *******************************/

	elem.typeahead({ minLength: 1,
			 highlight: true
		       },
		       [ { name: "source",
			   source: sourceMatcher,
			   templates: { suggestion: renderSourceMatch }
		         },
			 { name: "files",
			   source: files.ttAdapter(),
			   templates: { suggestion: renderFile }
		         },
			 { name: "predicates",
			   source: predicateMatcher,
			   templates: { suggestion: renderPredicate }
		         }
		       ])
	  .on('typeahead:selected typeahead:autocompleted',
	      function(ev, datum, set) {
		if ( datum.type == "store" ) {
		  $(ev.target).parents(".swish").swish('playFile', datum.file);
		} else if ( datum.arity !== undefined ) {
		  $(".swish-event-receiver").trigger("pldoc", datum);
		} else if ( datum.editor !== undefined &&
			    datum.line !== undefined ) {
		  $(datum.editor).prologEditor('gotoLine', datum.line,
					       { regex: datum.regex,
						 showAllMatches: true
					       });
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

	  elem.search('search', data);

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
      alert("Full search not yet implemented\n"+
	    "Please select from auto completion list");
    }
  }; // methods

  function htmlEncode(html) {
    if ( !html ) return "";
    return document.createElement('a')
                   .appendChild(document.createTextNode(html))
		   .parentNode
		   .innerHTML;
  };

  if (typeof String.prototype.startsWith != 'function') {
    String.prototype.startsWith = function(str) {
      return this.lastIndexOf(str, 0) === 0;
    };
  }

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
