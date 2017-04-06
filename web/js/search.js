/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2017, VU University Amsterdam
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

define([ "jquery", "config", "utils", "bloodhound", "typeahead" ],
       function($, config, utils, Bloodhound) {

(function($) {
  var pluginName = 'search';

  /** @lends $.fn.search */
  var methods = {
    /**
     * Turn Bootstrap search input into a typeahead widget
     * @param {Object}  [options]
     * @param {Boolean} [options.search=true] If false, merely use
     * typeahead to fill a value.
     */
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var query;			/* current query */

		 /*******************************
		 *	 FILE COMPLETION	*
		 *******************************/

	var files = new Bloodhound({
			name: "files",
			remote: { url: config.http.locations.swish_typeahead +
				       "?set=file&q=%QUERY",
				  wildcard: '%QUERY'
			},
			datumTokenizer: fileTokenizer,
			queryTokenizer: Bloodhound.tokenizers.whitespace
	               });
	files.initialize();

	function fileTokenizer(f) {
	  return (f.tags||[]).push(f.name);
	}

	function renderFile(f) {
	  function filetype(file) {
	    return file.split('.').pop();
	  }
	  function filebase(file) {
	    return file.split('.').slice(0,-1).join(".");
	  }

	  var str = "<div class=\"tt-match file type-icon "
	          + filetype(f.name)
	          + "\">"
		  + "<span class=\"tt-label\">"
		  + utils.htmlEncode(filebase(f.name));
	          + "</span>";

	  if ( f.tags ) {
	    str += "<span class=\"tt-tags\">";
	    for(var i=0; i<f.tags.length; i++) {
	      var tag = f.tags[i];
	      str += "<span class=\"tt-tag\">"
		   + utils.htmlEncode(tag)
		   + "</span>";
	    }
	    str += "</span>";
	  }

	  if ( f.title )
	    str += "<div class=\"tt-title file\">"
		 + utils.htmlEncode(f.title)
		 + "</div>";
	  str += "</div>";

	  return str;
	}

		 /*******************************
		 *     SEARCH STORE SOURCES	*
		 *******************************/

	var storeContent = new Bloodhound({
			     name: "store_content",
			     limit: 20,
			     cache: false,
			     remote: {
			       url: config.http.locations.swish_typeahead +
				     "?set=store_content&q=%QUERY",
			       replace:bloodHoundURL
			     },
			     datumTokenizer: sourceLineTokenizer,
			     queryTokenizer: Bloodhound.tokenizers.whitespace
	                   });
	storeContent.initialize();

	var currentFile  = null;
	var currentAlias = null;
	function renderStoreSourceLine(hit) {
	  var str = "";

	  if ( hit.file != currentFile || hit.alias != currentAlias ) {
	    var ext = hit.file.split('.').pop();
	    currentFile = hit.file;
	    currentAlias = hit.alias;
	    str = "<div class=\"tt-file-header type-icon "+ext+"\">"
		+ "<span class=\"tt-path-file\">"
		+ utils.htmlEncode(hit.file)
		+ "</span>"
		+ "</div>";
	  }

	  return str+renderSourceMatch(hit);
	}

		 /*******************************
		 *     SEARCH REMOTE SOURCES	*
		 *******************************/

	var sources = new Bloodhound({
			name: "source",
			limit: 15,
			cache: false,
			query_cache_length: 1,
			remote: {
			  url: config.http.locations.swish_typeahead +
				"?set=sources&q=%QUERY",
			  replace: bloodHoundURL
			},
			datumTokenizer: sourceLineTokenizer,
			queryTokenizer: Bloodhound.tokenizers.whitespace
	               });
	sources.initialize();

	function sourceLineTokenizer(hit) {
	  return Bloodhound.tokenizers.whitespace(hit.text);
	}

	function renderSourceLine(hit) {
	  var str = "";

	  if ( hit.file != currentFile || hit.alias != currentAlias ) {
	    currentFile = hit.file;
	    currentAlias = hit.alias;
	    str = "<div class=\"tt-file-header type-icon "+hit.ext+"\">"
	        + "<span class=\"tt-path-alias\">"
	        + utils.htmlEncode(hit.alias)
		+ "</span>(<span class=\"tt-path-file\">"
		+ utils.htmlEncode(hit.file)
		+ ")</span>"
		+ "</div>";
	  }

	  if ( hit.text )
	    str += renderSourceMatch(hit);
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
	       + utils.htmlEncode(p.name)
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
		 + utils.htmlEncode(p.summary)
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
		  + utils.htmlEncode(text)
	          + "</span>"
	          + "</span>"
		  + "</div>";

	  return str;
	}


		 /*******************************
		 *	       USERS		*
		 *******************************/

	var users = new Bloodhound({
			     name: "users",
			     limit: 20,
			     cache: false,
			     remote: {
			       url: config.http.locations.swish_typeahead +
				     "?set=user&q=%QUERY",
			       replace:bloodHoundURL
			     },
			     datumTokenizer: sourceLineTokenizer,
			     queryTokenizer: Bloodhound.tokenizers.whitespace
	                   });
	users.initialize();

	function renderUser(hit) {
	  function avatar(hit) {
	    if ( hit.avatar ) {
	      return '<img class="avatar" src="'+encodeURI(hit.avatar)+'">';
	    } else {
	      return "";
	    }
	  }

	  var str = '<div class="tt-match user">'
		  + avatar(hit)
		  + '<span class="tt-label">'
		  + utils.htmlEncode(hit.name)
		  + '</span>'
		  + '</div>';

	  return str;
	}


		 /*******************************
		 *	      COMBINE		*
		 *******************************/

	var typeaheadProperties = {
	  source:			/* local source */
	  { name: "source",
	    display: 'text',
	    source: sourceMatcher,
	    templates: { suggestion: renderSourceMatch }
	  },
	  sources:			/* remote sources */
	  { name: "sources",
	    display: 'file',
	    source: sources.ttAdapter(),
	    templates: { suggestion: renderSourceLine },
	    limit: 15
	  },
	  files:			/* files in gitty on name and tags */
	  { name: "files",
	    display: 'name',
	    source: files.ttAdapter(),
	    templates: { suggestion: renderFile }
	  },
	  store_content:		/* file content in gitty */
	  { name: "store_content",
	    display: 'file',
	    source: storeContent.ttAdapter(),
	    templates: { suggestion: renderStoreSourceLine }
	  },
	  predicates:			/* built-in and library predicates */
	  { name: "predicates",
	    display: function(p) {
	      return p.name+"/"+p.arity;
	    },
	    source: predicateMatcher,
	    templates: { suggestion: renderPredicate }
	  },
	  users:			/* Users (profiles) */
	  { name: "users",
	    display: "name",
	    source: users.ttAdapter(),
	    templates: { suggestion: renderUser }
	  }
	};

	// Get the actual query string exchanged between
	// typeahead and Bloodhound.
	var of = typeaheadProperties.sources.source;
	typeaheadProperties.sources.source = function(q, cb) {
	  currentFile = null;
	  currentAlias = null;
	  sourceRE = new RegExp(RegExp.escape(q));
	  return of(q, cb);
	}

	/**
	 * Assemble the sources
	 */

	function ttSources(from) {
	  var sources = [];
	  var src = from.replace(/\s+/g, ' ').split(" ");

	  for(var i=0; i<src.length; i++) {
	    sources.push(typeaheadProperties[src[i]]);
	  }

	  return sources;
	}

		 /*******************************
		 *	     TYPEAHEAD		*
		 *******************************/

	elem.typeahead({ minLength: 2,
			 highlight: true
		       },
		       ttSources(elem.data("search-in")))
	  .on('typeahead:selected typeahead:autocompleted',
	      function(ev, datum) {

		if ( options.search == false ) {
		  elem.data("json-value", datum);
		} else {
		  if ( datum.type == "store" ) {
		    if ( datum.query ) {
		      datum.regex = new RegExp(RegExp.escape(datum.query), "g");
		      datum.showAllMatches = true;
		    }
		    $(ev.target).closest(".swish").swish('playFile', datum);
		  } else if ( datum.arity !== undefined ) {
		    $(".swish-event-receiver").trigger("pldoc", datum);
		  } else if ( datum.editor !== undefined &&
			      datum.line !== undefined ) {
		    $(datum.editor).prologEditor('gotoLine', datum.line,
						 { regex: datum.regex,
						   showAllMatches: true
						 });
		  } else if ( datum.alias !== undefined ) {
		    var url = encodeURI("/"+datum.alias+
					"/"+datum.file+
					"."+datum.ext);
		    var play = { url:url, line:datum.line };

		    if ( datum.query ) {
		      play.regex = new RegExp(RegExp.escape(datum.query), "g");
		      play.showAllMatches = true;
		    }

		    $(ev.target).closest(".swish").swish('playURL', play);
		  } else {
		    elem.data("json-value", datum);
		    console.log(elem.data("json-value"));
		  }
		}
	      });

	if ( options.search != false ) {
	  elem.closest("form").submit(function(ev) {
	    var data = elem.data("json-value");
	    var str  = elem.val();

	    if ( !(data && data.datum && data.datum.label == str) )
	      data = str;

	    elem.val("");
	    elem.data("json-value", null);

	    elem.search('search', data);

	    return false;
	  });
	}
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

  function bloodHoundURL(url, query) {
    var url = url.replace('%QUERY',
			  encodeURIComponent(query));
    var match = $("label.active > input[name=smatch]").val();
    if ( match )
      url += "&match="+match;

    return url;
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

RegExp.escape = function(string) {
  return string.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')
};

});
