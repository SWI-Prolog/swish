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
 *
 * Manage hyper links.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define(["jquery", "config", "modal"],
       function($, config, modal) {

  var functions = {
    /** Decode a PlDoc specification and, if valid, open the
     * corresponding documentation.
     * @arg {String} from The PlDoc specification.  Accepted if it
     * is of the form `[.*:].*[/]/?\d+`
     * @return Boolean `true` if the string was recognised
     */
    PlDoc: function(from, ev) {
      function parsePred(s) {
	var pred = {};
	var i;

	if ( (i=s.indexOf(":")) > 0 ) {
	  pred.module = s.substring(0,i);
	  s = s.slice(i+1);
	}
	if ( (i=s.indexOf("/")) > 0 ) {
	  pred.name = s.substring(0,i);
	  if ( s.charAt(i+1) == '/' )	/* name//arity is a non-terminal */
	    pred.arity = parseInt(s.slice(i+2))+2;
	  else
	    pred.arity = parseInt(s.slice(i+1));

	  if ( !isNaN(pred.arity) )
	    return pred;
	}
      }

      if ( from ) {
	var pred = parsePred(decodeURIComponent(from));

	if ( pred ) {
	  $(ev.target).closest("#ajaxModal").modal('hide');
	  $(".swish-event-receiver").trigger("pldoc", pred);
	  ev.preventDefault();

	  return true;
	}
      }

      return false;
    },

    /**
     * Run a link that refers to a cell. Such a link has a
     * `data-query=name` attribute and optionally a number of
     * `data-Var=Value` attributes. Because attributes are
     * case-insensitive, `Var` is matched case-insensitive against
     * variables from the query.
     */
    runQueryLink: function(a, ev) {
      var nb    = a.closest(".notebook");
      var qname = a.data("query");
      var cell  = nb.find('.nb-cell[name="'+qname+'"]');

      if ( cell ) {
	var vars = $().prologEditor('variables', cell.nbCell('text'), true);
	var bindings = "";
	var options  = {};
	var novars   = [];

	function isVar(k) {
	  for(var i=0; i<vars.length; i++) {
	    if ( vars[i].toLowerCase() == k.toLowerCase() )
	      return vars[i];
	  }
	  novars.push(k);
	}

	$.each(a.data(), function(k, v) {
	  var vr;

	  if ( k !== 'query' && (vr=isVar(k)) ) {
	    if ( bindings != "" )
	      bindings += ", ";
	    bindings += vr + " = (" + v + ")";
	  }
	});

	if ( novars.length > 0 ) {
	  modal.feedback({
	    owner:    nb,
	    type:     "warning",
	    duration: 3000,
	    html:     "The variables <b>" + novars.join(", ") + "</b> do not appear in " +
		      "query <b>" + qname + "</b>"
	  });
	}

	if ( bindings != "" )
          options.bindings = bindings;

	cell.nbCell('run', options);
      }
    },

    /**
     * Follow a link from a markdown or HTML cell. This recognises links
     * to internal SWISH objects and handles them using AJAX calls
     * rather then opening a new page.  If the link is not recognised,
     * it is opened on a new tab/page.  Recognised:
     *
     *  - class="store" links open a gitty store element in a tab
     *  - class="file" links opens a file in a tab
     *  - PlDoc links creates a modal dialog holding the documentation
     *  - data-query=<query-name> runs a query.  data-<Var>=<Value>
     *    binds variables.
     *
     * @param {Event} ev is the event to follow form
     */
    followLink: function(ev) {
      var a = $(ev.target).closest("a");
      var done = false;

      function accept() {
	done = true;
	ev.preventDefault();

	$(ev.target).closest("#ajaxModal").modal('hide');
      }

      if ( a.attr("href") ) {
	var swishStore    = config.http.locations.swish + "p/";
	var swishExamples = config.http.locations.swish + "example/";
	var href	  = a.attr("href");
	var modal;

	if ( href.startsWith(swishStore) && !href.match(/#/) ) {
	  accept();
	  file = href.slice(swishStore.length);
	  $(ev.target).closest(".swish").swish('playFile', file);
	} else if ( a.hasClass("store") ) {
	  accept();
	  modal.alert("File does not appear to come from gitty store?");
	} else if ( a.hasClass("file") ||
		    (href.startsWith(swishExamples) && !href.match(/#/)) ) {
	  accept();
	  $(ev.target).closest(".swish")
		      .swish('playURL', {url: href});
	} else if ( a.hasClass("builtin") && href.match(/predicate=/) ) {
	  done = functions.PlDoc(href.split("predicate=").pop(), ev);
	} else if ( href.match(/object=/) ) {
	  done = functions.PlDoc(href.split("object=").pop(), ev);
	} else if ( (modal=$(ev.target).closest("#ajaxModal")).length == 1 &&
		    href.match(/#/) )
	{ var id = href.split("#").pop();
	  var target;

	  if ( (target=modal.find("#"+id)).length == 1 )
	  { done = true;
	    ev.preventDefault();
	    modal.animate({scrollTop: target.position().top}, 2000);
	  }
	}

	if ( !done ) {
	  ev.preventDefault();
	  window.open(href, '_blank');
	}
      } else if ( a.data("query") ) {
	functions.runQueryLink(a, ev);
      }
    }
  }

  return functions;
});
