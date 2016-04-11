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

      if ( a.attr("href") ) {
	if ( a.hasClass("store") ) {
	  done = true;
	  ev.preventDefault();
	  var swishStore = config.http.locations.swish + "p/";
	  var href = a.attr("href");
	  if ( href.startsWith(swishStore) ) {
	    file = href.slice(swishStore.length);
	    $(ev.target).parents(".swish").swish('playFile', file);
	  } else {
	    modal.alert("File does not appear to come from gitty store?");
	  }
	} else if ( a.hasClass("file") ) {
	  done = true;
	  ev.preventDefault();
	  $(ev.target).parents(".swish")
		      .swish('playURL', {url: a.attr("href")});
	} else if ( a.hasClass("builtin") ) {
	  done = functions.PlDoc(a.attr("href").split("predicate=").pop(), ev);
	} else {
	  done = functions.PlDoc(a.attr("href").split("object=").pop(), ev);
	}

	if ( !done ) {
	  ev.preventDefault();
	  window.open(a.attr("href"), '_blank');
	}
      } else if ( a.data("query") ) {
	functions.runQueryLink(a, ev);
      }
    }
  }

  return functions;
});
