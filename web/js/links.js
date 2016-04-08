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
    followLink: function(ev) {
      var a = $(ev.target).closest("a");
      var done = false;

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

      function PlDoc(from) {
	if ( from ) {
	  var pred = parsePred(decodeURIComponent(from));

	  if ( pred ) {
	    $(".swish-event-receiver").trigger("pldoc", pred);
	    ev.preventDefault();

	    return true;
	  }
	}
      }

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
	  done = PlDoc(a.attr("href").split("predicate=").pop());
	} else {
	  done = PlDoc(a.attr("href").split("object=").pop());
	}

	if ( !done ) {
	  ev.preventDefault();
	  window.open(a.attr("href"), '_blank');
	}
      }
    }
  }

  return functions;
});
