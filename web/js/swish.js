/**
 * @fileOverview
 * Put the SWISH application together.  This file provides the following
 *
 *   - Require.JS config declaration
 *   - Application configuration and initialization calls
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

require.config({
  paths:
  { jquery:      "../bower_components/jquery/dist/jquery.min",
    "jquery-ui": "../bower_components/jquery-ui/jquery-ui.min",
    laconic:     "../bower_components/laconic/laconic",
    bootstrap:   "../bower_components/bootstrap/dist/js/bootstrap.min",
    splitter:    "../bower_components/jquery.splitter/js/jquery.splitter-0.14.0",

    "cm/lib/codemirror": "../bower_components/codemirror/lib/codemirror",
					/* CodeMirror standard extensions */
    "cm/addon/edit/matchbrackets": "../bower_components/codemirror/addon/edit/matchbrackets",
    "cm/addon/comment/continuecomment": "../bower_components/codemirror/addon/comment/continuecomment",
    "cm/addon/comment/comment": "../bower_components/codemirror/addon/comment/comment",
    "cm/addon/hint/show-hint": "../bower_components/codemirror/addon/hint/show-hint",
    "cm/addon/hint/anyword-hint": "../bower_components/codemirror/addon/hint/anyword-hint",
    "cm/addon/display/placeholder": "../bower_components/codemirror/addon/display/placeholder",
    "cm/addon/runmode/runmode": "../bower_components/codemirror/addon/runmode/runmode",
					/* Our own Prolog mode */
    "cm/mode/prolog/prolog": "codemirror/mode/prolog/prolog",
    "cm/mode/prolog/prolog_keys": "codemirror/mode/prolog/prolog_keys",
    "cm/mode/prolog/prolog_query": "codemirror/mode/prolog/prolog_query"
  },
  shim:
  { bootstrap:
    { deps:["jquery"]
    },
    splitter:
    { deps:["jquery"]
    }
  }
}); //require.config

require([ "config",
	  "jquery",
	  "jquery-ui",
	  "splitter",
	  "bootstrap",
          "pane",
	  "navbar",
	  "editor",
	  "query",
	  "runner",
	  "modal",
	  "term",
	  "laconic"
	], function(config)
{

$(function() {

		 /*******************************
		 *	      NAVBAR		*
		 *******************************/

var newProgram = "% Your program goes here\n\n\n\n"+
		 "/** <examples>\n\n\n"+
		 "*/";


function menuBroadcast(event, data) {
  $(".swish-event-receiver").trigger(event, data);
}

function populateExamples(navbar, dropdown) {
  function openExample(ex) {
    return function() {
      $.ajax(ex.href,
	     { dataType: "text",
	       success: function(data) {
		 menuBroadcast("source",
			       { type: "example",
				 file: ex.file,
				 data: data
			       });
	       }
	     });
    };
  }

  $.ajax(config.http.locations.swish_examples,
	 { dataType: "json",
	   success: function(data) {
	     for(var i=0; i<data.length; i++) {
	       navbar.navbar('extendDropdown', dropdown,
			     data[i].title,
			     openExample(data[i]));
	     }
	   }
	 });
}

$("#navbar").navbar({
  "File":
  { "New": function() {
      menuBroadcast("source", { type: "new", data: newProgram });
    },
    "Share group": "--",
    "Save": function() {
      menuBroadcast("saveProgram");
    },
    "Collaborate ...": collaborate,
    "Print group": "--",
    "Print ...": function() {
      $(".prolog-editor").prologEditor('print');
    }
  },
  "Edit":
  { "Clear messages": function() {
      menuBroadcast("clearMessages");
    }
  },
  "Examples": populateExamples,
  "Tools":
  {
  },
  "Help":
  { "About ...": function() {
      menuBroadcast("help", "about.html");
    },
    "Help topics": "--",
    "Help ...": function() {
      menuBroadcast("help", "help.html");
    },
    "Caveats ...": function() {
      menuBroadcast("help", "caveats.html");
    }
  }
});

function swishLogo() {
  $(".swish-logo")
    .append($.el.b($.el.span({style:"color:darkblue"}, "SWI"),
		   $.el.span({style:"color:maroon"}, "SH")))
    .css("margin-left", "30px")
    .css("font-size", "24px")
    .addClass("navbar-brand");
};

swishLogo();

/**
 * pick up all Prolog sources, preparing to execute a query. Currently
 * picks up:
 *
 *   - The `.text()` from all elements that match
 *   `".background.prolog.source"`
 *   - The source of the Prolog editor.  We need some notion of a
 *   _current_ Prolog editor.
 */
function prologSource() {
  var list = [];
  var src;

  if ( (src=$(".prolog-editor").prologEditor('getSource')) )
    list.push(src);
  if ( (src=$(".background.prolog.source").text()) )
    list.push(src);

  return list.join("\n\n");
}

/**
 * Extract examples from `$(".examples.prolog").text()`
 * @returns {Array.String|null}
 */

function examples() {
  var text = $(".examples.prolog").text();

  if ( text )
    return $().prologEditor('getExamples', text, false);
  else
    return function() { return $(".prolog-editor").prologEditor('getExamples'); };
}


function loadHashSource() {
  var file  = window.location.hash.slice(1);
  var parts = file.split("&togetherjs=");

  if ( parts.length == 2 ) {
    file = parts[0];
    require([ "https://togetherjs.com/togetherjs-min.js"
	    ],
	    function() { });
  }

  if ( file ) {
    $.ajax({ url: config.http.locations.web_storage + "/" + file,
	     dataType: "text",
	     success: function(data) {
	       menuBroadcast("source",
			     { type: "hash", /* indicate reason */
			       file: file,   /* associate with file */
			       data: data    /* the content */
			     });
	     }
           });
  }
}


/**
 * Open TogetherJS after lazy loading.
 */
function collaborate() {
  var elem = this;
  $(this).attr("data-end-togetherjs-html", "End collaboration");
  require([ "https://togetherjs.com/togetherjs-min.js"
	  ],
	  function() {
	    TogetherJS(elem);
	  });
}


		 /*******************************
		 *	       PANES		*
		 *******************************/

// receive modal messages
$("body").append($.el.div({id:"modal"}));
$("#modal").swishModal();

// generate the tile layout and properly resize it
$(".tile").tile();
$(window).resize(function() { $(".tile").tile('resize'); });

// create the Prolog interaction components
var editor = $(".prolog-editor").prologEditor();
var runner = $(".prolog-runners").prologRunners();
var query  = $(".prolog-query").queryEditor(
  { source:   prologSource,
    examples: examples(),
    runner:   runner,
  });

loadHashSource();

$('body').on("click", "button.close-pane", function() {
  closePane($(this).parent());
});




}); // $()
}); // require()
