/**
 * @fileOverview
 * Run an manage Prolog queries and their output
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires laconic
 * @requires editor
 */

define([ "jquery", "config", "preferences",
	 "cm/lib/codemirror", "form", "answer", "laconic"
       ],
       function($, config, preferences, CodeMirror, form) {

		 /*******************************
		 *	  THE COLLECTION	*
		 *******************************/

(function($) {
  var pluginName = 'prologRunners';

  /** @lends $.fn.prologRunners */
  var methods = {
    /**
     * Initialize the container for Prolog queries.
     * @example $(".prolog-runners").prologRunners();
     * @param {Object} [options] currently ignored
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};

	function runnerMenu() {
	  var icon = $.el.span();
	  $(icon).html("&#9776");
	  var menu = dropdownButton(
	    icon,
	    { divClass:"runners-menu",
	      ulClass:"pull-right",
	      client:elem,
	      actions:
	      { "Collapse all": function() {
		  this.find(".prolog-runner").prologRunner('toggleIconic', true);
	        },
		"Expand all": function() {
		  this.find(".prolog-runner").prologRunner('toggleIconic', false);
		},
		"Stop all": function() {
		  this.find(".prolog-runner").prologRunner('stop');
		},
		"Clear": function() { this.prologRunners('clear'); }
	      }
	    });

	  return menu;
	}

	data.stretch = $($.el.div({class:"stretch"}));
	data.inner   = $($.el.div({class:"inner"}));

	elem.append(runnerMenu());
	elem.append(data.stretch);
	elem.append(data.inner);

	elem.on("pane.resize", function() {
	  elem.prologRunners('scrollToBottom', true);
	});

	elem.data(pluginName, data);
      });
    },

    /**
     * Run a Prolog query.  The methods appends a `<div>` and runs the
     * plugin `prologRunner` on the new div.
     * @param {Object} query
     * @param {String} query.query the Prolog query to prove
     * @param {String} [query.source] the Prolog program
     * @param {Boolean} [query.iconifyLast=true] define whether or not
     * to iconify the previous runner.
     * @param {Boolean} [query.tabled=false] if `true`, make a table with
     * the results.
     */
    run: function(query) {
      var data = this.data('prologRunners');

      if ( query.iconifyLast )
	this.prologRunners('iconifyLast');

      var runner = $.el.div({class: "prolog-runner"});

      data.inner.append(runner);
      $(runner).prologRunner(query);
      this.prologRunners('scrollToBottom');

      return this;
    },

    /**
     * Destroy all runners and, if applicable, their associated
     * pengines.
     */
    clear: function() {
      this.find(".prolog-runner").prologRunner('close');
    },

    /**
     * Iconify the last runner if it is not associated to an open
     * query.
     */
    iconifyLast: function() {
      var jrunner = $(this.inner).children().last();

      if ( jrunner.length == 1 )
      { var runner = jrunner.prologRunner();

	if ( !runner.alive() )
	  runner.toggleIconic(true);
      }

      return this;
    },

    /**
     * Keep the content at the bottom of the window, such that the
     * buttons remain in the same position.  The only way to achieve
     * this is by putting something on top of the content as long as
     * the content is lower than the window.
     *
     * @param {Boolean} [onlydown=false] only scroll down if we are
     * not at the bottom.
     */
    // the "- 4" compensates for the prolog-runner top&bottom margin.
    scrollToBottom: function(onlydown) {
      this.each(function() {
	var elem = $(this);
	var data   = elem.data('prologRunners');
	var height = data.inner.height();
	var room   = elem.height() - height - 4 - 2;

	if ( room > 0 || onlydown !== true ) {
	  data.stretch.height(room > 0 ? room : 0);
	  elem.scrollTop(height);
	}
      });

      return this;
    }
  }; // methods

  /**
   * Manage a subwindow (`<div>`) that acts as a collection of runner
   * items.  Each runner represents a Prolog query, either active or
   * terminated.  The collection keeps the runners properly stacked and
   * provides a menu to control the collection, such as _clear_,
   * _iconify all_, etc.
   *
   * @class prologRunners
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */
  $.fn.prologRunners = function(method) {
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


		 /*******************************
		 *	INDIVIDUAL RUNNER	*
		 *******************************/

(function($) {
  var pluginName = 'prologRunner';

  // keyBindings rely on the jQuery normalized `which` field
  var keyBindings = { 59:      'next',		/* ; (FF) */
		      186:     'next',		/* ; (Chromium) */
		      32:      'next',		/* space */
		      190:     'stop',		/* . */
		      13:      'stop',		/* Enter */
		      65:      'stopOrAbort',	/* a */
		      27:      'stopOrAbort',	/* Esc */
		      46:      'close',		/* Del */
		      112:     'help'		/* F1 */
                    };

  /** @lends $.fn.prologRunner */
  var methods = {
    /**
     * Initialize a runner for a Prolog query
     * @param {Object} query
     * @param {String} query.query the Prolog query to prove
     * @param {String} [query.source] the Prolog program
     * @param {Boolean} [query.tabled=false]  If `true`, represent the
     * results as a table.
     */
    _init: function(query) {
      return this.each(function() {
	var elem = $(this);
	var data = {};

	function closeButton() {
	  var btn = $.el.button({title:"Close query"});
	  $(btn).html('&times');

	  $(btn).on("click", function() { elem.prologRunner('close'); });
	  return btn;
	}

	function iconizeButton() {
	  var btn = $.el.button({title:"Iconify query"}, "_");
	  $(btn).on("click", function() { elem.prologRunner('toggleIconic'); });
	  return btn;
	}

	function csvButton() {
	  var btn = $.el.button({title:"Download CSV"}, "\u21ca");
	  $(btn).on("click", function() { elem.prologRunner('downloadCSV'); });
	  return btn;
	}

	function stateButton() {
	  var icon = $.el.span({class:"runner-state show-state idle"});

	  return dropdownButton(icon);
	}

	function controllerDiv() {
	  function next()     { elem.prologRunner('next',    1); }
	  function next10()   { elem.prologRunner('next',   10); }
	  function next100()  { elem.prologRunner('next',  100); }
	  function next1000() { elem.prologRunner('next', 1000); }
	  function stop()     { data.prolog.stop(); }
	  function abort()    { data.prolog.abort(); }

	  function button(action, label) {
	    var btn = $.el.button(label);
	    $(btn).on("click", action);
	    return btn;
	  }

	  function input() {
	    var inp = $.el.input({class:"prolog-input"});
	    var btn = $.el.button("Send");

	    $(inp).keypress(function(ev) {
			      var s;
			      if ( ev.which == 13 &&
				   (s=termNoFullStop($(inp).val())) != "" ) {
				$(inp).val("");
				ev.preventDefault();
				elem.prologRunner('respond', s);
				return false;		/* prevent bubbling */
			      } else if ( ev.key != "Esc" ) {
				ev.stopPropagation();   /* prevent bubbling */
			      }
			    });
	    $(btn).on("click", function() {
				 var s;
				 if ( (s=termNoFullStop($(inp).val())) != "" ) {
				   elem.prologRunner('respond', s);
				 }
			       });

	    return {input:inp, button:btn};
	  }

	  var inp = input();
	  var div = $.el.div({class:"controller show-state"},
			     $.el.div({class:"running"},
				      button(abort, "Abort")),
			     $.el.div({class:"wait-next"},
				      button(next, "Next"),
				      button(next10, "10"),
				      button(next100, "100"),
				      button(next1000, "1,000"), " ",
				      button(stop, "Stop")),
			     $.el.div({class:"wait-input"},
				      button(abort, "Abort"), inp.button,
				      $.el.span(inp.input)));

	  return div;
	}

	elem.addClass("prolog-runner");
	var qspan = $.el.span({class:"query cm-s-prolog"});
	CodeMirror.runMode(query.query, "prolog", qspan);
	elem.append($.el.div(
	  {class:"runner-title ui-widget-header"},
	  closeButton(),
	  iconizeButton(),
          csvButton(),
	  stateButton(),
          qspan));
	elem.append($.el.div(
	  {class:"runner-results"}));
	elem.append(controllerDiv());

	elem.data('prologRunner', data);

	elem.prologRunner('populateActionMenu');
	elem.keydown(function(ev) {
	  if ( elem.prologRunner('getState') != "wait-input" &&
	       !ev.ctrlKey && !ev.altKey ) {
	    if ( keyBindings[ev.which] ) {
	      ev.preventDefault();
	      elem.prologRunner(keyBindings[ev.which]);
	    }
	  }
	});

	data.savedFocus = document.activeElement;
	elem.attr('tabindex', -1);
	elem.focus();

	data.query   = query;
	data.answers = 0;

	/* Load pengines.js incrementally because we wish to ask the
	   one from the pengine server rather than a packaged one.
	*/

	require([config.http.locations.pengines+"/pengines.js"],
		function() {

	  data.prolog = new Pengine({
	    server: config.http.locations.pengines,
	    runner: elem,
	    application: "swish",
	    src: query.source,
	    destroy: false,
	    format: 'json-html',
	    oncreate: handleCreate,
	    onsuccess: handleSuccess,
	    onfailure: handleFailure,
	    onstop: handleStop,
	    onprompt: handlePrompt,
	    onoutput: handleOutput,
	    onerror: handleError,
	    onabort: handleAbort});
	  data.prolog.state = "idle";
	});

	return this;
      });
    }, //_init()

    /**
     * Add a _positive_ answer to the runner.  The answer is embedded in
     * a `<div class="answer">` with an additional class `"even"` or
     * `"odd"` to simplify styling. Note that using CSS odd/even
     * selectors is not possible because there are other elements such
     * as errors.
     * @param {Answer} answer pengine response in `json-html`
     */
    renderAnswer: function(answer) {
      var data = this.data('prologRunner');
      var even = (++data.answers % 2 == 0);

      if ( data.query.tabled ) {
	if ( data.answers == 1 ) {
	  if ( answer.projection && answer.projection.length > 0 ) {
	    var table = answerTable(answer.projection);
	    addAnswer(this, table);
	    data.table = table;
	    data.projection = answer.projection;
	    answer.nth = data.answers;
	    $(data.table).prologAnswer(answer);
	    return this;
	  }
        } else
	{ answer.projection = data.projection;
	  answer.nth = data.answers;
	  $(data.table).prologAnswer(answer);
	  return this;
	}
      }

      var div = $.el.div({class:"answer "+(even ? "even" : "odd")},
			 $.el.span({class:"answer-no"}, data.answers));

      addAnswer(this, div);
      $(div).prologAnswer(answer);
    },

    /**
     * Add pengine output as `<span class="output">`
     * @param {String} data HTML that is inserted in the span.
     */
    outputHTML: function(data) {
      var span = $.el.span({class:"output"});
      $(span).html(data);
      addAnswer(this, span);
    },

    /**
     * Add an error message to the output.  The error is
     * wrapped in a `<pre class="error">` element.
     * @param {String} msg the plain-text error message
     */
    error: function(msg) {
      addAnswer(this, $.el.pre({class:"prolog-message msg-error"}, msg));
    },

    /**
     * Handle trace events
     */
    trace: function(data) {
      var goal = $.el.span({class:"goal"});
      $(goal).html(data.data.goal);

      function capitalizeFirstLetter(string) {
	return string.charAt(0).toUpperCase() + string.slice(1);
      }

      function button(label, action) {
	var btn = $.el.button({class:action,
			       title:label
			      },
			      $.el.span(label));
	$(btn).on("click", function(ev) {
	  data.pengine.respond(action);
	  $(ev.target).parent().remove();
	});
	return btn;
      }

      addAnswer(this,
		$.el.div({class:"prolog-trace"},
			 $.el.span({ class:"depth",
			             style:"width:"+(data.data.depth*5-1)+"px"
				   }, "\u00A0"), /* &nbsp; */
			 $.el.span({ class:"port "+data.data.port
			           },
				   capitalizeFirstLetter(data.data.port),
				   ":"),
			 goal));
      addAnswer(this,
		$.el.div({class:"trace-buttons"},
			 button("Continue",  "nodebug"),
			 button("Step into", "continue"),
			 button("Step over", "skip"),
			 button("Step out",  "up"),
			 button("Retry",     "retry"),
			 button("Abort",     "abort")));
      this.prologRunner('setState', "wait-debug");
    },

    /**
     * set the placeholder of the input field.  This is normally
     * done from the pengine's onprompt handler
     * @param {String} p the new placeholder
     */
    setPrompt: function(p) {
      this.find(".controller input").attr("placeholder", p);
    },

    /**
     * send a response (to pengine onprompt handler) to the
     * pengine and add the response to the dialogue as
     * `div class="response">`
     * @param {String} s plain-text response
     */
    respond: function(s) {
      var data = this.data('prologRunner');
      addAnswer(this, $.el.div({class:"response"}, s));
      data.prolog.respond(s);
    },

    /**
     * Stop the associated Prolog engines.
     */
    stop: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data('prologRunner');
	data.prolog.stop();
      });
    },

    /**
     * Stop the pengine if it is waiting for a next solution,
     * abort it if it is running or waitin for input and ignore
     * otherwise.
     */
    stopOrAbort: function() {
      return this.each(function() {
	var elem  = $(this);
	var data  = elem.data('prologRunner');
	var state = elem.prologRunner('getState');

	switch(state)
	{ case "running":
	  case "wait-input":
	    data.prolog.abort();
	    break;
	  case "wait-next":
	    data.prolog.stop();
	}
      });
    },

    /**
     * Ask the associated Prolog engines for the next answer.
     * @param {Integer} chunk maximum number of answers to return in the
     * next chunk.
     */
    next: function(chunk) {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data('prologRunner');
	data.prolog.next(chunk);
	elem.prologRunner('setState', "running");
      });
    },

    /**
     * Abort the associated Prolog engines.
     */
    abort: function() {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data('prologRunner');
	data.prolog.abort();
      });
    },

    /**
     * If the associated pengine is alive, send it a `destroy`.  Next,
     * remove the runner from its container.
     */
    close: function() {
      if ( this.length ) {
	var runners = RS(this);

	this.each(function() {
	  var elem = $(this);
	  var data = elem.data('prologRunner');

	  if ( elem.prologRunner('alive') )
	    data.prolog.destroy();
	});
	this.remove();

	runners.prologRunners('scrollToBottom', true);
      }
      return this;
    },

    /**
     * Provide help on running a query
     */
     help: function() {
       $(".swish-event-receiver").trigger("help", {file:"runner.html"});
     },

    /**
     * Toggle or set the iconic state of the runner.
     * @param {Boolean} [on] if `true`, make iconify, `false` expanded
     * and toggle if unspecified
     */
    toggleIconic: function(on) {
      if ( on == undefined ) {
	this.toggleClass("iconic");
      } else if ( on ) {
	this.addClass("iconic");
      } else {
	this.removeClass("iconic");
      }

      RS(this).prologRunners('scrollToBottom', true);

      return this;
    },

    /**
     * Populate the menu associated with the pengine icon.
     * @param {Object} [actions] associates labels with functions.
     */
    populateActionMenu: function(actions) {
      var menu = this.find(".runner-title .btn-group.dropdown");

      actions = $.extend({ "Re-run": function() { console.log("Re-Run ", this); }
			 }, actions);

      populateMenu(menu, this, actions);

      return this;
    },

    /**
     * Download query results as CSV.
     * @param {Object} [options]
     * @param {String} [options.projection] holds the Prolog projection
     * variables, separated by commas, e.g., `"X,Y"`
     * @param {String} [options.format="prolog"] holds a string that
     * defines the variation of the CSV format, e.g., `"prolog"` or
     * `"rdf"`
     * @param {String|Number} [options.limit] defines the max number of
     * results.
     * @param {Boolean} [options.distinct] requests only distinct
     * results.
     */
    downloadCSV: function(options) {
      var elem = this;
      var data = this.data('prologRunner');
      var vars = [];

      options = options||{};

      if ( options.projection ) {
	var formel;
	var format = options.format||"prolog";
	var query = data.query.query.replace(/\.\s*$/,"");

	function attr(name,value) {
	  return $.el.input({type:"hidden", name:name, value:value});
	}

	if ( options.distinct )
	  query = "distinct(["+options.projection+"],("+query+"))";
	if ( options.limit ) {
	  var limit = parseInt(options.limit.replace(/[ _]/g,""));

	  if ( typeof(limit) == "number" ) {
	    query = "limit("+limit+",("+query+"))";
	  } else {
	    alert("Not an integer: ", options.limit);
	    return false;
	  }
	}

	formel = $.el.form({ method:"POST",
                             action:config.http.locations.pengines+"/create",
			     target:"_blank"
		           },
			   attr("format", "csv"),
			   attr("chunk", "100000000"),
			   attr("application", "swish"),
			   attr("ask", query),
			   attr("src_text", data.query.source),
			   attr("template", format+"("+options.projection+")"));
	$("body").append(formel);
	formel.submit();
	$(formel).remove();
      } else {
	this.find("span.query span.cm-var").each(function() {
	  var name = $(this).text();
	  if ( vars.indexOf(name) < 0 )
	    vars.push(name);
        });


	function infoBody() {
	  var formel = $.el.form(
            {class:"form-horizontal"},
	    form.fields.projection(vars.join(",")),
	    form.fields.csvFormat(config.swish.csv_formats,
				  preferences.getVal("csvFormat")),
	    form.fields.limit("10 000", false),
	    form.fields.buttons(
	      { label: "Download CSV",
		action: function(ev, params) {
		  ev.preventDefault();
		  if ( config.swish.csv_formats.length > 1 )
		    preferences.setVal("csvFormat", params.format);
		  elem.prologRunner('downloadCSV', params);

		  return false;
		}
	      }));
	  this.append(formel);
	}

	form.showDialog({ title: "Download query results as CSV",
			  body:  infoBody
		        });
      }

      return this;
      },

  /**
   * @param {String} state defines the new state of the pengine.
   * Known states are:
   *
   *   - "idle"	      - Pengine is not yet created
   *   - "running"    - Pengine is running
   *   - "wait-next"  - Pengine produced a non-deterministic answer
   *   - "wait-input" - Pengine waits for input
   *   - "wait-debug" - Pengine waits for for debugger reply
   *   - "true"       - Pengine produced the last answer
   *   - "false"      - Pengine failed
   *   - "error"      - Pengine raised an error
   *   - "stopped"    - User selected *stop* after non-det answer
   *   - "aborted"    - User aborted execution
   *
   * The widget is brought to the new  state   by  adding the state as a
   * class to all members of  the   class  `show-state`, which currently
   * implies the pengines icon at the   top-left  and a _controller_ div
   * created by controllerDiv().
   */
   setState: function(state) {
     var data = this.data('prologRunner');

     if ( data.prolog.state != state ) {
       var stateful = this.find(".show-state");

       stateful.removeClass(data.prolog.state).addClass(state);
       data.prolog.state = state;
       if ( !aliveState(state) && data.savedFocus ) {
	 $(data.savedFocus).focus();
	 data.savedFocus = null;
       } else if ( state == "wait-input" ) {
	 this.find("input").focus();
       }
       if ( !aliveState(state) )
	 data.prolog.destroy();
     }
     if ( state == "wait-next" || state == "true" ) {
       var runners = RS(this);
       setTimeout(function() { runners.prologRunners('scrollToBottom') }, 100);
     } else {
       RS(this).prologRunners('scrollToBottom');
     }
     return this;
   },

   /** @returns {String} representing the current state of the
    * query execution.
    * @see {@link setState}
    */
   getState: function() {
     var data = this.data('prologRunner');

     return data.prolog ? data.prolog.state : "idle";
   },

   /**
    * @returns {Boolean} true if the related pengine is alive.  That
    * means it has state `"running"`, `"wait-next"`, `"wait-input"` or
    * `"wait-debug"`
    */
   alive: function() {
     return aliveState(this.prologRunner('getState'));
   }

  }; // methods


		 /*******************************
		 *     PRIVATE FUNCTIONS	*
		 *******************************/

  function RS(from) {			/* find runners from parts */
    return $(from).parents(".prolog-runners");
  }

  function addAnswer(runner, html) {
    var results = runner.find(".runner-results");
    results.append(html);
    return this;
  }

  function aliveState(state) {
    switch( state )
    { case "running":
      case "wait-next":
      case "wait-input":
      case "wait-debug":
	return true;
      default:
	return false;
    }
  }

  function answerTable(projection) {
    var tds = [{class:"projection"}];

    for(i=0; i<projection.length; i++)
      tds.push($.el.th({class:"pl-pvar"}, projection[i]));
    tds.push($.el.th({class:"answer-nth"}, "No"));

    var table = $.el.table({class:"prolog-answers"},
			   $.el.tbody($.el.tr.apply(this, tds)));

    return table;
  }



		 /*******************************
		 *   HANDLE PROLOG CALLBACKS	*
		 *******************************/

  function handleCreate() {
    var elem = this.pengine.options.runner;
    var data = elem.data('prologRunner');

    this.pengine.ask("'$swish wrapper'((" +
		     termNoFullStop(data.query.query) +
		     "))");
    elem.prologRunner('setState', "running");
  }

  function handleSuccess() {
    var elem = this.pengine.options.runner;

    for(var i=0; i<this.data.length; i++) {
      var answer = this.data[i];
      if ( this.projection )
	answer.projection = this.projection;

      elem.prologRunner('renderAnswer', answer);
    }
    if ( this.time > 0.1 )	/* more than 0.1 sec. CPU (TBD: preference) */
      addAnswer(elem, $.el.div(
	{class:"cputime"},
	$.el.span(this.time.toFixed(3),
		  " seconds cpu time")));

    elem.prologRunner('setState', this.more ? "wait-next" : "true");
  }

  function handleFailure() {
    var elem = this.pengine.options.runner;

    addAnswer(elem, $.el.span({class: "prolog-false"}, "false"));
    elem.prologRunner('setState', "false");
  }

  function handleStop() {
    var elem = this.pengine.options.runner;

    elem.prologRunner('setState', "stopped");
  }

  function handlePrompt() {
    var elem   = this.pengine.options.runner;

    if ( this.data && this.data.type == "trace" ) {
      elem.prologRunner('trace', this);
    } else {
      var prompt = this.data ? this.data : "Please enter a Prolog term";

      elem.prologRunner('setPrompt', prompt);
      elem.prologRunner('setState', "wait-input");
    }
  }

  /**
   * handle `pengine_output/1`.  Note that compiler warnings and errors
   * also end up here. If they have a location, this is provided through
   * this.location, which contains `file`, `line` and `ch`.  We must use
   * this to indicate the location of the error in CodeMirror.
   */

  function handleOutput() {
    var elem = this.pengine.options.runner;

    this.data = this.data.replace(new RegExp("'[-0-9a-f]{36}':", 'g'), "")
    if ( this.location ) {
      this.data = this.data.replace(/pengine:\/\/[-0-9a-f]*\//, "");
      $(".swish-event-receiver").trigger("source-error", this);
    }

    elem.prologRunner('outputHTML', this.data);
    RS(elem).prologRunners('scrollToBottom');
  }

  function handleError() {
    var elem = this.pengine.options.runner;
    var msg;

    if ( this.code == "too_many_pengines" ) {
      msg = "Too many open queries.  Please complete some\n"+
	    "queries by using |Next|, |Stop| or by\n"+
	    "closing some queries.";
    } else
    { msg = String(this.data)
                .replace(new RegExp("'"+this.pengine.id+"':", 'g'), "");
    }

    elem.prologRunner('error', msg);
    elem.prologRunner('setState', "error");
  }

  function handleAbort() {
    var elem = this.pengine.options.runner;

    elem.prologRunner('error', "** Execution aborted **");
    elem.prologRunner('setState', "aborted");
  }

  /**
   * @param {Object} answer a positive answer from the Pengine
   * @returns {Boolean} true if the answer has printable part, i.e., no
   * variable bindings nor residual goals.
   */

  function answerHasOutput(answer) {
    return answer.variables.length > 0 || answer.residuals;
  }

  function termNoFullStop(s) {
    return String($.trim(s)).replace(/\.$/, "");
  }

  /**
   * Run a Prolog query by starting a remote pengine.
   *
   * @class prologRunner
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.prologRunner = function(method) {
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


		   /*******************************
		   *	       UTIL		*
		   *******************************/

  function dropdownButton(icon, options) {
    if ( !options ) options = {};
    var cls     = options.divClass;
    var ulClass = options.ulClass;

    var dropdown = $.el.div(
      {class: "btn-group dropdown"+(cls?" "+cls:"")},
      $.el.button(
	{class:"dropdown-toggle",
	 "data-toggle":"dropdown"},
	icon),
      $.el.ul({class:"dropdown-menu"+(ulClass?" "+ulClass:"")}));

    if ( options.actions )
      populateMenu($(dropdown), options.client, options.actions);

    return dropdown;
  }

  function populateMenu(menu, client, actions) {
    var ul = menu.find(".dropdown-menu");

    function runMenu(a) {
      var action = $(a).data('action');

      if ( action )
	action.call(client);

      return false;
    }

    function addMenuItem(label, onclick) {
      var a = $.el.a(label);

       $(a).data('action', onclick);
       ul.append($.el.li(a));
    }

    for(var a in actions) {
      if ( actions.hasOwnProperty(a) ) {
	addMenuItem(a, actions[a]);
      }
    }

    ul.on("click", "a", function() { runMenu(this); } );

    return menu;
  }
});
