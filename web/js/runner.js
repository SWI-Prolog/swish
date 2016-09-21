/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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
 * Run an manage Prolog queries and their output
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 * @requires laconic
 * @requires editor
 */

define([ "jquery", "config", "preferences",
	 "cm/lib/codemirror", "form", "prolog", "links",
	 "answer", "laconic", "sparkline", "download"
       ],
       function($, config, preferences, CodeMirror, form, prolog, links) {

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
	  var icon = $.el.span({class:"glyphicon glyphicon-menu-hamburger"});
	  var menu = form.widgets.dropdownButton(
	    icon,
	    { divClass:"runners-menu btn-transparent",
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
     * @param {prologEditor} [query.editor] the source editor
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
     * @param {Boolean} [query.title=true] If `false`, suppress the
     * title.
     */
    _init: function(query) {
      return this.each(function() {
	var elem = $(this);
	var data = {};

	function titleBarButton(glyph, title, action) {
	  var btn = $.el.button({title:title, class:"rtb-"+action},
				$.el.span({class:"glyphicon glyphicon-"+glyph}));
	  $(btn).on("click", function() { elem.prologRunner(action); });
	  return btn;
	}

	function stateButton() {
	  var icon = $.el.span({class:"runner-state show-state idle"});

	  return form.widgets.dropdownButton(icon);
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
			      if ( ev.which == 13 &&
				   elem.prologRunner('respond', $(inp).val()) ) {
				$(inp).val("");
				ev.preventDefault();
				return false;		/* prevent bubbling */
			      } else if ( ev.key != "Esc" ) {
				ev.stopPropagation();   /* prevent bubbling */
			      }
			    });
	    $(btn).on("click", function() {
				 elem.prologRunner('respond', $(inp).val());
			       });

	    return {input:inp, button:btn};
	  }

	  function statusChart() {
	    var spark = $.el.span({class:"sparklines"}, "");

	    return spark;
	  }

	  var inp = input();
	  var div = $.el.div({class:"controller show-state"},
			     $.el.span({class:"running"},
				       button(abort, "Abort")),
			     $.el.span({class:"wait-next"},
				       button(next, "Next"),
				       button(next10, "10"),
				       button(next100, "100"),
				       button(next1000, "1,000"), " ",
				       button(stop, "Stop")),
			     $.el.span({class:"wait-input"},
				       button(abort, "Abort"), inp.button,
				       $.el.span(inp.input)),
			     statusChart());

	  return div;
	}

	elem.addClass("prolog-runner");
	if ( query.tabled )
	  elem.addClass("tabled");
	if ( query.title != false ) {
	  var qspan = $.el.span({class:"query cm-s-prolog"});
	  CodeMirror.runMode(query.query, "prolog", qspan);
	  elem.append($.el.div(
	    {class:"runner-title ui-widget-header"},
	    titleBarButton("remove-circle", "Close",        'close'),
	    titleBarButton("minus",         "Iconify",      'toggleIconic'),
	    titleBarButton("download",      "Download CSV", 'downloadCSV'),
	    stateButton(),
	    qspan));
	} else {
	  var close = glyphButton("remove-circle", "Close");
	  elem.append(close);
	  $(close).on("click", function() {
	    elem.prologRunner('close');
	  });
	}
	if ( query.chunk )
	  data.chunk = query.chunk;
	elem.append($.el.div({class:"runner-results"}));
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
	elem.on("click", "a", links.followLink);

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
	    onping: handlePing,
	    onerror: handleError,
	    onabort: handleAbort});
	  data.prolog.state = "idle";
	  if ( config.swish.ping && data.prolog.ping != undefined ) {
	    data.prolog.ping(config.swish.ping*1000);
	  }
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
     * @param {String} data HTML that is inserted into the span.
     * @return {DOM} the added node (a span)
     */
    outputHTML: function(data) {
      var span = $.el.span({class:"output"});
      addAnswer(this, span);
      span.innerHTML = data;
      runScripts(span);
      return span;
    },

    /**
     * Handle object output
     */
     downloadButton: function(obj) {
       var button = $.el.a({class:"download"});
       addAnswer(this, button);
       $(button).downloader(obj);
     },

    /**
     * Display a syntax error in the query.
     * {Object} options
     * {String} options.message is the message
     * {Object} options.location contains the `line` and `ch` position
     */
     syntaxError: function(options) {
       var data = this.data(pluginName);

       options.data = "<pre class=\"output msg-error\">" +
		      options.message +
		      "</pre>";
       options.location.file = true;
       $(data.query.query_editor).prologEditor('highlightError', options);
       return this;
     },

    /**
     * Add an error message to the output.  The error is
     * wrapped in a `<pre class="error">` element.
     * @param {String|Object} options If `options` is a string, it is a
     * plain-text error message.  Otherwise it is the Pengine error
     * object.
     * @param {String} options.message is the plain error message
     * @param {String} options.code is the error code
     */
    error: function(options) {
      var msg;

      if ( typeof(options) == 'object' ) {
	if ( options.code == "died" ) {
	  addAnswer(this, $.el.div({
	    class:"RIP",
	    title:"Remote pengine timed out"
	  }));
	  return this;
	} else if ( options.code == "syntax_error" )
	{ var m = options.message.match(/^HTTP:DATA:(\d+):(\d+):\s*(.*)/);

	  if ( m && m.length == 4 ) {
	    this.prologRunner('syntaxError',
			      { location:
				{ line: parseInt(m[1])-1,
				  ch:	parseInt(m[2])
				},
				message: m[3]
			      });
	    msg = "Cannot run query due to a syntax error (check query window)";
	  }
	}
	if ( !msg )
	  msg = options.message;
      } else
	msg = options;

      addAnswer(this, $.el.pre({class:"prolog-message msg-error"}, msg));
      return this;
    },

    /**
     * Handle trace events
     */
    trace: function(data) {
      var elem = this;
      var goal = $.el.span({class:"goal"});
      var prompt = data.data;
      $(goal).html(prompt.goal);

      function capitalizeFirstLetter(string) {
	return string.charAt(0).toUpperCase() + string.slice(1);
      }

      function button(label, action, context) {
	var btn = $.el.button({class:action,
			       title:label
			      },
			      $.el.span(label));
	$(btn).on("click", function(ev) {
	  if ( context !== undefined ) {
	    action += "("+Pengine.stringify(context(ev))+")";
	  }
	  data.pengine.respond(action);
	  $(ev.target).parent().remove();
	});
	return btn;
      }

      addAnswer(this,
		$.el.div({class:"prolog-trace"},
			 $.el.span({ class:"depth",
			             style:"width:"+(prompt.depth*5-1)+"px"
				   }, "\u00A0"), /* &nbsp; */
			 $.el.span({ class:"port "+prompt.port
			           },
				   capitalizeFirstLetter(prompt.port),
				   ":"),
			 goal));
      if ( prompt.port == "exception" )
	addAnswer(this,
		  $.el.div({class:"prolog-exception"},
			   prompt.exception.message));
      addAnswer(this,
		$.el.div({class:"trace-buttons"},
			 button("Continue",  "nodebug", function(ev) {
			   return breakpoints($(ev.target)
				    .closest(".prolog-runner"));
			 }),
			 button("Step into", "continue"),
			 button("Step over", "skip"),
			 button("Step out",  "up"),
			 button("Retry",     "retry"),
			 button("Abort",     "abort")));

      this.closest(".swish")
          .find(".tabbed")
          .trigger("trace-location", prompt);

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
     * Support arbitrary jQuery requests from Prolog
     */
    jQuery: function(prompt) {
      var request = prompt.data;
      var receiver;

      if ( typeof(request.selector) == "string" ) {
	receiver = $(request.selector);
      } else if ( typeof(request.selector) == "object" ) {
	switch(request.selector.root) {
	  case "this":	root = this; break;
	  case "swish":	root = this.closest(".swish"); break;
	}
	if ( request.selector.sub == "" ) {
	  receiver = root;
	} else {
	  receiver = root.find(request.selector.sub);
	}
      }

      console.log(receiver);
      var result = receiver[request.method].apply(receiver, request.arguments);
      console.log(result);

      prompt.pengine.respond(Pengine.stringify(result));
    },

    /**
     * send a response (to pengine onprompt handler) to the
     * pengine and add the response to the dialogue as
     * `div class="response">`
     * @param {String} s plain-text response
     */
    respond: function(text) {
      var data = this.data('prologRunner');

      if ( data.wait_for == "term" ) {
	s = termNoFullStop(text);
	if ( s == "" )
	  return null;
      } else {
	s = Pengine.stringify(text+"\n");
      }

      addAnswer(this, $.el.div({class:"response"}, text));
      data.prolog.respond(s);
      return this;
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

	  if ( elem.prologRunner('alive') ) {
	    $(".prolog-editor").trigger('pengine-died', data.prolog.id);
	    data.prolog.destroy();
	  }
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

      form.widgets.populateMenu(menu, this, actions);

      return this;
    },

    /**
     * Download query results as CSV.
     */
    downloadCSV: function(options) {
      var data = this.data('prologRunner');
      var query = data.query.query.replace(/\.\s*$/,"");

      prolog.downloadCSV(query, data.query.source, options);

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

     if ( !data )
       return;

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
       if ( !aliveState(state) ) {
	 $(".prolog-editor").trigger('pengine-died', data.prolog.id);
	 data.prolog.destroy();
       }
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
   },

   /**
    * Handle ping data, updating the sparkline status
    */
   ping: function(stats) {
     var data = this.data('prologRunner');

     if ( data && data.prolog && data.prolog.state == "running" ) {
       var spark = this.find(".sparklines");
       var stacks = ["global", "local", "trail"];
       var colors = ["red", "blue", "green"];
       var names  = ["Global ", "Local ", "Trail "];
       var maxlength = 10;

       if ( !data.stacks )
	 data.stacks = { global:{usage:[]}, local:{usage:[]}, trail:{usage:[]} };

       for(i=0; i<stacks.length; i++) {
	 var s = stacks[i];
	 var limit = stats.stacks[s].limit;
	 var usage = stats.stacks[s].usage;

	 var u = Math.log10((usage/limit)*10000);
	 function toBytes(limit, n) {
	   var bytes = Math.round((Math.pow(10, n)/10000)*limit);

	   function numberWithCommas(x) {
	     x = x.toString();
	     var pattern = /(-?\d+)(\d{3})/;
	     while (pattern.test(x))
	       x = x.replace(pattern, "$1,$2");
	     return x;
	   }

	   return numberWithCommas(bytes);
	 }

	 data.stacks[s].limit = limit;
	 if ( data.stacks[s].usage.length >= maxlength )
	   data.stacks[s].usage = data.stacks[s].usage.slice(1);
	 data.stacks[s].usage.push(u);
	 spark.sparkline(data.stacks[s].usage,
			 { height: spark.parent().height(),
			   composite: i>0,
			   chartRangeMin: 0,
			   chartRangeMax: 4,
			   lineColor: colors[i],
			   tooltipPrefix: names[i],
			   tooltipSuffix: " bytes",
			   tooltipChartTitle: i == 0 ? "Stack usage" : undefined,
			   numberFormatter: function(n) {
			     return toBytes(limit, n);
			   }
			 });
       }
     }
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
    tds.push($.el.th({class:"answer-nth"}, ""));

    var table = $.el.table({class:"prolog-answers"},
			   $.el.tbody($.el.tr.apply(this, tds)));

    return table;
  }

		 /*******************************
		 *	 SCRIPTS IN NODES	*
		 *******************************/

  var node_id = 1;
  function runScripts(elem) {
    var scripts = [];
    elem = $(elem);

    elem.find("script").each(function() {
      var type = this.getAttribute('type')||"text/javascript";
      if ( type == "text/javascript" )
	scripts.push(this.textContent);
    });

    if ( scripts.length > 0 ) {
      var script = "(function(node){" + scripts.join("\n") + "})";
      var node = new Node({
        node: elem[0]
      });

      try {
	eval(script)(node);
      } catch(e) {
	alert(e);
      }
    }
  }

  function Node(options) {
    this.my_node = options.node;
  }

  Node.prototype.node = function() {
    return $(this.my_node);
  }

  /**
   * Provide a unique id for the node.  This can be used as prefix to
   * avoid conflicts for `id` attributes.
   */
  Node.prototype.unique_id = function() {
    if ( !this.uid )
      this.uid = node_id++;
    return this.uid;
  }


		 /*******************************
		 *   HANDLE PROLOG CALLBACKS	*
		 *******************************/

  function breakpoints(runner) {
    var data = runner.data(pluginName);

    return $(runner).parents(".swish").swish('breakpoints', data.prolog.id);
  }

  function registerSources(pengine) {
    var runner = pengine.options.runner;
    var data   = runner.data(pluginName);

    if ( data.query.editor )
      $(data.query.editor).prologEditor('pengine', {add: pengine.id});
  }

  function handleCreate() {
    var elem = this.pengine.options.runner;
    var data = elem.data(pluginName);
    var options = {};
    var bps;
    var resvar = config.swish.residuals_var || "Residuals";

    registerSources(this.pengine);

    if ( (bps = breakpoints(elem)) )
      options.breakpoints = Pengine.stringify(bps);
    if ( data.chunk )
      options.chunk = data.chunk;

    this.pengine.ask("'$swish wrapper'((\n" +
		     termNoFullStop(data.query.query) +
		     "\n), "+resvar+")", options);
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
    var data   = elem.data('prologRunner');
    var prompt = this.data || "Please enter a Prolog term";

    data.wait_for = "term";

    if ( typeof(prompt) == "object" ) {
      if ( prompt.type == "trace" ) {
	return elem.prologRunner('trace', this);
      } else if ( prompt.type == "jQuery" ) {
	return elem.prologRunner('jQuery', this);
      } else if ( prompt.type == "console" ) {
	prompt = prompt.prompt || "console> ";
	data.wait_for = "line";
      } else {
	prompt = JSON.stringify(prompt);
      }
    }

    elem.prologRunner('setPrompt', prompt);
    elem.prologRunner('setState', "wait-input");
  }

  /**
   * Make indicated source locations clickable.
   * @param {String} msg is the HTML error message string
   * @param {DOM} editor is the source editor; the editor for pengine://
   * source locations
   */
  function clickableLocations(msg, editor) {
    var pattern = /pengine:\/\/[-0-9a-f]{36}\/src:(\d+)/;

    return msg.replace(pattern, function(matched) {
      var line = matched.match(pattern)[1];
      return "<a class='goto-error' title='Goto location'>" +
               "<span class='glyphicon glyphicon-hand-right'></span> "+
	       "<b>line <span class='line'>"+line+"</span></b></a>";
    });
  }

  function gotoError(ev) {
    var a        = $(ev.target).closest("a.goto-error");
    var ctx      = $(ev.target).closest(".error-context");
    var econtext = ctx.data("error_context");

    if ( a[0] ) {
      var line = parseInt(a.find("span.line").text());
      var file = a.find("span.file").text();

      ev.preventDefault();

      if ( file ) {
	ctx.closest("body.swish")
	   .swish('playFile', {file:file, line:line});
      } else {
	$(econtext.editor).prologEditor('gotoLine', line);
      }

      return false;
    } else if ( econtext.location.file ) {
      ctx.closest("body.swish")
	 .swish('playFile', econtext.location);
    } else {
      $(econtext.editor).prologEditor('gotoLine', econtext.location.line);
    }
  }

  /**
   * handle `pengine_output/1`.  Note that compiler warnings and errors
   * also end up here. If they have a location, this is provided through
   * this.location, which contains `file`, `line` and `ch`.  We must use
   * this to indicate the location of the error in CodeMirror.
   */

  function handleOutput(msg) {
    var elem = msg.pengine.options.runner;

    if ( typeof(msg.data) == 'string' ) {
      var data = elem.data(pluginName);
      var econtext = {editor: data.query.editor};

      msg.data = msg.data.replace(/'[-0-9a-f]{36}':/g, "")  /* remove module */

      if ( msg.location ) {
	var loc = msg.location;
	var prefix = "swish://";
	var span;

	function clickableError() {
	  var str = loc.file+":"+loc.line+":";
	  if ( loc.ch ) str += loc.ch+":";
	  str += "\\s*";

	  msg.data = clickableLocations(
			 msg.data.replace(new RegExp(str, "g"), ""),
			 econtext.editor);

	  span = elem.prologRunner('outputHTML', msg.data);

	  $(span).addClass("error-context");
	  $(span).append($.el.span({class:"glyphicon glyphicon-hand-right"}));
	  $(span).attr("title", "Error in program.  Click to show in context");
	  $(span).on("click", gotoError);
	  $(span).data("error_context", econtext);
	}

	if ( loc.file.startsWith(prefix) ) {
	  var file = loc.file.slice(prefix.length);
	  econtext.location = {file:file, line:loc.line};
	  clickableError();
	} else if ( loc.file.startsWith("pengine://") ) {
	  econtext.location = {line:loc.line};
	  clickableError(data.query.editor);
	}
	registerSources(msg.pengine);
	msg.error_context = econtext;
	msg.error_handler = gotoError;
	$(".swish-event-receiver").trigger("source-error", msg);
      } else {
	var span = elem.prologRunner('outputHTML',
				     clickableLocations(msg.data,
							econtext.editor));
	$(span).on("click", gotoError);
	$(span).data("error_context", econtext);
      }
    } else if ( typeof(msg.data) == 'object' ) {
      elem.prologRunner(msg.data.action, msg.data);
    } else {
      console.log(msg.data);
    }
    RS(elem).prologRunners('scrollToBottom');
  }

  function handleError() {
    var elem = this.pengine.options.runner;
    var msg;

    if ( this.code == "too_many_pengines" ) {
      this.message = "Too many open queries.  Please complete some\n"+
		     "queries by using |Next|, |Stop| or by\n"+
		     "closing some queries.";
    } else if ( typeof(this.data) == 'string' ) {
      this.message = this.data
			 .replace(new RegExp("'"+this.pengine.id+"':", 'g'), "");
    } else {
      this.message = "Unknown error";
    }

    elem.prologRunner('error', this);
    elem.prologRunner('setState', "error");
  }

  function handleAbort() {
    var elem = this.pengine.options.runner;

    elem.prologRunner('error', "** Execution aborted **");
    elem.prologRunner('setState', "aborted");
  }

  function handlePing() {
    var elem = this.pengine.options.runner;

    elem.prologRunner('ping', this.data);
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

  function glyphButton(glyph, title) {
    var btn = $.el.a({href:"#", class:"close btn btn-link btn-sm",
		      title:title},
		     $.el.span({class:"glyphicon glyphicon-"+glyph}));

    return btn;
  }
});
