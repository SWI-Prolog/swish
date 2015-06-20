/**
 * @fileOverview
 * Manage the cell structure of a notebook modelled after IPython
 * NoteBook.  The nodebook consists of a toolbar with a series of
 * buttons and manages a list of cells.  The file defines two plugins
 * `notebook`, implementing the overall notebook and `nbCell`,
 * implementing a single cell.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define([ "jquery", "config", "tabbed",
	 "laconic", "runner", "storage", "sha1"
       ],
       function($, config, tabbed) {

var cellTypes = {
  "program":  { label:"Program" },
  "query":    { label:"Query" },
  "markdown": { label:"Markdown" },
};

(function($) {
  var pluginName = 'notebook';
  var clipboard = null;

  /** @lends $.fn.notebook */
  var methods = {
    /**
     * Initialize a Prolog Notebook.
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var storage = {};		/* storage info */
	var data = {};			/* private data */
	var toolbar, content;

	elem.addClass("notebook");
	elem.addClass("swish-event-receiver");

	elem.append(toolbar = $.el.div(
            {class:"nb-toolbar"},
	    glyphButton("trash", "delete", "Delete cell"),
	    glyphButton("copy", "copy", "Copy cell"),
	    glyphButton("paste", "paste", "Paste cell below"),
	    sep(),
	    glyphButton("chevron-up", "up", "Move cell up"),
	    glyphButton("chevron-down", "down", "Move cell down"),
	    sep(),
	    glyphButton("plus", "insertBelow", "Insert cell below"),
	    sep(),
	    glyphButton("play", "run", "Run")
	    ));
	elem.append($.el.div({class:"nb-view"},
			     content=$.el.div({class:"nb-content"}),
			     $.el.div({class:"nb-bottom"})));

	$(toolbar).on("click", "a.btn", function(ev) {
	  var action = $(ev.target).closest("a").data("action");
	  elem.notebook(action);
	  ev.preventDefault();
	  return false;
	});

	$(content).on("click", ".nb-cell-buttons a.btn", function(ev) {
	  var a    = $(ev.target).closest("a");
	  var cell = a.closest(".nb-cell");

	  var action = a.data("action");
	  cell.nbCell(action);
	  ev.preventDefault();
	  return false;
	});

	elem.focusin(function(ev) {
	  elem.notebook('active', $(ev.target).closest(".nb-cell"));
	});

	elem.data(pluginName, data);	/* store with element */

					/* restore content */
	var content = elem.find(".notebook-data");
	if ( content.length > 0 ) {
	  function copyData(name) {
	    var value = content.data(name);
	    if ( value ) storage[name] = value;
	  }

	  copyData("file");
	  copyData("title");
	  if ( window.swish && window.swish.meta_data )
	    storage.meta = window.swish.meta_data;

	  elem.notebook('value', content.text());
	  content.remove();
	} else {
	  elem.notebook('placeHolder');
	}

	elem.notebook('setupStorage', storage);
      });
    },

		 /*******************************
		 *	  BUTTON ACTIONS	*
		 *******************************/

    delete: function(cell) {
      cell = cell||currentCell(this);
      if ( cell ) {
	this.notebook('active', cell.next());
	cell.remove();
	this.notebook('updatePlaceHolder');
      }
      return this;
    },

    copy: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	clipboard = $(cell).nbCell('saveDOM');
    },

    paste: function() {
      if ( clipboard ) {
	var newcell = $.el.div({class:"nb-cell"});
	this.notebook('insert', { where:"below", cell:newcell });
	$(newcell).nbCell($(clipboard));
      } else {
	alert("Clipboard is empty");
      }
    },

    up: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.insertBefore(cell.prev());
      return this;
    },

    down: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.insertAfter(cell.next());
      return this;
    },

    insertAbove: function() {
      return this.notebook('insert', { where:"above" });
    },

    insertBelow: function() {
      return this.notebook('insert', { where:"below" });
    },

    run: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.nbCell("run");
    },

    cellType: function(cell, type) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.nbCell('type', type);
    },

		 /*******************************
		 *	 CELL MANAGEMENT	*
		 *******************************/

    /**
     * @param {jQuery} cell is the cell that must be activated
     * @param {Boolean} [focus] if `true`, give the cell the focus
     */
    active: function(cell, focus) {
      if ( cell && cell.length == 1 )
      { var current = this.find(".nb-content").children(".nb-cell.active");

	if ( !(current.length == 1 && cell[0] == current[0]) ) {
	  current.nbCell('active', false);
	  cell.nbCell('active', true);
	  if ( focus )
	    cell.focus();
	}
      }
    },

    /**
     * Insert a new cell
     * @param {Object} [options]
     * @param {String} [options.where] defines where the cell is
     * inserted relative to the cell with the current focus.
     */
    insert: function(options) {
      options   = options||{};
      var relto = currentCell(this);
      var cell  = options.cell || $.el.div({class:"nb-cell"});

      if ( relto ) {
	if ( options.where == 'above' ) {
	  $(cell).insertBefore(relto);
	} else {
	  $(cell).insertAfter(relto);
	}
      } else {
	this.find(".nb-content").append(cell);
      }

      if ( !options.cell )
	$(cell).nbCell();
      this.notebook('updatePlaceHolder');
      this.notebook('active', $(cell));
    },

		 /*******************************
		 *	   SAVE/RESTORE		*
		 *******************************/

    /**
     * Setup connection to the storage manager.
     */
    setupStorage: function(storage) {
      var notebook = this;

      storage = $.extend(storage, {
        getValue: function() {
	  return notebook.notebook('value');
	},
	setValue: function(string) {
	  return notebook.notebook('value', string);
	},
	changeGen: function() {
	  return notebook.notebook('changeGen');
	},
	isClean: function(gen) {
	  var cgen = notebook.notebook('changeGen');
	  return gen == cgen;
	},
	cleanGeneration: this.notebook('changeGen'),
	cleanData:       this.notebook('value'),
	cleanCheckpoint: "load",
	dataType:        "swinb",
	typeName:	 "notebook"
      });

      return this.storage(storage);
    },

    /**
     * Set or get the state of this notebook as a string.
     * @param [String] val is an HTML string that represents
     * the notebook state.
     */
    value: function(val) {
      if ( val == undefined ) {
	var dom = $.el.div({class:"notebook"});
	this.find(".nb-cell").each(function() {
	  cell = $(this);
	  $(dom).append(cell.nbCell('saveDOM'));
	});

	var html = $($.el.div(dom)).html();
	return html.replace(/(<div [^>]*>|<\/div>)/g, function(t) {
	  return "\n"+t+"\n";
	}).slice(1);
      } else {
	var notebook = this;
	var content  = this.find(".nb-content");
	var dom = $.el.div();

	content.html("");
	$(dom).html(val);
	$(dom).find(".nb-cell").each(function() {
	  var cell = $.el.div({class:"nb-cell"});
	  content.append(cell);
	  $(cell).nbCell($(this));
	});

	this.notebook('updatePlaceHolder');
      }
    },

    /**
     * Compute a state fingerprint for the entire notebook
     * @return {String} SHA1 fingerprint
     */
    changeGen: function() {
      var list = [];
      this.find(".nb-cell").each(function() {
	cell = $(this);
	list.push(cell.nbCell('changeGen'));
      });
      return sha1(list.join());
    },

		 /*******************************
		 *	       HELP		*
		 *******************************/

    updatePlaceHolder: function() {
      if ( this.find(".nb-content").children().length == 0 )
	this.notebook('placeHolder');
      else
	this.find(".nb-placeholder").remove();
    },

    placeHolder: function() {
      var placeholder = $.el.div({class:"nb-placeholder"});

      $.ajax({ url: config.http.locations.help + "/notebook.html",
	       dataType: "html",
	       success: function(data) {
		 $(placeholder).html(data);
	       }
             });
      this.find(".nb-content").append(placeholder);
    }
  }; // methods

  // <private functions>

  /**
   * @returns {Object|null} cell that is focussed and inside our
   * notebook.
   * @param {Object} nb is the notebook
   */
  function currentCell(nb) {
    var active = $(nb).find(".nb-cell.active");

    if ( active.length == 1 )
      return active.first();

    return null;
  }

  tabbed.tabTypes.notebook = {
    dataType: "swinb",
    typeName: "notebook",
    label: "Notebook",
    create: function(dom) {
      $(dom).notebook();
    }
  };

  /**
   * <Class description>
   *
   * @class notebook
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.notebook = function(method) {
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
		 *	    PLUGIN nbCell	*
		 *******************************/

(function($) {
  var pluginName = 'nbCell';
  var id = 0;				/* generate unique cell ids */

  /** @lends $.fn.nbCell */
  var methods = {
    /**
     * Create a new notebook cell
     * @param {jQuery} [dom] initialise the new cell from the saved
     * DOM
     */
    _init: function(dom) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	var g;

	elem.data(pluginName, data);	/* store with element */
	elem.attr("tabIndex", -1);
	elem.attr("id", "nbc"+id++);

	if ( dom instanceof jQuery ) {
	  elem.nbCell('restoreDOM', dom);
	} else {
	  elem.append($.el.div({class:"nb-type-select"},
			       $.el.label("Create a "),
			       g=$.el.div({class:"btn-group",role:"group"}),
			       $.el.label("cell here.")));

	  for(var k in cellTypes) {
	    if ( cellTypes.hasOwnProperty(k) )
	      $(g).append($.el.button({ type:"button",
					class:"btn btn-default",
					"data-type":k
				      },
				      cellTypes[k].label));
	  }

	  $(g).on("click", ".btn", function(ev) {
	    elem.nbCell('type', $(ev.target).data('type'));
	  });
	}
      });
    },

    /**
     * (de)activate the current cell.
     */
    active: function(val) {
      var data = this.data(pluginName);

      if ( val ) {
	this.addClass("active");
	switch( data.type ) {
	  case "program":
	    this.find(".editor").prologEditor('makeCurrent');
	    break;
	}
      } else if ( this.length > 0 ) {
	this.removeClass("active");
	switch( data.type ) {
	  case "markdown":
	    if ( this.hasClass("runnable") ) {
	      this.nbCell('run');
	    }
	    break;
	}
      }
    },


    type: function(type) {
      var data = this.data(pluginName);
      if ( data.type != type ) {
	methods.type[type].apply(this);
	data.type = type;
	this.addClass(type);
      }
      return this;
    },

    /**
     * Run the current cell
     */
    run: function() {
      if ( this.hasClass("runnable") ) {
	var data = this.data(pluginName);

	return methods.run[data.type].apply(this, arguments);
      } else {
	alert("Cell is not runnable");
      }
      return this;
    },

    runTabled: function() {
      return this.nbCell('run', {tabled:true});
    },

    /**
     * Currently simply returns the preceeding program cell.
     * @return {jQuery} set of prologEditor elements that form the
     * sources for the receiving query cell.
     */
    programs: function() {
      var data = this.data(pluginName);

      return this.prevAll(".program").first().find(".editor");
    },

    saveDOM: function() {
      return methods.saveDOM[this.data(pluginName).type].call(this);
    },

    restoreDOM: function(dom) {
      var data = this.data(pluginName);

      function domCellType(dom) {
	for(var k in cellTypes) {
	  if ( cellTypes.hasOwnProperty(k) && dom.hasClass(k) )
	    return k;
	}
      }

      data.type = domCellType(dom);
      methods.restoreDOM[data.type].apply(this, arguments);
      this.addClass(data.type);
    },

    /**
     * Compute a state fingerprint for the current cell.
     */
    changeGen: function() {
      return methods.changeGen[this.data(pluginName).type].call(this);
    }
  }; // methods

		 /*******************************
		 *	     SET TYPE		*
		 *******************************/

  methods.type.markdown = function(options) {	/* markdown */
    var editor;

    options = options||{};
    options.mode = "markdown";

    this.html("");
    this.append(editor=$.el.div({class:"editor"}));
    $(editor).prologEditor(options);
    this.addClass("runnable");
  }

  methods.type.program = function(options) {	/* program */
    var editor;

    options = options||{};

    this.html("");
    this.append(editor=$.el.div({class:"editor"}));
    $(editor).prologEditor(options);
  }

  methods.type.query = function(options) {	/* query */
    var editor;
    var cell = this;

    options = $.extend({}, options,
      { role: "query",
	sourceID: function() {
	  return cell.nbCell('programs').prologEditor('getSourceID');
	},
	prologQuery: function(q) {
	  cell.nbCell('run');
	}
      });

    this.html("");
    this.append($.el.div($.el.div({class:"nb-cell-buttons"},
      {class:"btn-group nb-cell-buttons",role:"group"},
      glyphButton("play", "run",       "Run query",                 "xs"),
      glyphButton("th",   "runTabled", "Run query (table results)", "xs"))));

    this.append($.el.div({class:"query"},
			 $.el.span({class:"prolog-prompt"}, "?-"),
			 editor=$.el.div({class:"editor query"})));
    $(editor).prologEditor(options);
    this.addClass("runnable");
  }


		 /*******************************
		 *	    RUN BY TYPE		*
		 *******************************/

  methods.run.markdown = function(markdownText) {	/* markdown */
    var cell = this;

    markdownText = markdownText||cellText(this);

    function makeEditable(ev) {
      var cell = $(ev.target).closest(".nb-cell");
      var text = cell.data('markdownText');
      cell.removeData('markdownText');
      methods.type.markdown.call(cell, {value:text});
      cell.off("dblclick", makeEditable);
      cell.off("click", followLink);
    }

    function followLink(ev) {
      var a = $(ev.target).closest("a");

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

      if ( a.hasClass("swinb") ) {
	$(ev.target).parents(".swish").swish('playFile', a.attr("href"));
	ev.preventDefault();
      } else if ( a.hasClass("builtin") ) {
	var s    = a.attr("href").split("predicate=").pop();
	var pred = parsePred(s);

	if ( pred ) {
	  $(".swish-event-receiver").trigger("pldoc", pred);
	  ev.preventDefault();
	}
      }
    }

    $.get(config.http.locations.markdown,
	  { text: markdownText
	  },
	  function(data) {
	    cell.html(data);
	    cell.removeClass("runnable");
	    cell.data('markdownText', markdownText);
	    cell.on("dblclick", makeEditable);
	    cell.on("click", "a", followLink);
	  });
  };

  methods.run.program = function() {		/* program */
    alert("Please define a query to run this program");
  };

  methods.run.query = function(options) {	/* query */
    var programs = this.nbCell('programs');

    options = options||{};
    var query = { source: programs.prologEditor('getSource'),
                  query: cellText(this),
		  tabled: options.tabled||false,
		  title: false
                };
    var runner = $.el.div({class: "prolog-runner"});
    this.find(".prolog-runner").remove();
    this.append(runner);
    $(runner).prologRunner(query);
  };

		 /*******************************
		 *	SAVE/RESTORE DOM	*
		 *******************************/

/* ---------------- saveDOM ---------------- */

  methods.saveDOM.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return $.el.div({class:"nb-cell markdown"}, text);
  };

  methods.saveDOM.program = function() {	/* program */
    return $.el.div({class:"nb-cell program"}, cellText(this));
  };

  methods.saveDOM.query = function() {		/* query */
    return $.el.div({class:"nb-cell query"}, cellText(this));
  };

/* ---------------- restoreDOM ---------------- */

  methods.restoreDOM.markdown = function(dom) {	/* markdown */
    var text = dom.text().trim();
    this.data('markdownText', text);
    methods.run.markdown.call(this, text);
  };

  methods.restoreDOM.program = function(dom) {	/* program */
    methods.type.program.call(this, {value:dom.text().trim()});
  };

  methods.restoreDOM.query = function(dom) {	/* query */
    methods.type.query.call(this, {value:dom.text().trim()});
  };

/* ---------------- changeGen ---------------- */

  methods.changeGen.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return sha1(text);
  };

  methods.changeGen.program = function() {	/* program */
    return sha1(cellText(this));
  };

  methods.changeGen.query = function() {	/* query */
    return sha1(cellText(this));
  };


		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

  function cellText(cell) {
    return cell.find(".editor").prologEditor('getSource');
  }

  /**
   * <Class description>
   *
   * @class nbCell
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.nbCell = function(method) {
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

function glyphButton(glyph, action, title, size) {
  size = size||"sm";
  var btn = $.el.a({href:"#", class:"btn btn-info btn-"+size,
		    title:title, "data-action":action},
		   $.el.span({class:"glyphicon glyphicon-"+glyph}));

  return btn;
}

function sep() {
  return $.el.span({class:"thin-space"}, " ");
}
});
