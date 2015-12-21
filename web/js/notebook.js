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

define([ "jquery", "config", "tabbed", "form", "preferences",
	 "laconic", "runner", "storage", "sha1"
       ],
       function($, config, tabbed, form, preferences) {

var cellTypes = {
  "prolog":   { label:"Prolog" },
  "lpad":     { label:"LPAD" },
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
	    glyphButton("trash", "delete", "Delete cell", "warning"),
	    glyphButton("copy", "copy", "Copy cell", "default"),
	    glyphButton("paste", "paste", "Paste cell below", "default"),
	    sep(),
	    glyphButton("chevron-up", "up", "Move cell up", "default"),
	    glyphButton("chevron-down", "down", "Move cell down", "default"),
	    sep(),
	    glyphButton("plus", "insertBelow", "Insert cell below", "primary")
	    ));
	elem.append($.el.div({class:"nb-view", tabIndex:"-1"},
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
	  var cell = $(ev.target).closest(".nb-cell");
	  if ( cell.length > 0 ) {
	    elem.notebook('active', cell);
	  } else if ( $(ev.target).closest(".nb-view").length > 0 )
	  { elem.find(".nb-content").children(".nb-cell.active")
				    .nbCell('active', false);
	  }
	});
	elem.focusout(function(ev) {
	  if ( $(ev.target).closest(".notebook")[0] != elem[0] ) {
	    elem.find(".nb-content").children(".nb-cell.active")
				    .nbCell('active', false);
	  }
	});

	/* Activate the active source or first source.  If the active
	 * cell is a query, we could activate the source of the query?
	 */
	elem.on("activate-tab", function(ev) {
	  if ( ev.target == elem[0] ) {
	    var eds = elem.find(".nb-content")
                          .children(".nb-cell.program");
	    var aeds = eds.filter(".active");
	    var nc = aeds[0]||eds[0];

	    if ( nc ) {
	      $(nc).find(".prolog-editor").prologEditor('makeCurrent');
	    }
	    ev.stopPropagation();
	  }
	});

	elem.data(pluginName, data);	/* store with element */

					/* restore content */
	var content = elem.find(".notebook-data");
	if ( content.length > 0 ) {
	  function copyData(name) {
	    var value = content.data(name);
	    if ( value ) {
	      storage[name] = value;
	    }
	  }

	  copyData("file");
	  copyData("url");
	  copyData("title");
	  copyData("meta");
	  copyData("st_type");

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
	this.notebook('active', cell.next()||cell.prev());
	cell.nbCell('close');
	this.notebook('updatePlaceHolder');
      }
      return this;
    },

    copy: function(cell) {
      cell = cell||currentCell(this);
      if ( cell ) {
	var dom = $.el.div({class:"notebook"});
	$(dom).append($(cell).nbCell('saveDOM'));
	clipboard = stringifyNotebookDOM(dom);
      }
    },

    paste: function(text) {
      var nb = this;

      text = text||clipboard;
      if ( text ) {
	var dom = $.el.div();

	$(dom).html(text);
	var cells = $(dom).find(".nb-cell");
	if ( cells.length > 0 ) {
	  $(dom).find(".nb-cell").each(function() {
	    nb.notebook('insert', {
	      where: "below",
	      restore: $(this)
	    });
	  });
	  return this;
	} else {
	  alert("Not a SWISH notebook");
	}
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
      if ( cell ) {
	var current = this.find(".nb-content .nb-cell.active");

	function removeNotForQuery(elem) {
	  elem.find(".nb-content .nb-cell.not-for-query")
	      .removeClass("not-for-query");
	}

	if ( cell.length == 1 )
	{ if ( !(current.length == 1 && cell[0] == current[0]) ) {
	    removeNotForQuery(this);
	    current.nbCell('active', false);
	    cell.nbCell('active', true);
	    if ( focus )
	      cell.focus();
	  }
	} else
	{ removeNotForQuery(this);
	  current.nbCell('active', false);
	}
      }
    },

    /**
     * Insert a new cell
     * @param {Object} [options]
     * @param {String} [options.where] defines where the cell is
     * inserted relative to the cell with the current focus.
     * @param {jQuery} [options.restore] If provided, it must contains
     * a save/restore node that will be used to fill the new cell.
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

      if ( !options.cell ) {
	$(cell).nbCell(options.restore);
      }
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
	setValue: function(source) {
	  return notebook.notebook('setSource', source);
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
	typeName:	 "notebook"
      });

      return this.storage(storage);
    },

    /**
     * Set the source
     */
    setSource: function(source) {
      if ( typeof(source) == "string" )
	source = {data:source};

      this.notebook('value', source.data);
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
	  if ( !cell.nbCell('isEmpty') )
	    $(dom).append(cell.nbCell('saveDOM'));
	});

	return stringifyNotebookDOM(dom);
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

	this.find(".nb-cell.query").nbCell('onload');
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

  function stringifyNotebookDOM(dom) {
    /*
     * Attributes from .html() are not ordered.  We need a canonical
     * representation and therefore we need to reorder the HTML
     * attributes and map the attribute names to lower case.
     */
    function orderAttrs(s) {
      attrs = s.match(/[-a-z]+="[^"]*"/g);
      if ( attrs ) {
	var start = s.match(/^<[a-z]* /);
	for(var i=0; i<attrs.length; i++) {
	  var l = attrs[i].split(/=(.*)/);
	  attrs[i] = l[0].toLowerCase()+"="+l[1];
	}
	return start[0]+attrs.sort().join(" ")+">";
      } else
	return s;
    }

    var html = $($.el.div(dom)).html();
    return html.replace(/(<div [^>]*>|<\/div>)/g, function(t) {
      return "\n"+orderAttrs(t)+"\n";
    }).slice(1);
  }

  tabbed.tabTypes.notebook = {
    dataType: "swinb",
    typeName: "notebook",
    label: "Notebook",
    contentType: "text/x-prolog-notebook",
    order: 200,
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
					"data-type":((k=="prolog" || k=="lpad")?"program" : k),
					"data-code":k
				      },
				      cellTypes[k].label));
	  }

	  $(g).on("click", ".btn", function(ev) {
	    if ($(ev.target).data('code') == "prolog" || $(ev.target).data('code') == "lpad")
	    	elem.nbCell('type', $(ev.target).data('type'), $(ev.target).data('code'));
	    else
	        elem.nbCell('type', $(ev.target).data('type'));
	  });

	  elem.append($.el.div({class:"nb-type-more"},
			       typeMore(),
			       typeLess($.el.label("Insert notebook from " +
						   "local file "),
					fileInsertInput()[0])));
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
	switch( data.st_type ) {
	  case "program":
	    this.find(".editor").prologEditor('makeCurrent');
	    break;
	  case "query":
	    this.closest(".notebook")
                .find(".nb-cell.program")
                .not(this.nbCell("program_cells"))
                .addClass("not-for-query");
	    break;
	}
      } else if ( this.length > 0 ) {
	this.removeClass("active");
	switch( data.st_type ) {
	  case "markdown":
	    if ( this.hasClass("runnable") ) {
	      this.nbCell('run');
	    }
	    break;
	}
      }
    },


    type: function(type, code) {
      var data = this.data(pluginName);
        
        if ( data.st_type != type ) {
          if (type == "program"){
            methods.type[type].apply(this, [{codeType : code}]); //fix it!!!! ci siamo quasi
	  } else {
	    methods.type[type].apply(this);
	  }
	  data.st_type = type;
	  this.addClass(type);
        }
        return this;
      
    },

    /**
     * Run the current cell
     */
    run: function() {
      return this.each(function() {
	var cell = $(this);
	if ( cell.hasClass("runnable") ) {
	  var data = cell.data(pluginName);

	  return methods.run[data.st_type].apply(cell, arguments);
	} else {
	  console.log("Cell is not runnable: ", cell);
	}
      });
    },

    runTabled: function() {
      return this.nbCell('run', {tabled:true});
    },

    onload: function() {
      return this.each(function() {
	var cell = $(this);
	if ( cell.data("run") == "onload" )
	  cell.nbCell("run");
      });
    },

    close: function() {
      this.find(".prolog-runner").prologRunner('close');
      return this.remove();
    },

    getSettings: function() {
      return {
        tabled: this.data("tabled") == "true",
	run:    this.data("run")    == "onload",
	chunk:  parseInt(this.data("chunk")||"1")
      };
    },

    /**
     * Present a modal that shows the current query properties and
     * allows for changing them.
     */
    settings: function() {
      var elem    = this;
      var current = this.nbCell('getSettings');

      function querySettingsBody() {
	this.append($.el.form(
          { class:"form-horizontal"
	  },
	  form.fields.checkboxes(
		[ { name: "tabled",
		    label: "table results",
		    value: current.tabled,
		    title: "Table results"
		  },
		  { name: "run",
		    label: "run on page load",
		    value: current.run,
		    title: "Run when document is loaded"
		  }
		]),
	  form.fields.chunk(current.chunk),
	  form.fields.buttons(
	  { label: "Apply",
	    offset: 3,
	    action: function(ev, values) {
	      if ( values.tabled != current.tabled ) {
		if ( values.tabled )
		  elem.data("tabled", "true");
		else
		  elem.removeData("tabled");
	      }
	      if ( values.run != current.run ) {
		if ( values.run )
		  elem.data("run", "onload");
		else
		  elem.removeData("run");
	      }
	      if ( values.chunk != current.chunk ) {
		if ( values.chunk != 1 )
		  elem.data("chunk",  ""+values.chunk);
		else
		  elem.removeData("chunk");
	      }
	    }
	  })));
      }

      form.showDialog({ title: "Set options for query",
                        body: querySettingsBody
                      });
    },

    /**
     * Change the editor of a program cell to fixed (one line) height
     */
    singleline: function() {
      this.toggleClass("singleline");
      this.find(".editor").prologEditor('refresh');
      glyphButtonGlyph(this, "singleline",
		       this.hasClass("singleline")
				? "triangle-left"
				: "triangle-bottom");
      this.find("a[data-action=singleline]").blur();
      return this;
    },

    /**
     * Toggle a program fragment to be background/non-background
     */
    background: function() {
      this.toggleClass("background");
      this.find("a[data-action=background]").blur();
      return this;
    },

    /**
     * Returns all program cells in current notebook that are loaded
     * for executing the receiving query.
     * @return {jQuery} set of nbCell elements that form the
     * sources for the receiving query cell.
     */
    program_cells: function() {
      var data = this.data(pluginName);
      var programs = this.closest(".notebook")
	                 .find(".nb-cell.program.background")
			 .add(this.prevAll(".program").first());
      return programs;
    },

    programs: function() {
      var cells = this.nbCell('program_cells');
      return cells.find(".editor");
    },

    isEmpty: function() {
      var type = this.data(pluginName).st_type;
      if (type == "lpad" || type == "prolog")
        return methods.isEmpty["program"].call(this);
      else return methods.isEmpty[type].call(this);
    },

    saveDOM: function() {
      var type = this.data(pluginName).st_type;
      if (type == "lpad" || type == "prolog")
        return methods.saveDOM["program"].call(this);
      else return methods.saveDOM[type].call(this);
    },

    restoreDOM: function(dom) {
      var data = this.data(pluginName);

      function domCellType(dom) {
        console.log(dom);
	for(var k in cellTypes) {
	  if ( cellTypes.hasOwnProperty(k) && dom.hasClass(k) )
	    return k;
	}
      }

      console.log("a"); 
      console.log(data);
      data.st_type = domCellType(dom);
            console.log("b");
            console.log(data);
                  console.log("c");
                  console.log(data.st_type);
      var type;
      if (data.st_type == "lpad" || data.st_type == "prolog")
        type = "program";
      else type = data.st_type;
      if (type == undefined) type = "program";
      methods.restoreDOM[type].apply(this, arguments);
      this.addClass(type);
    },

    /**
     * Compute a state fingerprint for the current cell.
     */
    changeGen: function() {
      var type = this.data(pluginName).st_type;
      if (type == "lpad" || type == "prolog")
        return methods.changeGen["program"].call(this);
      else
        return methods.changeGen[type].call(this);
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
    var editor, bg;
    /*if (options && options.codeType == undefined)
    	options = {};*/
    options = options||{};
    if (options.codeType)
    	if(options.codeType == "lpad")
    	  options.placeholder = "Your LPAD rules and facts go here ...";
         
    options.autoCurrent = false;
    

    
    this.html("");

    var buttons = $.el.div(
      {class:"btn-group nb-cell-buttons", role:"group"},
      glyphButton("triangle-bottom", "singleline", "Show only first line",
		  "default", "xs"),
      bg=glyphButton("cloud", "background", "Use as background program",
		     "success", "xs"));
    this.append(buttons,
		editor=$.el.div({class:"editor with-buttons"}));
    if ( options.background )
    { this.addClass("background");
    }
    if ( options.singleline )
    { this.nbCell('singleline');
    }

    $(editor).prologEditor(options);
    
  }
  


  methods.type.query = function(options) {	/* query */
    var editor;
    var cell = this;

    this.html("");

    options = options||{};
    if ( options.tabled == undefined )
      options.tabled = preferences.getVal("tabled_results");

    function setAttr(name) {
      if ( options[name] != undefined ) {
	cell.data(name, ""+options[name]);
	delete options[name];
      }
    }
    setAttr("tabled");
    setAttr("chunk");
    setAttr("run");

    options = $.extend({}, options,
      { role: "query",
	sourceID: function() {
	  return cell.nbCell('programs').prologEditor('getSourceID');
	},
	prologQuery: function(q) {
	  cell.nbCell('run');
	}
      });

    var buttons = $.el.div(
      {class:"btn-group nb-cell-buttons", role:"group"},
      glyphButton("wrench", "settings", "Settings",
		  "default", "xs"),
      glyphButton("play", "run",       "Run query",
		  "primary", "xs"));

    this.append(buttons,
		$.el.div({class:"query with-buttons"},
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

      if ( a.hasClass("store") ) {
	done = true;
	ev.preventDefault();
	var swishStore = config.http.locations.swish + "p/";
	var href = a.attr("href");
	if ( href.startsWith(swishStore) ) {
	  file = href.slice(swishStore.length);
	  $(ev.target).parents(".swish").swish('playFile', file);
	} else {
	  alert("File does not appear to come from gitty store?");
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

    function setHTML(data) {
      cell.html(data);
      cell.removeClass("runnable");
      cell.data('markdownText', markdownText);
      cell.on("dblclick", makeEditable);
      cell.on("click", "a", followLink);
    }

    if ( markdownText.trim() != "" )
    { $.ajax({ type: "POST",
	       url: config.http.locations.markdown,
	       data: markdownText,
	       contentType: "text/plain; charset=UTF-8",
	       success: setHTML
	     });
    } else
    { setHTML("");
    }
  };

  methods.run.program = function() {		/* program */
    alert("Please define a query to run this program");
  };

  methods.run.query = function(options) {	/* query */
    var programs = this.nbCell('programs');
    var settings = this.nbCell('getSettings');
    
    options = options||{};
    var query = { source: programs.prologEditor('getSource'),
                  query:  cellText(this),
		  tabled: settings.tabled||false,
		  chunk:  settings.chunk,
		  title:  false,
		  codeType: programs.prologEditor('getCodeType')
                };
    if ( programs[0] )
      query.editor = programs[0];

    var runner = $.el.div({class: "prolog-runner"});
    this.find(".prolog-runner").prologRunner('close');
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
    var cell = this;
    console.log("savedom"); console.log(cell);
    var codetype = cellCode(cell);
    var dom = $.el.div({class:"nb-cell program " + codetype}, cellText(this));

    function copyClassAttr(name) {
      if ( cell.hasClass(name) ) {
	$(dom).attr("data-"+name, true);
      }
    }

    copyClassAttr("background");
    copyClassAttr("singleline");

    return dom;
  };

  methods.saveDOM.query = function() {		/* query */
    var cell = this;
    var dom  = $.el.div({class:"nb-cell query"}, cellText(this));

    function isDefault(name, value) {
      if ( name == 'tabled' && (!value || value == "false") ) return true;
      return false;
    }

    function copyAttr(name) {
      var value;
      if ( (value=cell.data(name)) && !isDefault(name,value) ) {
	$(dom).attr("data-"+name, value);
      }
    }

    copyAttr("tabled");
    copyAttr("chunk");
    copyAttr("run");

    return dom;
  };

/* ---------------- restoreDOM ---------------- */

  methods.restoreDOM.markdown = function(dom) {	/* markdown */
    var text = dom.text().trim();
    this.data('markdownText', text);
    methods.run.markdown.call(this, text);
  };

  methods.restoreDOM.program = function(dom) {	/* program */
    var opts = { value:dom.text().trim() };

    function getAttr(name) {
      var value;
      if ( (value=dom.data(name)) ) {
	opts[name] = value;
      }
    }

    getAttr("background");
    getAttr("singleline");

    methods.type.program.call(this, opts);
  };

  methods.restoreDOM.query = function(dom) {	/* query */
    var opts = { value:dom.text().trim() };

    function getAttr(name) {
      var value;
      if ( (value=dom.data(name)) ) {
	if ( name == "chunk" )
	  opts.chunk = parseInt(value);
	else
	  opts[name] = value;
      }
    }

    getAttr("tabled");
    getAttr("chunk");
    getAttr("run");
    if ( opts.tabled == undefined )
      opts.tabled = false;

    methods.type.query.call(this, opts);
  };

/* ---------------- changeGen ---------------- */

  methods.changeGen.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return sha1(text);
  };

  methods.changeGen.program = function() {	/* program */
    var text = "";
    var cell = this;

    function addClassAttr(name, key) {
      if ( cell.hasClass(name) )
	text += key;
    }

    addClassAttr("background", "B");
    addClassAttr("singleline", "S");

    text += "V"+cellText(this);
    return sha1(text);
  };

  methods.changeGen.query = function() {	/* query */
    var text = "";
    var cell = this;

    function addAttr(name, key) {
      var value;

      if ( (value=cell.data(name)) )
	text += key+value;
    }

    addAttr("tabled", "T");
    addAttr("chunk", "C");
    addAttr("run", "R");
    text += "V"+cellText(this);

    return sha1(text);
  };

/* ---------------- isEmpty ---------------- */

  methods.isEmpty.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return text.trim() == "";
  };

  methods.isEmpty.program = function() {	/* program */
    return cellText(this).trim() == "";
  };

  methods.isEmpty.query = function() {		/* query */
    return cellText(this).trim() == "";
  };

		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

  function cellText(cell) {
    return cell.find(".editor").prologEditor('getSource');
  }
  
  function cellCode(cell) {
    return cell.find(".editor").prologEditor('getCodeType');
  }

  /**
   * Creates a file input that, after a file is selected, replaces
   * the cell with the content of the local file.
   */
  function fileInsertInput() {
    var form = $('<input type="file" name="file">');

    form.on("change", function(ev) {
      var reader = new FileReader();
      reader.onload = function(theFile) {
	var cell = $(ev.target).closest(".nb-cell");
	var nb   = cell.closest(".notebook");

	if ( nb.notebook('paste', reader.result) )
	  cell.remove();
      };
      reader.readAsText(ev.target.files[0]);

      ev.preventDefault();
      return false;
    });

    return form;
  }

  function typeMore() {
    var div = $('<div class="form-more">' +
		' <a href="#">more<a>' +
		'</div>');
    div.find("a").on("click", function(ev) {
      var more = $(ev.target).closest(".form-more");
      more.hide(400);
      more.next().show(400);
    });
    return div[0];
  }

  function typeLess() {
    var div = $('<div class="form-less" style="display:none">' +
		' <div><a href="#" class="less">less<a></div>' +
		'</div>');
    for(var i=0; i<arguments.length; i++) {
      div.append(arguments[i]);
    }
    div.find("a.less").on("click", function(ev) {
      var less = $(ev.target).closest(".form-less");
      less.hide(400);
      less.prev().show(400);
    });

    return div[0];
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

function glyphButton(glyph, action, title, style, size) {
  size = size||"sm";
  var btn = $.el.a({href:"#", class:"btn btn-"+style+" btn-"+size,
		    title:title, "data-action":action},
		   $.el.span({class:"glyphicon glyphicon-"+glyph}));

  return btn;
}

function glyphButtonGlyph(elem, action, glyph) {
  var span = elem.find("a[data-action="+action+"] > span.glyphicon");

  span.removeClass(function(i,s) {
    return s.match(/glyphicon-[-a-z]*/g).join(" ");
  }).addClass("glyphicon-"+glyph);
}

function sep() {
  return $.el.span({class:"thin-space"}, " ");
}
});
