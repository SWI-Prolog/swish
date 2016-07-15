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

define([ "jquery", "config", "tabbed", "form",
	 "preferences", "modal", "prolog", "links",
	 "laconic", "runner", "storage", "sha1"
       ],
       function($, config, tabbed, form, preferences, modal, prolog, links) {

var cellTypes = {
  "prolog":   { label:"Prolog" },
  "lpad":     { label:"LPAD" },
  "query":    { label:"Query" },
  "markdown": { label:"Markdown" },
  "html":     { label:"HTML" }
};

(function($) {
  var pluginName = 'notebook';
  var clipboard = null;

  /** @lends $.fn.notebook */
  var methods = {
    /**
     * Initialize a Prolog Notebook.
     * @param {Object} options
     * @param {String} [options.value] provides the initial content
     * @param {Boolean} [options.fullscreen] open notebook in fullscreen
     * mode.
     */
    _init: function(options) {
      options = options||{};
      return this.each(function() {
	var elem = $(this);
	var storage = {};		/* storage info */
	var data = {};			/* private data */
	var toolbar, content;

	elem.addClass("notebook");
	elem.addClass("swish-event-receiver");

	function notebookMenu() {
	  var icon = $.el.span({class:"glyphicon glyphicon-menu-hamburger"});
	  var menu = form.widgets.dropdownButton(
	    icon,
	    { divClass:"notebook-menu btn-transparent",
	      ulClass:"pull-right",
	      client:elem,
	      actions:
	      { "Delete cell":     function() { this.notebook('delete'); },
		"Copy cell":       function() { this.notebook('copy'); },
		"Paste cell":      function() { this.notebook('paste'); },
		"Move cell up":    function() { this.notebook('up'); },
		"Move cell down":  function() { this.notebook('down'); },
		"Insert cell":     function() { this.notebook('insertBelow'); },
		"--":		   "Notebook actions",
		"Exit fullscreen": function() { this.notebook('fullscreen', false) }
	      }
	    });

	  return menu;
	}

	elem.append(toolbar = $.el.div(
            {class:"nb-toolbar"},
	    glyphButton("trash", "delete", "Delete cell", "warning"),
	    glyphButton("copy", "copy", "Copy cell", "default"),
	    glyphButton("paste", "paste", "Paste cell below", "default"),
	    sep(),
	    glyphButton("chevron-up", "up", "Move cell up", "default"),
	    glyphButton("chevron-down", "down", "Move cell down", "default"),
	    sep(),
	    glyphButton("plus", "insertBelow", "Insert cell below", "primary"),
	    glyphButton("fullscreen", "fullscreen", "Full screen", "default")
	    ));
	elem.append(notebookMenu());
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
	if ( options.value ) {
	  elem.notebook('value', options.value);
	} else if ( content.length > 0 ) {
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

	  elem.notebook('value', content.text(),
			{ fullscreen: elem.hasClass("fullscreen")
			});
	  content.remove();
	} else {
	  elem.notebook('placeHolder');
	}

	elem.notebook('setupStorage', storage);
	elem.on("data-is-clean", function(ev, clean) {
	  if ( $(ev.target).hasClass("prolog-editor") )
	  { elem.notebook('checkModified');
	    ev.stopPropagation();
	    return false;
	  }
	});
      }); /* end .each() */
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
      this.notebook('checkModified');
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
	  modal.alert("Not a SWISH notebook");
	}
      } else {
	modal.alert("Clipboard is empty");
      }
    },

    up: function(cell) {
      cell = cell||currentCell(this);
      if ( cell ) {
	cell.insertBefore(cell.prev());
	this.notebook('checkModified');
      }
      return this;
    },

    down: function(cell) {
      cell = cell||currentCell(this);
      if ( cell ) {
	cell.insertAfter(cell.next());
	this.notebook('checkModified');
      }
      return this;
    },

    insertAbove: function() {
      return this.notebook('insert', { where:"above" });
    },

    insertBelow: function() {
      if ( this.notebook('insert', {where:"below", if_visible:true}) == false ) {
	modal.alert("<p>New cell would appear outside the visible area of the " +
		    "notebook." +
		    "<p>Please select the cell below which you want the "+
		    "new cell to appear or scroll to the bottom of the " +
		    "notebook.");
      }

      return this;
    },

    run: function(cell) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.nbCell("run");
    },

    fullscreen: function(val) {
      if ( val == undefined )		/* default: toggle */
	val = !this.hasClass("fullscreen");

      if ( val )
	$("body.swish").swish('fullscreen', this);
      else
	$("body.swish").swish('exitFullscreen');

      return this;
    },

    cellType: function(cell, type) {
      cell = cell||currentCell(this);
      if ( cell )
	cell.nbCell('type', type);
    },

		 /*******************************
		 *	    CLEAN/DIRTY		*
		 *******************************/

    checkModified: function() {
      var store = this.data("storage");
      var clean = store.cleanGeneration == this.notebook('changeGen');

      this.notebook('markClean', clean);
    },

    /**
     * Called if the noteook changes from clean to dirty or visa versa.
     * This triggers `data-is-clean`, which is trapped by the tab to
     * indicate the changed state of the editor.
     */
    markClean: function(clean) {
      var data = this.data(pluginName);

      if ( data.clean_signalled != clean )
      { data.clean_signalled = clean;
	this.trigger("data-is-clean", clean);
      }

      if ( clean ) {
	this.find(".prolog-editor").prologEditor('setIsClean');
      }
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
     * @param {Bool}   [options.if_visible]  If `true`, only insert is
     * the insertion point is visible.
     */
    insert: function(options) {
      options   = options||{};
      var relto = currentCell(this);
      var cell  = options.cell || $.el.div({class:"nb-cell"});
      var view  = this.find(".nb-view")
      var viewrect;

      if ( options.if_visible ) {
	if ( view.find(".nb-cell").length > 0 )
	  viewrect = view[0].getBoundingClientRect();
      }

      if ( relto ) {
	if ( options.where == 'above' ) {
	  if ( viewrect ) {
	    var seltop = relto[0].getBoundingClientRect().top;
	    if ( seltop < viewrect.top )
	      return false;
	  }
	  $(cell).insertBefore(relto);
	} else {
	  if ( viewrect ) {
	    var selbottom = relto[0].getBoundingClientRect().bottom;

	    if ( selbottom > viewrect.bottom - 20 )
	      return false;
	  }
	  $(cell).insertAfter(relto);
	}
      } else {
	var content = this.find(".nb-content");

	if ( viewrect ) {
	  var cbottom = content[0].getBoundingClientRect().bottom;

	  if ( cbottom > viewrect.bottom - 20 )
	    return false;
	}
	content.append(cell);
      }

      if ( !options.cell ) {
	$(cell).nbCell(options.restore);
      }
      this.notebook('updatePlaceHolder');
      this.notebook('active', $(cell));
      this.notebook('checkModified');

      return this;
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
	markClean: function(clean) {
	  notebook.notebook('markClean', clean);
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
     * @param {Object} options
     * @param {Boolean} [options.skipEmpty=false] if `true`, do not save
     *		        empty cells.
     * @param {Boolean} [options.fullscren] if `true', go fullscreen.
     * Default is `true` if the toplevel `div.notebook` has a class
     * `fullscreen`.
     * @param [String] val is an HTML string that represents
     * the notebook state.
     */
    value: function(val, options) {
      options = options||{};

      if ( val == undefined ) {
	var dom = $.el.div({class:"notebook"});
	this.find(".nb-cell").each(function() {
	  cell = $(this);
	  if ( !(options.skipEmpty && cell.nbCell('isEmpty')) )
	    $(dom).append(cell.nbCell('saveDOM'));
	});

	return stringifyNotebookDOM(dom);
      } else {
	var notebook = this;
	var content  = this.find(".nb-content");
	var dom = $.el.div();

	content.html("");
	dom.innerHTML = val;		/* do not execute scripts */

	if ( options.fullscreen == undefined )
	  options.fullscreen = $(dom).find("div.notebook").hasClass("fullscreen");
	if ( options.fullscreen ) {
	  this.removeClass("fullscreen");
	  this.notebook('fullscreen', true);
	}

	$(dom).find(".nb-cell").each(function() {
	  var cell = $.el.div({class:"nb-cell"});
	  content.append(cell);
	  $(cell).nbCell($(this));
	});

	this.find(".nb-cell").nbCell('onload');
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
    var nest = [];
    return html.replace(/(<div [^>]*>|<\/div>)/g, function(t) {
      var is_cell;
      if ( t == "</div>" ) {
	is_cell = nest.pop();
	return is_cell ? "\n"+t+"\n" : t;
      } else {
	is_cell = (t.match(/(nb-cell|notebook)/) != null);
	nest.push(is_cell);
	return is_cell ? "\n"+orderAttrs(t)+"\n" : t;
      }
    }).slice(1);
  }

  tabbed.tabTypes.notebook = {
    dataType: "swinb",
    typeName: "notebook",
    label: "Notebook",
    contentType: "text/x-prolog-notebook",
    order: 200,
    create: function(dom, options) {
      $(dom).notebook(options);
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
	  var close = glyphButton("remove-circle", "close", "Close",
				  "default", "xs");
	  elem.append(close);
	  $(close).addClass("close-select");
	  $(close).on("click", function() {
	    elem.nbCell('close');
	  });

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
	  case "html":
	    if ( this.hasClass("runnable") ) {
	      this.nbCell('run');
	    }
	    break;
	}
      }
    },


    type: function(type, code) {
      console.log("nbCell type" + type + code);
      var data = this.data(pluginName);
        
        if ( data.st_type != type ) {
          if (type == "program"){
            methods.type[type].apply(this, [{codeType : code}]);
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
      var args = arguments;

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
      var args = arguments;

      this.each(function() {
	var cell = $(this);
	var data = cell.data(pluginName);

	if ( methods.onload[data.type] )
	  methods.onload[data.type].apply(cell, args);
      });

      return this.nbCell('refresh');
    },

    close: function() {
      this.find(".prolog-runner").prologRunner('close');
      return this.remove();
    },

    refresh: function() {
      if ( this.hasClass("program") ) {
	this.find("a[data-action='background']")
            .attr('title', this.hasClass("background") ?
			     "Used for all queries in this notebook" :
		             "Used for queries below this cell");

      }
      return this;
    },

    getSettings: function() {
      return {
        tabled: this.data("tabled") == "true",
	run:    this.data("run")    == "onload",
	chunk:  parseInt(this.data("chunk")||"1"),
	name:   this.attr("name")
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
	  form.fields.name(current.name||""),
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
	      var name = values.name ? values.name.trim() : "";
	      if (  name != current.name ) {
		if ( name )
		  elem.attr("name", name);
		else
		  elem.attr("name", null);
	      }
	      elem.closest(".notebook").notebook('checkModified');
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
      this.closest(".notebook").notebook('checkModified');
      this.nbCell('refresh');
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
      //console.log(this.data(pluginName))
      var type = this.data(pluginName).st_type;
      if (type == "lpad" || type == "prolog")
        return methods.saveDOM["program"].call(this);
      else return methods.saveDOM[type].call(this);
    },

    restoreDOM: function(dom) {
      var data = this.data(pluginName);
      //console.log("restoreDOM nbCell");
      function domCellType(dom) {
	for(var k in cellTypes) {
	  if ( cellTypes.hasOwnProperty(k) && dom.hasClass(k) ){
	    return k;
	  }
	}
	if (dom.hasClass("program")) {
	  if (dom.hasClass("lpad")) {
	    return "lpad";
	  }
	  return "prolog";
	}
      }

      data.st_type = domCellType(dom);
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
      //console.log(this.data(pluginName));
      var type = this.data(pluginName).st_type;
      if (type == "lpad" || type == "prolog")
        return methods.changeGen["program"].call(this);
      else
        if( type )
          return methods.changeGen[type].call(this);
	else
	  return 0;
    },

    text: function() {
      return cellText(this);
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

  methods.type.html = function(options) {	/* HTML */
    var editor;

    options = options||{};
    options.mode = "htmlmixed";

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

    function setData(name) {
      if ( options[name] != undefined ) {
	cell.data(name, ""+options[name]);
	delete options[name];
      }
    }
    function setAttr(name) {
      if ( options[name] != undefined ) {
	cell.attr(name, ""+options[name]);
	delete options[name];
      }
    }
    setData("tabled");
    setData("chunk");
    setData("run");
    setAttr("name");

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

    function wrapSolution(a)
    { this.find(".editor.query").prologEditor('wrapSolution', $(a).text());
    }

    var menu = form.widgets.dropdownButton(
      $.el.span({class:"glyphicon glyphicon-menu-hamburger"}),
      { client: cell,
	divClass: "nb-query-menu",
        actions: {
	  "Aggregate (count all)": wrapSolution,
	  "--":			   null,
	  "Order by":              wrapSolution,
	  "Distinct":              wrapSolution,
	  "Limit":		   wrapSolution,
	  "---":		   null,
	  "Download answers as CSV": function() {
	    var query  = cellText(this).replace(/\.\s*$/,"");
	    var source = this.nbCell('programs')
			     .prologEditor('getSource');
	    var options = {};
	    var name   = this.attr("name");
	    if ( name )
	      options.disposition = name;
	    prolog.downloadCSV(query, source, options);
	  }
        }
      });

    this.append(buttons,
		$.el.div({class:"query with-buttons"},
			 menu,
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
      cell.off("click", links.followLink);
    }


    function setHTML(data) {
      cell.html(data);
      cell.removeClass("runnable");
      cell.data('markdownText', markdownText);
      cell.on("dblclick", makeEditable);
      cell.on("click", "a", links.followLink);
    }

    if ( markdownText.trim() != "" )
    { $.ajax({ type: "POST",
	       url: config.http.locations.markdown,
	       data: markdownText,
	       contentType: "text/plain; charset=UTF-8",
	       success: setHTML
	     });
    } else
    { setHTML("<div class='nb-placeholder'>"+
	      "Empty markdown cell.  Double click to edit"+
	      "</div>");
    }
  };

  methods.run.html = function(htmlText, options) {
    var cell = this;

    options = options||{};
    if ( options.html == false )
    { runScripts();
      return;
    }

    htmlText = (htmlText||cellText(this)).trim();

    function makeEditable(ev) {
      var cell = $(ev.target).closest(".nb-cell");
      var text = cell.data('htmlText');
      cell.removeData('htmlText');
      methods.type.html.call(cell, {value:text});
      cell.off("dblclick", makeEditable);
      cell.off("click", links.followLink);
    }

    function runScripts() {
      if ( config.swish.notebook.eval_script == true &&
	   options.eval_script != false ) {
	var scripts = [];

	cell.find("script").each(function() {
	  var type = this.getAttribute('type')||"text/javascript";
	  if ( type == "text/javascript" )
	    scripts.push(this.textContent);
	});

	if ( scripts.length > 0 ) {
	  var script = "(function(notebook){" + scripts.join("\n") + "})";
	  var nb = new Notebook({
	    cell: cell[0]
	  });

	  try {
	    eval(script)(nb);
	  } catch(e) {
	    alert(e);
	  }
	}
      }
    }

    function runHTML(data) {
      cell[0].innerHTML = data;
      runScripts();
    }

    function setHTML(data) {
      runHTML(data);
      cell.removeClass("runnable");
      cell.data('htmlText', htmlText);
      cell.on("dblclick", makeEditable);
      cell.on("click", "a", links.followLink);
    }

    if ( htmlText != "" )
    { setHTML(htmlText);
    } else
    { setHTML("<div class='nb-placeholder'>"+
	      "Empty HTML cell.  Double click to edit"+
	      "</div>");
    }
  };

  methods.run.program = function() {		/* program */
    modal.alert("Please define a query to run this program");
  };

  /**
   * Run a query cell.
   * @param {Object} [options]
   * @param {Any}    [options.bindings] Initial bindings.  If this is a
		     string, it is simply prepended to the query.  If
		     it is an object, it is translated into a sequence
		     of Prolog unifications to bind the variables.
   */
  methods.run.query = function(options) {	/* query */
    var programs = this.nbCell('programs');
    var settings = this.nbCell('getSettings');
    var text     = cellText(this);

    //console.log("run query in notebook ");
    options = options||{};
    if ( options.bindings ) {
      var pretext = "";
      if ( typeof(options.bindings) === 'string' ) {
	pretext = options.bindings;
      } else {
	for(var k in options.bindings) {
	  if ( options.bindings.hasOwnProperty(k) ) {
	    if ( pretext )
	      pretext += ", ";
	    pretext += k + " = " + Pengine.stringify(options.bindings[k]);
	  }
	}
      }
      if ( pretext )
	text = pretext + ", (" + prolog.trimFullStop(text) + ")";
    }
    var query = { source: programs.prologEditor('getSource'),
                  query:  text,
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
		 *	       ONLOAD		*
		 *******************************/

/* These methods are executed after all cells have been initialised */

  methods.onload.query = function() {
    if ( this.data("run") == "onload" )
      this.nbCell("run");
  };

  methods.onload.html = function() {
    return methods.run.html.call(this,
				 undefined,	/* text */
				 {html:false, eval_script:true});
  };


		 /*******************************
		 *	SAVE/RESTORE DOM	*
		 *******************************/

/* ---------------- saveDOM ---------------- */

  methods.saveDOM.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return $.el.div({class:"nb-cell markdown"}, text);
  };

  methods.saveDOM.html = function() {		/* HTML */
    var text = this.data('htmlText') || cellText(this);
    var div  = $.el.div({class:"nb-cell html"});

    // assume scripts are executed when put into the DOM
    $(div).html(text);

    return div;
  };

  methods.saveDOM.program = function() {	/* program */
    var cell = this;
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

    function copyData(name) {
      var value;
      if ( (value=cell.data(name)) && !isDefault(name,value) ) {
	$(dom).attr("data-"+name, value);
      }
    }
    function copyAttr(name) {
      var value;
      if ( (value=cell.attr(name)) && value ) {
	$(dom).attr(name, value);
      }
    }

    copyData("tabled");
    copyData("chunk");
    copyData("run");
    copyAttr("name");

    return dom;
  };

/* ---------------- restoreDOM ---------------- */

  methods.restoreDOM.markdown = function(dom) {	/* markdown */
    var text = dom.text().trim();
    this.data('markdownText', text);
    methods.run.markdown.call(this, text);
  };

  methods.restoreDOM.html = function(dom) {	/* HTML */
    methods.run.html.call(this, dom.html(), {eval_script:false});
  };

  methods.restoreDOM.program = function(dom) {	/* program */
    var opts = { value:dom.text().trim() };
    if (dom.hasClass("lpad"))
        opts.codeType = "lpad";
        

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

    function getData(name) {
      var value;
      if ( (value=dom.data(name)) ) {
	if ( name == "chunk" )
	  opts.chunk = parseInt(value);
	else
	  opts[name] = value;
      }
    }
    function getAttr(name) {
      var value;
      if ( (value=dom.attr(name)) ) {
	opts[name] = value;
      }
    }

    getData("tabled");
    getData("chunk");
    getData("run");
    getAttr("name");
    if ( opts.tabled == undefined )
      opts.tabled = false;

    methods.type.query.call(this, opts);
  };

/* ---------------- changeGen ---------------- */

  methods.changeGen.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return sha1(text);
  };

  methods.changeGen.html = function() {	/* HTML */
    var text = this.data('htmlText') || cellText(this);

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

    function addData(name, key) {
      var value;

      if ( (value=cell.data(name)) )
	text += key+value;
    }
    function addAttr(name, key) {
      var value;

      if ( (value=cell.attr(name)) )
	text += key+value;
    }

    addData("tabled", "T");
    addData("chunk", "C");
    addData("run", "R");
    addAttr("name", "N");
    text += "V"+cellText(this);

    return sha1(text);
  };

/* ---------------- isEmpty ---------------- */

  methods.isEmpty.markdown = function() {	/* markdown */
    var text = this.data('markdownText') || cellText(this);

    return text.trim() == "";
  };

  methods.isEmpty.html = function() {	/* HTML */
    var text = this.data('htmlText') || cellText(this);

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
  var btn = $.el.a({href:"#",
		    class:"btn btn-"+style+" btn-"+size+" action-"+action,
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

		 /*******************************
		 *	 NOTEBOOK ClASS		*
		 *******************************/

function Notebook(options) {
  this.my_cell = options.cell;
}

/**
 * Create a Pengine from default arguments
 */
Notebook.prototype.swish = function(options) {
  var pcells = this.cell().nbCell("programs");
  var source = pcells.prologEditor('getSource');

  if ( source )
    options.src = source;

  return $.swish(options);
}

/**
 * @param {String} [name] Return (query) cell with given name.  If
 * name is omitted, return the current cell.
 * @return {jQuery} Notebook cells
 */
Notebook.prototype.cell = function(name) {
  if ( name )
    return this.notebook().find('.nb-cell[name="'+name+'"]');
  else
    return $(this.my_cell);
};

/**
 * @returns {jQuery} the notebook as a whole.
 */
Notebook.prototype.notebook = function() {
  return $(this.my_cell).closest(".notebook");
};

/**
 * Run named query cell with bindings
 * @param {String} cell Name of the cell to run
 * @param {Object|String} [bindings] Bindings to pass to the query.
 */
Notebook.prototype.run = function(cell, bindings) {
  var options = {};
  if ( bindings )
    options.bindings = bindings;

  this.cell(cell).nbCell('run', options);
};

/**
 * Submit a form, calling a predicate
 * @param {String} formsel is the selector to find the form in the
 * notebook cell.
 * @param {Object} options
 * @param {String} options.predicate predicate to call.  The predicate
 * receives one argument, containing the form data as a dict.
 * @param {Function} options.onsuccess is the function run on successful
 * completion
 */
Notebook.prototype.submit = function(formsel, options) {
  var formel = this.$(formsel);
  var data   = form.serializeAsObject(formel);

  form.formError(formel, null);
  this.swish({
    ask: options.predicate+"(("+Pengine.stringify(data)+"))",
    onerror: function(data) { form.formError(formel, data); },
    onsuccess: options.onsuccess
  });
};

Notebook.prototype.$ = function(selector) {
  return this.cell().find(selector);
}
});
