/**
 * @fileOverview
 * Defines the interaction with the `File` menu and gitty storage
 * module of the server.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "modal", "form", "gitty", "history", "tabbed",

	 "laconic", "diff"
       ],
       function($, config, modal, form, gitty, history, tabbed) {

(function($) {
  var pluginName = 'storage';

  var defaults = {
    typeName: "program"
  }

  /** @lends $.fn.storage */
  var methods = {
    /**
     * @param {Object} options
     * @param {Function} options.setValue sets the new document value
     * @param {Function} options.getValue gets the current document value
     * @param {Function} options.changeGen identifies the current state
     * @param {String}   options.cleanData identifies the clean state
     * @param {String}	 options.cleanCheckpoint is the action that
     * caused the clean state
     * @param {Any}	 options.cleanGeneration identifies the clean
     * state.
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = $.extend({}, defaults, options);

	/**
	 * Execute a method on the storage plugin. This particularly
	 * avoids handling events that have bubbled up from children
	 * that have a storage plugin attached, which may happen in
	 * notebooks.
	 */
	function onStorage(ev, method) {
	  var target = $(ev.target);

	  if ( target.hasClass("storage") && target.is(":visible") ) {
	    var rc = target.storage.apply(
		       target,
		       Array.prototype.slice.call(arguments, 1));
	    if ( rc == "propagate" )
	      return;
	  }
	  ev.stopPropagation();
	}

	elem.addClass("storage");
	if ( options.title||options.file )
	  elem.tabbed('title',
		      options.title||filebase(options.file),
		      options.file ? options.file.split('.').pop() : "pl");

	elem.on("source", function(ev, src) {
	  onStorage(ev, 'setSource', src);
	});
	elem.on("save", function(ev, data) {
	  onStorage(ev, 'save', data);
	});
	elem.on("fileInfo", function(ev) {
	  onStorage(ev, 'info');
	});
	elem.on("diff", function(ev) {
	  onStorage(ev, 'diff');
	});
	elem.on("revert", function(ev) {
	  onStorage(ev, 'revert');
	});
	elem.on("activate-tab", function(ev) {
						/* TBD: What exactly? */
	});

	$(window).bind("beforeunload", function(ev) {
	  return elem.storage('unload', "beforeunload", ev);
	});

	elem.data(pluginName, data);	/* store with element */
      });
    },

    /**
     * @param {String|Object} src becomes the new contents of the editor
     * @param {String} src.data contains the data in the case that
     * `src` is an object.
     * @return {Object|String} The string `"propagate"` is
     * returned if the provided src does not match the supported type.
     */
    setSource: function(src) {
      var data = this.data(pluginName);
      var type = tabbed.tabTypes[data.typeName];

      if ( typeof(src) == "string" )
	src = {data:src};

      if ( src.newTab )
	return "propagate";

      if ( (src.meta && src.meta.name) || src.url )
      { var name = (src.meta && src.meta.name) ? src.meta.name : src.url;
	var ext  = name.split('.').pop();

	if ( ext != type.dataType )
	  return "propagate";
      }

      if ( this.storage('unload', "setSource") == false )
	return false;

      if ( src.meta ) {
	data.file = src.meta.name;
	data.meta = src.meta;
	data.url  = null;
      } else {
	data.file = null;
	data.meta = null;
	if ( src.url )
	  data.url = src.url;
      }

      data.setValue(src);
      data.cleanGeneration = data.changeGen();
      data.cleanData       = src.data;
      data.cleanCheckpoint = src.cleanCheckpoint || "load";

      function basename(path) {
	return path ? path.split('/').pop() : null;
      }
      var title = (filebase(data.file) ||
		   filebase(basename(src.url)) ||
		   type.label);

      if ( !src.url )
	src.url = config.http.locations.swish;

      this.tabbed('title', title, type.dataType);
      if ( !src.noHistory )
	history.push(src);

      return this;
    },

    /**
     * Load document from the server.
     */
    load: function(file) {
      if ( file ) {
	var that = this;
	var options = this.data(pluginName);

	$.ajax({ url: config.http.locations.web_storage + "/" + file,
		 dataType: "text",
		 success: function(data) {
		   that.storage('setSource',
				{ data: data,
				  meta: { name:file
				        }
				});
		 },
		 error: function(jqXHDR) {
		   modal.ajaxError(jqXHR);
		 }
	       });
      }
      return this;
    },

    /**
     * Revert to upstream version
     */
    revert: function() {
      var data = this.data(pluginName);

      data.setValue(data.cleanData);
      return this;
    },

    /**
     * Save the current document to the server.  Depending on the
     * arguments, this function implements several forms of saving:
     *
     *   - Without arguments arguments, it implements "Save".
     *   - With ("as"), it implements "Save as", which opens a
     *     dialog which calls this method again, but now with
     *     meta-data in the first argument.
     *   - With ({...}) it performs the save operation of "Save as"
     *   - With ({...}, "only-meta-data") it only updates the meta
     *     data on the server.
     *
     * @param {Object} [meta] provides additional meta-information.
     * Currently defined fields are `author`, `email`,
     * `title`, `keywords` and `description`. Illegal fields are ignored
     * by the server.
     * @param {String} [what] If `"only-meta-data"`, only the meta-data
     * is updated.
     */
    save: function(meta, what) {
      var options = this.data(pluginName);
      var type    = tabbed.tabTypes[options.typeName];
      var url     = config.http.locations.web_storage;
      var method  = "POST";
      var elem    = this;
      var data;

      if ( options.url )
	return this.storage('saveURL');

      if ( meta == "as" ) {
	this.storage('saveAs');
	return this;
      }

      if ( options.file &&
	   (!meta || !meta.name || meta.name == options.file) ) {
	url += "/" + encodeURI(options.file);
	method = "PUT";
      }

      if ( what == "only-meta-data" ) {
	meta = gitty.reduceMeta(meta, options.meta)
	if ( $.isEmptyObject(meta) ) {
	  alert("No change");
	  return;
	}
	data = { update: "meta-data" };
      } else if ( method == "POST" ) {
	data = { data: options.getValue(),
		 type: type.dataType
	       };
	if ( options.meta ) {			/* rename */
	  data.previous = options.meta.commit;
	}
      } else {
	if ( !options.isClean(options.cleanGeneration) ) {
	  data = { data: options.getValue(),
		   type: type.dataType
		 };
	} else if ( gitty.diffTags(options.meta.tags, meta.tags) == null ) {
	  alert("No change");
	  return;
	}
      }

      if ( meta )
	data.meta = meta;

      $.ajax({ url: url,
               dataType: "json",
	       contentType: "application/json",
	       type: method,
	       data: JSON.stringify(data),
	       success: function(reply) {
		 if ( reply.error ) {
		   alert(JSON.stringify(reply));
		 } else {
		   options.file = reply.file;
		   options.meta = reply.meta;
		   options.cleanGeneration = options.changeGen();
		   options.cleanData       = options.getValue();
		   options.cleanCheckpoint = "save";
		   modal.feedback({ html: "Saved",
				    owner: elem
		                  });

		   elem.tabbed('title', meta.name);
		   history.push(reply);
		 }
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
	     });

      return this;
    },

    /**
     * Provide a Save As dialog
     */
    saveAs: function() {
      var options = this.data(pluginName);
      var meta    = options.meta||{};
      var editor  = this;
      var update  = Boolean(options.file);
      var fork    = options.meta && meta.symbolic != "HEAD";
      var type    = tabbed.tabTypes[options.typeName];

      if ( meta.public === undefined )
	meta.public = true;

      function saveAsBody() {
	this.append($.el.form({class:"form-horizontal"},
			      form.fields.fileName(fork ? null: options.file,
						   meta.public, meta.example),
			      form.fields.title(meta.title),
			      form.fields.author(meta.author),
			      update ? form.fields.commit_message() : undefined,
			      form.fields.tags(meta.tags),
			      form.fields.buttons(
				{ label: fork   ? "Fork "+type.label :
					 update ? "Update "+type.label :
						  "Save "+type.label,
				  action: function(ev,data) {
				            editor.storage('save', data);
					    return false;
				          }
				})));
      }

      form.showDialog({ title: fork   ? "Fork from "+meta.commit.substring(0,7) :
			       update ? "Save new version" :
			                "Save "+type.label+" as",
			body:  saveAsBody
		      });

      return this;
    },

    /**
     * Save data to the URL it was loaded from.
     * FIXME: feedback, allow recompilation (if Prolog source)
     */
    saveURL: function() {
      var options = this.data(pluginName);
      var data = options.getValue();
      var type = tabbed.type(options.url)||{};
      var elem = this;

      if ( options.isClean(options.cleanGeneration) ) {
	alert("No change");
	return this;
      }

      $.ajax({ url: options.url,
               dataType: "json",
	       contentType: type.contentType||"text/plain",
	       type: "PUT",
	       data: data,
	       success: function(reply) {
		 if ( reply.error ) {
		   alert(JSON.stringify(reply));
		 } else {
		   options.cleanGeneration = options.changeGen();
		   options.cleanData       = options.getValue();
		   options.cleanCheckpoint = "save";
		   modal.feedback({ html: "Saved",
				    owner: elem
		                  });
		 }
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
	     });

      return this;
    },

    /**
     * @param {Object} [options]
     * @param {String|Boolean} [options.data=false] if `true`, always
     * include the content of the storage.  If `"if_modified"`, only
     * include the content if it is modified.
     * @param {String} [options.type] if provided, only return objects
     * associated with files with the given extension.
     * @return {Array.Object}
     */
    getData: function(options) {
      var result = [];

      options = options||{};

      this.each(function() {
	var data = $(this).data(pluginName);
	var obj = {};

	if ( data.meta ) {
	  obj.type = "store";
	  obj.name = data.meta.name;
	} else if ( data.url ) {
	  obj.type = "url";
	  obj.url  = data.url;
	  obj.name = data.url.split("/").pop();
	} else
	{ obj.type = "local";
	}

	if ( $(this).closest(".tab-pane.active").length == 1 )
	  obj.active = true;

	if ( !options.type ||
	     ( options.name &&
	       options.name.split(".").pop() == options.type ) ) {
	  if ( options.data ) {
	    var value = data.getValue();

	    obj.modified = (value != data.cleanData);
	    if ( options.data == true ||
		 (obj.modified && options.data == "if_modified") )
	      obj.data = value;
	  }

	  result.push(obj);
        }
      });

      return result;
    },

    /**
     * @return {jQuery|undefined} the jQuery storage element that
     * matches `to`
     */
    match: function(to) {
      for(var k=0; k<this.length; k++) {
	me = $(this[k]);
	var data = me.data(pluginName);

	if ( to.file && to.file == data.file )
	  return me;
	if ( to.url && to.url == data.url )
	  return me;
      }
    },

    /**
     * Expose associateted tab
     * @return {jQuery|undefined} storage plugin if tab could be
     * exposed.
     */
    expose: function(reason) {
      var tab = this.closest(".tab-pane");
      if ( tab.length == 1 ) {
	var tabbed = tab.closest(".tabbed");
	tabbed.tabbed('show', tab.attr('id'));
	if ( reason )
	  modal.feedback({ html: reason,
	                   owner: this
	                 });

	return this;
      }
    },

    /**
     * Provide information about the current source in a modal
     * dialog.
     */
    info: function() {
      var options = this.data(pluginName);
      var meta = options.meta;
      var editor = this;
      var title;

      if ( options.meta ) {
	title = $().gitty('title', options.meta);
      } else {
	title = "Local source";
      }

      function infoBody() {
	if ( options.meta ) {
	  options.editor = editor;		/* circular reference */
	  this.gitty(options);
	} else {
	  this.append($.el.p("The source is not associated with a file. ",
			     "Use ",
			     $.el.b("Save ..."),
			     " to save the source with meta information."
			    ));
	}
      }

      form.showDialog({ title: title,
			body:  infoBody
		      });

      return this;
    },

    /**
     * Generate diff relative to last checkpoint.
     */
    diff: function() {
      var data = this.data(pluginName);
      var baseName = { load: "Loaded text",
		       new: "New text",
		       save: "Saved text"
		     };

      function infoBody() {
	var diff = $.el.div();
	var current = data.getValue();

	this.append(diff);

	if ( current == data.cleanData ) {
	  $(diff).append($.el.p("No changes"));
	} else {
	  $(diff).diff({ base: data.cleanData,
			 head: current,
			 baseName: baseName[data.cleanCheckpoint]
		       });
	  this.parents("div.modal-dialog").addClass("modal-wide");
	}
      }

      form.showDialog({ title: "Changes since " + baseName[data.cleanCheckpoint],
			body:  infoBody
		      });

      return this;
    },

    /**
     * Called if the editor is destroyed to see whether it has pending
     * modifications.
     *
     * @param {String} why is one of `"beforeunload" if the window is
     * left or "setSource" if the source will be replaced.
     */
    unload: function(why, ev) {
      var data = this.data(pluginName);

      if ( !data )				/* how can this happen? */
	return undefined;

      if ( data.meta ) {
	history.addRecent({ type: "gitty",
			    id:	  data.meta.name	/* FIXME: add hash? */
			  });
      }

      if ( data.cleanData != data.getValue() ) {
	if ( why == "beforeunload" ) {
	  var message = "The source editor has unsaved changes.\n"+
	                "These will be lost if you leave the page";

	  ev = ev||window.event;
	  if ( ev )
	    ev.returnValue = message;

	  return message;
	} else {
	  var message = "The source editor has unsaved changes.\n"+
	                "These will be lost"+
			( why == "setSource" ? " if you load a new program" :
			  why == "closetab"  ? " close this tab" : ""
			);

	  return confirm(message);
	}
      }

      return undefined;
    }
  }; // methods

  function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
  }

  /**
   * <Class description>
   *
   * @class storage
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.storage = function(method) {
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

  function filebase(file) {
    return file ? file.split('.').slice(0,-1).join(".") : null;
  }
});
