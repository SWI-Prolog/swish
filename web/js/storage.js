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
    typeName: "program",
    markClean: function(clean) {}
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
	if ( options.title||options.file||options.url ) {
	  var file = options.file;
	  if ( !file && options.url )
	    file = options.url.split("/").pop();
	  elem.tabbed('title',
		      options.title||filebase(file),
		      file ? file.split('.').pop() : "pl");
	}

	elem.on("source", function(ev, src) {
	  onStorage(ev, 'setSource', src);
	});
	elem.on("save", function(ev, data) {
	  onStorage(ev, 'save', data);
	});
	elem.on("download", function(ev) {
	  onStorage(ev, 'download');
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
      }
      data.url     = src.url     || undefined;
      data.st_type = src.st_type || undefined;

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

	$.ajax({ url: config.http.locations.web_storage + file,
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
      data.cleanGeneration = data.changeGen();
      data.markClean(true);
      return this;
    },

    /**
     * Reload from server
     */
    reload: function() {
      var elem = this;
      var data = elem.data(pluginName);
      var url  = config.http.locations.web_storage +
		 encodeURI(data.file);

      $.ajax({ url: url,
	       type: "GET",
	       data: { format: "json" },
	       success: function(reply) {
		 reply.url = url;
		 reply.st_type = "gitty";
		 reply.noHistory = true;
		 elem.storage('setSource', reply);
	       },
	       error: function(jqXHR) {
		 modal.ajaxError(jqXHR);
	       }
	     });

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
      var data   = this.data(pluginName);
      var type   = tabbed.tabTypes[data.typeName];
      var url    = config.http.locations.web_storage;
      var method = "POST";
      var elem   = this;
      var post;

      if ( (data.st_type == "filesys" || data.st_type == "external") && data.url )
	return this.storage('saveURL');

      if ( meta == "as" ) {
	this.storage('saveAs');
	return this;
      }

      if ( data.file &&
	   (!meta || !meta.name || meta.name == data.file) ) {
	url += encodeURI(data.file);
	method = "PUT";
      }

      if ( what == "only-meta-data" ) {
	meta = gitty.reduceMeta(meta, data.meta)
	if ( $.isEmptyObject(meta) ) {
	  alert("No change");
	  return;
	}
	post = { update: "meta-data" };
      } else if ( method == "POST" ) {
	post = { data: data.getValue(),
		 type: type.dataType
	       };
      } else {
	if ( !data.isClean(data.cleanGeneration) ) {
	  post = { data: data.getValue(),
		   type: type.dataType
		 };
	} else if ( gitty.diffTags(data.meta.tags, meta.tags) == null ) {
	  alert("No change");
	  return;
	}
      }

      if ( meta )
	post.meta = meta;
      if ( data.meta )
	post.previous = data.meta.commit;

      $.ajax({ url: url,
               dataType: "json",
	       contentType: "application/json",
	       type: method,
	       data: JSON.stringify(post),
	       success: function(reply) {
		 if ( reply.error ) {
		   modal.alert(errorString("Could not save", reply));
		 } else {
		   if ( data.meta &&
			data.meta.example != reply.meta.example ) {
		     elem.closest(".swish").trigger('examples-changed');
		   }
		   data.file = reply.file;
		   data.meta = reply.meta;
		   data.st_type = "gitty";
		   data.cleanGeneration = data.changeGen();
		   data.cleanData       = data.getValue();
		   data.cleanCheckpoint = "save";
		   data.markClean(true);
		   modal.feedback({ html: "Saved",
				    owner: elem
		                  });

		   elem.tabbed('title', data.meta.name);
		   history.push(reply);
		 }
	       },
	       error: function(jqXHR, textStatus, errorThrown) {
		 if ( jqXHR.status == 409 ) {
		   elem.storage('resolveEditConflict',
				JSON.parse(jqXHR.responseText));
		 } else {
		   alert('Save failed; click "ok" to try again');
		   elem.storage('saveAs');
		 }
	       }
	     });

      return this;
    },

    /**
     * Provide a Save As dialog
     */
    saveAs: function(options) {
      var data = this.data(pluginName);
      var meta    = data.meta||{};
      var editor  = this;
      var update  = Boolean(data.file);
      var fork    = data.meta && meta.symbolic != "HEAD";
      var type    = tabbed.tabTypes[data.typeName];
      var author  = config.swish.user ?
        ( config.swish.user.realname && config.swish.user.email ?
	    config.swish.user.realname + " <" + config.swish.user.email + ">" :
	    config.swish.user.user
        ) :
	meta.author;

      if ( meta.public === undefined )
	meta.public = true;

      options = options||{};

      function saveAsBody() {
	this.append($.el.form({class:"form-horizontal"},
			      form.fields.fileName(fork ? null: data.file,
						   meta.public, meta.example),
			      form.fields.title(meta.title),
			      form.fields.author(author),
			      update ? form.fields.commit_message() : undefined,
			      form.fields.tags(meta.tags),
			      form.fields.buttons(
				{ label: fork   ? "Fork "+type.label :
					 update ? "Update "+type.label :
						  "Save "+type.label,
				  action: function(ev, as) {
				            editor.storage('save', as);
					    return false;
				          }
				})));
      }

      form.showDialog({ title: options.title ? options.title :
			       fork   ? "Fork from "+meta.commit.substring(0,7) :
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
		   modal.alert(errorString("Could not save", reply));
		 } else {
		   options.cleanGeneration = options.changeGen();
		   options.cleanData       = options.getValue();
		   options.cleanCheckpoint = "save";
		   options.markClean(true);
		   modal.feedback({ html: "Saved",
				    owner: elem
		                  });
		 }
	       },
	       error: function(jqXHR) {
		 if ( jqXHR.status == 403 ) {
		   var url = options.url;
		   delete(options.meta);
		   delete(options.st_type);
		   delete(options.url);
		   elem.storage('saveAs', {
		     title: "<div class='warning'>Could not save to "+url+
			    "</div> Save a copy as"
		   });
		 } else
		 { modal.ajaxError(jqXHR);
		 }
	       }
	     });

      return this;
    },

		 /*******************************
		 *	    EDIT CONFLICTS	*
		 *******************************/

    resolveEditConflict: function(options) {
      var bdiv;

      options.storage = this;

      function body() {
	var elem = $(this);

	elem.addClass("edit-conflict");

	function tabLabel(label, active, id, disabled) {
	  var attrs = {role:"presentation"};
	  var classes = [];
	  if ( active   ) classes.push("active");
	  if ( disabled ) classes.push("disabled");
	  if ( classes != [] )
	    attrs.class = classes.join(" ");
	  var elem =
	  $.el.li(attrs, $.el.a({href:"#"+id, 'data-toggle':"tab"}, label));
	  return elem;
	}

	tabs = $($.el.div({class:"tab-content"}));
	elem.append($.el.ul(
	  {class:"nav nav-tabs"},
	  tabLabel("My edits",    true,  "merge-my-edits"),
	  tabLabel("Their edits", false, "merge-server-edits"),
	  tabLabel("Conflicts",   false, "merge-conflicts")));
	elem.append(tabs);

	function tabContent(id, cls) {
	  tabs.append($.el.div({class:"tab-pane fade "+id+" "+cls, id:id}));
	  elem.find('[href="#'+id+'"]').on("show.bs.tab", function(ev) {
	    elem.storage(id);
	  });
	}

	tabContent("merge-my-edits",    "in active");
	tabContent("merge-server-edits", "");
	tabContent("merge-conflicts",   "");

	elem.data("edit-conflict", options);

	elem.storage('merge-my-edits');

	elem.append(bdiv =
	  $.el.div({class:"form-group"},
		   $.el.button({name:"merge",
				class:"btn btn-primary"},
			       "Merge"),
		   $.el.button({name:"discard-my-edits",
				class:"btn btn-primary"},
			       "Discard my changes"),
		   $.el.button({name:"discard-server-edits",
				class:"btn btn-primary"},
			       "Discard changes on server"),
		   $.el.button({name:"cancel",
				class:"btn btn-danger",
				'data-dismiss':"modal"},
			       "Cancel")));


	$(bdiv).on("click", "button", function(ev) {
	  elem.storage('editConflictAction', $(ev.target).attr("name"));
	  $(ev.target).parents(".modal").modal('hide');
	  ev.preventDefault();
	  return false;
	});
      }

      form.showDialog({ title: "Edit conflict",
			body: body
		      });

      return this;
    },

    'merge-my-edits': function() {
      var data = $(this).data("edit-conflict");
      $(this).find(".merge-my-edits")
        .empty()
        .append(udiff(data.edit.me.data));
    },

    'merge-server-edits': function() {
      var data = $(this).data("edit-conflict");
      $(this).find(".merge-server-edits")
        .empty()
        .append(udiff(data.edit.server.data));
    },

    'merge-conflicts': function() {
      var data = $(this).data("edit-conflict");
      var tab  = $(this).find(".merge-conflicts");

      tab.empty();
      if ( data.patch_status != 0 ) {
	tab.append(editConflicts(data.merged));
      } else {
	tab.html("No merge conflicts");
      }
    },

    editConflictAction: function(action) {
      var options = $(this).data("edit-conflict");
      var data = $(options.storage).data(pluginName);

      if ( action == "merge" ) {
	data.setValue(options.merged);
	data.meta.commit = options.edit.server.to.commit;
      } else if ( action == "discard-my-edits" ) {
	$(options.storage).storage('reload');
      } else if ( action == "discard-server-edits" ) {
	var data = $(options.storage).data(pluginName);
	data.meta.commit = options.edit.server.to.commit;
      }
    },


		 /*******************************
		 *	   DOWNLOADING		*
		 *******************************/

    download: function() {
      var options = this.data(pluginName);
      var type    = tabbed.tabTypes[options.typeName];
      var data    = options.getValue();
      var href    = "data:text/plain;charset=UTF-8,"
	          + encodeURIComponent(data);

      var a = $.el.a({ href:href,
		       download:options.file||("swish."+type.dataType)
		     });
      this.append(a);
      a.click();
      $(a).remove();

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

	obj.type = data.type;
	if ( data.url ) obj.url = data.url;
	if ( data.meta ) {
	  function copyMeta(name) {
	    if ( data.meta[name] )
	      obj[name] = data.meta[name];
	  }

	  copyMeta("name");
	  copyMeta("path");
	  copyMeta("modified");
	  copyMeta("loaded");
	  copyMeta("modified_since_loaded");
	  copyMeta("module");
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
      var data = this.data(pluginName);
      var meta = data.meta||{};
      var editor = this;
      var title;

      if ( data.st_type == "gitty" ) {
	title = $().gitty('title', meta);
      } else if ( data.st_type == "filesys" ) {
	title = "File system -- " + basename(meta.path);
      } else if ( data.st_type == "external" ) {
	title = "External -- " + data.url;
      } else {
	title = "Scratch source";
      }

      function infoBody() {
	if ( data.st_type == "gitty" ) {
	  data.editor = editor;		/* circular reference */
	  this.gitty(data);
	} else if ( data.st_type == "filesys" ) {
	  filesysInfo(this, meta);
	} else if ( !data.st_type ) {
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
	  var rb;
	  var buttons = $.el.div({ class:"btn-group diff",
			           role:"group"
				 },
				 $.el.button({ name:"close",
					       'data-dismiss':"modal",
				               class:"btn btn-primary"
					     },
					     "Close"),
				 rb=
				 $.el.button({ name:"revert",
				               class:"btn btn-danger",
					       'data-dismiss':"modal"
					     },
					     "Revert changes"));
	  $(diff).diff({ base: data.cleanData,
			 head: current,
			 baseName: baseName[data.cleanCheckpoint]
		       });
	  this.append($.el.div({class: "wrapper text-center"}, buttons));
	  $(rb).on("click", function(ev) {
	    $(".swish-event-receiver").trigger("revert");
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
	history.addRecent({ st_type: "gitty",
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

  function filesysInfo(form, meta) {
    var table = $.el.table({class:"table table-striped"});

    $(table).append($.el.tr($.el.th("Path"),
			    $.el.td(meta.path)));
    $(table).append($.el.tr($.el.th("Modified"),
			    $.el.td(new Date(meta.last_modified*1000)
				    .toLocaleString())));
    $(table).append($.el.tr($.el.th("Loaded"),
			    $.el.td(meta.modified_since_loaded ? "yes (modified)":
				    meta.loaded ? "yes" : "no")));

    form.append(table);
  }

  function errorString(action, error) {
    if ( error.error == "file_exists" ) {
      return action + ": file exists: " + error.file;
    }

    return JSON.stringify(error);
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

  function basename(path) {
    return path ? path.split('/').pop() : null;
  }

  function udiff(diff) {
    if ( diff ) {
      var lines = diff.split("\n");
      var pre = $($.el.pre({class:"udiff"}));

      for(var i=0; i<lines.length; i++) {
	var line = lines[i];
	var classmap = { '@': 'udiff-hdr',
			 ' ': 'udiff-ctx',
			 '+': 'udiff-add',
			 '-': 'udiff-del'
		       };
	pre.append($.el.span({class:classmap[line.charAt(0)]}, line),
		   $.el.br());
      }
      return pre;
    } else {
      return $($.el.div({class:"udiff"}, "No changes"));
    }
  }

  function editConflicts(merged) {
    var lines = merged.split("\n");
    var pre = $($.el.pre({class:"udiff"}));
    var cls = null;

    function addLine(line, cls) {
      pre.append($.el.span({class:cls}, line),
		 $.el.br());
    }

    for(var i=0; i<lines.length; i++) {
      var line = lines[i];

      if ( line == "<<<<<<<" ) {
	addLine(line, "edit-conflict-sep");
	cls = "edit-conflict-me";
      } else if ( cls == "edit-conflict-me" && line == "=======" ) {
	addLine(line, "edit-conflict-sep");
	cls = "edit-conflict-them";
      } else if ( cls == "edit-conflict-them" && line == ">>>>>>>" ) {
	addLine(line, "edit-conflict-sep");
	cls = null;
      } else if ( cls ) {
	addLine(line, cls);
     }
   }

   return pre;
  }
});
