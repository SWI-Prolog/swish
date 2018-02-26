/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2018, VU University Amsterdam
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

define([ "jquery", "config", "modal", "form", "gitty",
	 "history", "tabbed", "utils",
	 "laconic", "diff"
       ],
       function($, config, modal, form, gitty, history, tabbed, utils) {

(function($) {
  var pluginName = 'storage';

  var defaults = {
    typeName: "program",
    is_clean: true,
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

	elem.data(pluginName, data);	/* store with element */
	elem.addClass("storage unloadable");
	elem.storage('update_tab_title');

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
	elem.on("reload", function(ev) {
	  onStorage(ev, 'reload');
	});
	elem.on("chat-about-file", function(ev) {
	  onStorage(ev, 'chat');
	});
	elem.on("follow-file", function(ev) {
	  onStorage(ev, 'follow');
	});
	elem.on("activate-tab", function(ev) {
						/* TBD: What exactly? */
	});
	elem.on("data-is-clean", function(ev, clean) {
	  elem.storage('markClean', clean);
	});
	elem.on("fullscreen", function(ev, val) {
	  if ( !val )
	    elem.storage('update_tab_title');
	});
	elem.on("unload", function(ev, rc) {
	  rc.rc = elem.storage('unload', "beforeunload", ev);
	});

	elem.storage('chat', (data.meta||{}).chat||'update');
      });
    },

    /**
     * @returns {Boolean} `true` if the storage can represent the
     * requested type
     */
    supportsType: function(src) {
      var data = this.data(pluginName);
      var type = tabbed.tabTypes[data.typeName];

      if ( typeof(src) == "string" )
	src = {data:src};

      if ( (src.meta && src.meta.name) || src.url )
      { var name = (src.meta && src.meta.name) ? src.meta.name : src.url;

	if ( tabbed.type(name)["typeName"] != type.typeName )
	  return false;
      }

      return true;
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

      if ( typeof(src) == "string" )
	src = {data:src};

      if ( !this.storage('supportsType', src) )
	return undefined;

      if ( this.storage('unload', "setSource") == false )
	return false;

      if ( src.meta ) {
	data.file = src.meta.name;
	data.meta = src.meta;
	data.url  = null;
	if ( src.meta.symbolic == "HEAD" )
	  src.url = config.http.locations.web_storage + src.meta.name;
      } else {
	data.file = null;
	data.meta = null;
      }
      data.url     = src.url;
      data.st_type = src.st_type;
      data.chats   = src.chats;

      data.setValue(src);
      data.cleanGeneration = data.changeGen();
      data.cleanData       = data.getValue();
      data.cleanCheckpoint = src.cleanCheckpoint || "load";
      data.markClean(true);

      this.storage('update_tab_title');

      if ( !src.url       ) src.url = config.http.locations.swish;
      if ( !src.noHistory ) history.push({ url: src.url,
					   reason: 'load'
					 });

      this.storage('chat', src.chat||(src.meta||{}).chat||'update');
      $(".storage").storage('chat_status', true);

      return this;
    },

    is_clean: function() {
      var data = this.data(pluginName);
      return data.isClean(data.cleanGeneration);
    },

    /**
     * Set the value, but do not update the clean generation, meta-
     * data, etc.  This is used for restoring a modified state.
     * See tabbed.setState().
     */
    setValue: function(value) {
      var data = this.data(pluginName);

      data.setValue(value);
      this.trigger("data-is-clean", data.isClean(data.cleanGeneration));

      return this;
    },

    /**
     * Update the label and icon shown in the tab
     */
    update_tab_title: function(action) {
      return this.each(function() {
	var elem  = $(this);
	var docid = elem.storage('docid');

	if ( action == 'chats++' ) {
	  elem.tabbed('chats++', docid);
	} else {
	  var data = elem.data(pluginName);
	  var file = data.file||data.url;
	  var type;

	  if ( !file || !(type = tabbed.type(file)) )
	    type = tabbed.tabTypes[data.typeName];

	  var title = (filebase(utils.basename(file)) ||
		       type.label);

	  if ( docid && data.chats )
	    data.chats.docid = docid;

	  elem.tabbed('title', title, type.dataType);
	  elem.tabbed('chats', data.chats);
	}
      });
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
     * Reload from server.
     * @param {String} file Name of the file to reload.  Default is to
     * reload the current `data.file`.
     */
    reload: function(file) {
      var elem = this;
      var data = elem.data(pluginName);
          file = file||data.file;
      var url  = config.http.locations.web_storage +
		 encodeURI(file);

      $.ajax({ url: url,
	       type: "GET",
	       data: { format: "json" },
	       success: function(reply) {
		 reply.url = url;
		 reply.st_type = "gitty";
		 reply.noHistory = true;
		 elem.storage('setSource', reply);
		 $("#chat").trigger('send',
				    { type:'reloaded',
				      file:file,
				      commit:reply.meta.commit
				    });
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
	   ( what == "only-meta-data" ||
	     ( !(meta && meta.default) &&
	       (!meta || meta.name == data.file)
	     )
	   ) ) {
	url += encodeURI(data.file);
	method = "PUT";
      }

      if ( what == "only-meta-data" ) {
	if ( $.isEmptyObject(gitty.reduceMeta(meta, data.meta)) ) {
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

		   if ( method == "POST" )
		     data.chats = {		/* forked file has no chats */
		       docid: elem.storage('docid'),
		       total: 0
		     };
		   elem.storage('update_tab_title');
		   elem.storage('chat', (data.meta||{}).chat||'update');
		   $(".storage").storage('chat_status', true);
		   history.push({url: reply.url, reason: "save"});
		 }
	       },
	       error: function(jqXHR, textStatus, errorThrown) {
		 if ( jqXHR.status == 409 ) {
		   elem.storage('resolveEditConflict',
				JSON.parse(jqXHR.responseText));
		 } else if ( jqXHR.status == 403 ) {
		   modal.alert("Permission denied.  Please try a different name");
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
      var fork    = data.meta && meta.symbolic != "HEAD" && !meta.default;
      var type    = tabbed.tabTypes[data.typeName];
      var profile = $("#login").login('get_profile',
				      [ "display_name", "avatar", "email",
					"identity"
				      ]);
      var author  = profile.display_name;
      var modify  = meta.modify;
      var canmodify;

      if ( meta.public === undefined )
	meta.public = true;

      if ( profile.identity ) {
	if ( !modify )
	  modify = ["login", "owner"];
      } else
      { modify = ["any", "login", "owner"];
      }

      canmodify = ( profile.identity == meta.identity ||
		    (profile.identity && !(meta.identity||meta.user)) );

      options = options||{};

      function saveAsBody() {
	this.append($.el.form(
          { class:"form-horizontal"},
	    form.fields.hidden("identity", profile.identity),
	    form.fields.hidden("default", meta.default),
	    form.fields.hidden("chat", meta.chat),
	    profile.identity ? undefined :
			       form.fields.hidden("avatar", profile.avatar),
	    form.fields.fileName(fork ? null: data.file,
				 meta.public, meta.example),
	    form.fields.title(meta.title),
	    form.fields.author(author, profile.identity),
	    update ? form.fields.commit_message() : undefined,
	    form.fields.tags(meta.tags),
	    form.fields.modify(modify, canmodify),
	    form.fields.follow(profile.email),
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

    /**
     * Storage was activated (e.g., a tab switch)
     */
    activate: function() {
      var data = this.data(pluginName);

      if ( data && data.url ) {
	history.push({url: data.url, reason: 'activate'});
      }

      return this
    },

    /**
     * @return {Object} state of a set of storage objects, typically
     * called from a tabbed environment to save the state of all tabs.
     */
    getState: function(always) {
      var state = {
        tabs: []
      };

      this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName);
	var meta = elem.meta || {};
	var h;

					/* avoid incomplete elements */
	if ( (data.file || data.url) && data.isClean && data.cleanGeneration ) {
	  if ( !meta.name && data.file )
	    meta.name = data.file;

	  var tab = {
	    file:    meta.name,
	    st_type: data.st_type,
	    url:     data.url
	  };
	  if ( elem[pluginName]('getActive') )
	    tab.active = true;
	  if ( (h=elem[pluginName]('chatroom_size')) )
	    tab.chatroom = h;

	  state.tabs.push(tab);

	  if ( always ||
	       !data.isClean(data.cleanGeneration) ) {
	    tab.meta = meta;
	    tab.data = data.getValue();
	  }
	}
      });

      return state;
    },

    /**
     * Restore a storage object from local (when modified) or remote
     * version.
     *
     * @param {String} name is the name of the document to retrieve.
     */
    restoreLocal: function(name) {
      var str = localStorage.getItem("$file$"+name);
      var data;

      try {
	data = JSON.parse(str);
	if ( typeof(data) != "object" )
	  data = undefined;
      } catch(err) {
	data = undefined;
      }

      if ( data ) {
	this[pluginName]('setSource', data);
      } else {
	this[pluginName]('reload', name);
      }
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

	obj.type = data.st_type;
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

	if ( $(this)[pluginName]('getActive') )
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
	title = "File system -- " + utils.basename(meta.path);
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
     * Get a description of the selection to be transferred with a
     * chat message.
     */
    getSelection: function() {
      if ( this.hasClass("prolog-editor") ) {	/* plain editor */
	var sel = this.prologEditor('getSelection');
	return sel ? sel[0].selections : null;
      } else if ( this.hasClass("notebook") ) {
	return this.notebook('getSelection');
      } else {
	console.log("Don't know how to get selection from", this);
      }
    },

    /**
     * @returns {String} description of the selection to use inside
     * a link or button
     */
    getSelectionLabel: function(sel) {
      function editorLabel(sels) {
	var label = "";
	for(var i=0; i<sels.length; i++) {
	  var s = sels[i];
	  if ( label != "" )
	    label += ";";
	  label += "@L"+(s.from.line+1);
	  if ( s.to.line != s.from.line )
	    label += "-"+(s.to.line+1);
	}
	return label;
      }

      if ( sel[0].selections ) {
	var label = "";

	for(var i=0; i<sel.length; i++) {
	  var ed = sel[i];
	  if ( label != "" )
	    label += ",";
	  label += (ed.cell||"") + editorLabel(ed.selections);
	}
	return label;
      } else {
	return editorLabel(sel);
      }
    },

    /**
     * Restore a selection retrieved using `getSelection`.
     */
    restoreSelection: function(sel) {
      if ( this.hasClass("prolog-editor") ) {	/* plain editor */
	return this.prologEditor('restoreSelection', sel);
      } else if ( this.hasClass("notebook") ) { /* notebook */
	return this.notebook('restoreSelection', sel);
      } else {
	console.log(sel);
      }
    },

    /**
     * @return {Boolean} `true` if storage is in an active tab
     */
    getActive: function() {
      return $(this).closest(".tab-pane.active").length == 1;
    },

    /**
     * Get a document identification string for chats, status, etc.
     * @param {String} [type] defines the type of storage supported
     * @param {Object} [data] is the data object from which to derive
     * the id.
     * @return {String} identifier for the document
     */
    docid: function(type, data) {
      data = data||this.data(pluginName);

      if ( !type || type == data.st_type ) {
	var meta = data.meta||{};

	if ( data.st_type == "gitty" ) {
	  return "gitty:"+meta.name;
	} else if ( data.st_type == "filesys" ) {
	  return "filesys:"+meta.path;
	} else if ( data.st_type == "external" ) {
	  return "url:"+data.url;
	}
      }
    },

    /**
     * Open the chat window for the current file
     */
    chat: function(action) {
      var data = this.data(pluginName);
      var docid = this.storage('docid', 'gitty');

      if ( docid ) {
	var chat = this.closest(".tab-pane").find(".chatroom");

	if ( chat.length > 0 ) {
	  if ( action == 'update' )
	    chat.chatroom('docid', docid, 'close');
	  else
	    utils.flash(chat);
	} else if ( action != 'update' ) {
	  chat = $($.el.div({class:"chatroom"}));
	  var percentage;

	  if ( typeof(action) == "number" )
	    percentage = action;
	  else if ( action == 'large' )
	    percentage = 80;
	  else
	    percentage = 20;

	  chat.chatroom({docid:docid});
	  this.tile('split', chat, "below", percentage, 150)
	      .addClass("chat-container");
	}
      } else if ( action == 'update' ) {
	this.storage('close_chat');
      } else if ( !data.st_type ) {
	modal.alert("You can only chat about a saved document.<br>"+
		    "Please save your document and try again.");
      } else {
	modal.alert("The chat facility is only available for "+
		    "user-saved files.<br>"+
		    "You can use the <b>Open hangout</b> menu from "+
		    "the top-right bell to access the hangout room.");
      }

      return this;
    },

    /**
     * Close associated chat
     */
    close_chat: function() {
      this.closest(".chat-container").find(".chatroom").chatroom('close');
    },

    /**
     * @return percentage of the chatroom, `true` when undefined or
     * `false` if there is no chatroom.
     */
    chatroom_size: function() {
      var tab = this.closest(".tab-pane");
      var cr = tab.find(".chatroom");
      if ( cr.length > 0 ) {
	var h = tab.height();
	if ( h == 0 )
	  return 20;			/* default */
	return Math.round(cr.height()*100/h);
      }
      return false;
    },

    /**
     * Act upon the arrival of a chat message.  Update the tab title.
     * If the message is not displayed and it is not permanent
     * (`create == false`) we should not update the counter.
     */
    chat_message: function(msg) {
      if ( !msg.displayed && msg.create == false )
	return this;

      return this.each(function() {
	var elem = $(this);

	if ( msg.docid == elem.storage('docid') ) {
	  var data = elem.data(pluginName);

	  if ( data.chats ) {
	    if ( data.chats.total != undefined ) data.chats.total++;
	    if ( data.chats.count != undefined ) data.chats.count++;
	  } else {
	    data.chats = {total:1};
	  }

	  elem.storage('update_tab_title', 'chats++');
	}
      });
    },

    /**
     * Edit the _follow_ options for this file.
     */

    follow: function() {
      var docid = this.storage('docid', 'gitty');

      if ( docid ) {
	modal.server_form({
	  title: "Follow file options",
	  url:   config.http.locations.follow_file_options,
	  data:  {docid: docid}
	});
      } else {
	modal.alert("Sorry, can only follow files");
      }
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

      if ( data.cleanData && data.getValue &&
	   data.cleanData != data.getValue() ) {
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
    },

    /**
     * Called if this element is inside a tab this is being closed
     */
    close: function() {
    },

    /**
     * maintain `data.is_clean`
     */
    markClean: function(clean) {
      var data = this.data(pluginName);

      data.is_clean = clean;
    },

    /**
     * Broadcast all open (gitty) files. This is used to synchronise
     * state.  Each state object has the property `file`.  If the file
     * is locally modified, `state.modified` is `true` and if the file
     * is the visible one, `state.visible` is true
     * @param {Bool} [always] if `true`, also report if no files are
     * open.
     */
    chat_status: function(always) {
      var opened = [];

      this.each(function() {
	var data = $(this).data(pluginName);

	if ( data.st_type == "gitty" && data.meta && data.meta.name ) {
	  var state = { file:  data.meta.name };

	  if ( !data.is_clean ) state.modified = true;
	  if ( $(this).is(":visible") ) state.visible = true;
	  opened.push(state);
	}
      });

      if ( always || opened.length > 0 )
	$("#chat").trigger('send',
			   { type:'has-open-files',
			     files:opened
			   });
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
