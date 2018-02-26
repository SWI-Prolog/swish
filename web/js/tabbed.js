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
 * This file deals with tabbed panes.  It implements dynamic tabs on top
 * if Bootstrap.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "form", "config", "preferences", "modal",
	 "laconic", "search", "chatbell", "sourcelist" ],
       function($, form, config, preferences, modal) {
var tabbed = {
  tabTypes: {},
  type: function(from) {
    var ext = from.split('.').pop();

    for(var k in tabbed.tabTypes) {
      if ( tabbed.tabTypes.hasOwnProperty(k) &&
	   tabbed.tabTypes[k].dataType == ext )
	return tabbed.tabTypes[k];
    }
  }
};

tabbed.tabTypes.permalink = {
  dataType: "lnk",
  typeName: "program",
  label: "Program",
  create: function(dom, options) {
    $(dom).addClass("prolog-editor")
	  .prologEditor($.extend({save:true}, options))
	  .prologEditor('makeCurrent');
  }
};


(function($) {
  var pluginName = 'tabbed';
  var tabid = 0;

  /** @lends $.fn.tabbed */
  var methods = {
    /**
     * Turn the current element into a Bootstrap tabbed pane. All
     * children of the current element are changed into tabs.  The
     * child can control the mapping using:
     *
     *   - `data-label = "Label"`
     *   - `data-close = "disabled"`
     */
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	data.newTab   = options.newTab;
	data.tabTypes = options.tabTypes || tabbed.tabTypes;
	elem.data(pluginName, data);	/* store with element */

	elem.addClass("tabbed unloadable");
	elem.tabbed('makeTabbed');
	elem.on("trace-location", function(ev, prompt) {
	  elem.tabbed('showTracePort', prompt);
	});
	elem.on("data-is-clean", function(ev, clean) {
	  var tab = $(ev.target).closest(".tab-pane");
	  var a   = elem.tabbed('navTab', tab.attr('id'));

	  if ( a )
	  { if ( clean )
	      a.removeClass("data-dirty");
	    else
	      a.addClass("data-dirty");
	  }
	});
	elem.on("unload", function(ev) {
	  if ( ev.target == elem[0] &&
	       elem.closest(".swish").swish('preserve_state') ) {
	    var state = elem[pluginName]('getState');
	    localStorage.setItem("tabs", JSON.stringify(state));
	  }
	});
	elem.on("restore", function(ev) {
	  var state;

	  if ( ev.target == elem[0] ) {
	    // TBD: How to act with already open documents?
	    try {
	      var str = localStorage.getItem("tabs");
	      var state = JSON.parse(str);
	    } catch(err) {
	    }

	    if ( typeof(state) == "object" ) {
	      elem[pluginName]('setState', state);
	    }
	  }
	});
	elem.on("preference", function(ev, pref) {
	  if ( pref.name == "preserve-state" &&
	       pref.value == false ) {
	    localStorage.removeItem("tabs");
	  }
	});
      });
    },

    /**
     * Turn the pane into a tabbed pane
     */
    makeTabbed: function() {
      var children = this.children();
      var ul = $.el.ul({ class:"nav nav-tabs",
			 role:"tablist"
		       });
      var contents = $.el.div({class:"tab-content"});

      this.prepend(contents);
      this.prepend(ul);

      $(ul).on("click", "span.xclose", function(ev) {
	var id = $(ev.target).parent().attr("data-id");
	$(ev.target).parents(".tabbed").first().tabbed('removeTab', id);
	ev.preventDefault();
      });
      $(ul).on("click", "a", function(ev) {
	$(ev.target).closest("a").tab('show');
	ev.preventDefault();
      });

			/* Turn children into tabs */
      for(var i=0; i<children.length; i++) {
	var child = $(children[i]);
	var id = genId();
	var label = child.attr("data-label") || "Unknown";
	var close = child.attr("data-close") != "disabled";
	var active = (i == children.length-1);	/* activate last */

	var li = this.tabbed('tabLabel', id, label, close);
	if ( active )
	  $(li).addClass("active");
	$(ul).append(li);
	$(contents).append(wrapInTab($(children[i]), id, active));
      }

			/* Create and handle "+" button */
      var create = $.el.a({ class: "tab-new compact",
			    title: "Open a new tab"
			  },
			  glyphicon("plus"));
      $(ul).append($.el.li({ class: "tab-new", role:"presentation" }, create));
      $(create).on("click", function(ev) {
	var tabbed = $(ev.target).parents(".tabbed").first();

	tabbed.tabbed('newTab');
	ev.preventDefault();
	return false;
      });

			/* Handle tab-switching */
      $(ul).on("shown.bs.tab", "a", function(ev) {
	var newContentID  = $(ev.target).data("id");
	$("#"+newContentID+" .swish-event-receiver").trigger("activate-tab");
	$("#"+newContentID+" .storage").storage("activate");
      });

      if ( this.tabbed('navContent').children().length == 0 ) {
	this.tabbed('newTab');
      }
    },

    /**
     * Add an empty new tab from the "+" button.  This calls
     * options.newTab() to return a DOM element for the new
     * tab.
     * @param {HTMLElement} [content] Content for the new tab
     * If omitted, it calls `options.newTab` or uses the method
     * `tabSelect`.
     * @return {jQuery} object representing the created tab
     */
    newTab: function(dom, active) {
      var data = this.data(pluginName);

      if ( dom == undefined ) {
	if ( data.newTab ) {
	  dom = data.newTab();
	} else {
	  var sl;
	  dom = this.tabbed('tabSelect');
	  $(dom).append(this.tabbed('profileForm'),
			$.el.hr(),
			//this.tabbed('searchForm'),
		        sl = $.el.div({class:"sourcelist"}));
	  $(sl).sourcelist();
	}
      }

      if ( active == undefined )
	active = true;

      return this.tabbed('addTab', dom, {active:active,close:true});
    },

    getState: function() {
      var state = this[pluginName]('get_ordered_storage').storage('getState');

      state.pathname = window.location.pathname;
      state.time     = new Date().getTime();

      return state;
    },

    setState: function(state) {
      var elem = this;

      for(var i=0; i<state.tabs.length; i++) {
	var data = state.tabs[i];
	this[pluginName]('restoreTab', data);
      }
    },

    restoreTab: function(data) {
      var elem = this;
      var tab;

      data.query = null;		/* null keeps query */
      data.noHistory = true;		/* do not update window path */

      var existing = this.find(".storage").storage('match', data);
      if ( existing ) {
	tab = existing.closest(".tab-pane");
	elem.tabbed('move_right', tab);
      } else
      { tab = undefined;
      }

      function restoreData(into, from) {
	if ( from.data ) {
	  into.find(".storage").storage('setValue', {
	    data: from.data,
	    role: 'source'
	  });
	}
	if ( from.chatroom ) {
	  into.find(".storage").storage('chat', from.chatroom);
	}
      }

      if ( existing ) {
	restoreData(tab, data);
      } else if ( existing ) {
	/* nothing to do? */
      } else {				/* TBD: Centralise */
	var select = this.find("div.tabbed-select");
	var newtab;
	var restoring = '<div class="restore-tab">Restoring ' +
	                   (data.file||data.url) + " ..." +
			'</div>';

	if ( select.length > 0 )  {
	  newtab = select.first().closest(".tab-pane");
	  newtab.html(restoring);
	} else {
	  newtab = elem.tabbed('newTab', $(restoring), Boolean(data.active));
	}

	if ( data.st_type == "gitty" ) {
	  var url = config.http.locations.web_storage + data.file;
	  $.ajax({ url: url,
		   type: "GET",
		   data: {format: "json"},
		   success: function(reply) {
		     reply.url = url;
		     reply.st_type = "gitty";
		     reply.noHistory = true;
		     if ( !elem.tabbed('setSource', newtab, reply) ) {
		       console.log("Failed to restore", data.file);
		       elem.tabbed('removeTab', tab.attr("id"));
		     }
		     restoreData(newtab, data);
		     if ( newtab.hasClass("active") )
		       newtab.find(".storage").storage("activate");
		   },
		   error: function(jqXHR) {
		     modal.ajaxError(jqXHR);
		   }
	  });
	} else if ( data.url ) {
	  $.ajax({ url: data.url,
		   type: "GET",
		   data: {format: "json"},
		   success: function(source) {
		     var msg;

		     if ( typeof(source) == "string" ) {
		       msg = { data: source };
		       msg.st_type = "external";
		     } else if ( typeof(source) == "object" &&
				 typeof(source.data) == "string" ) {
		       msg = source;
		       msg.st_type = "filesys";
		     } else {
		       alert("Invalid data");
		       return;
		     }
		     msg.noHistory = true;
		     msg.url = data.url;
		     if ( !elem.tabbed('setSource', newtab, msg) ) {
		       console.log("Failed to restore", data.url);
		       elem.tabbed('removeTab', newtab.attr("id"));
		     }
		     restoreData(newtab, data);
		     if ( newtab.hasClass("active") )
		       newtab.find(".storage").storage("activate");
		   },
		   error: function(jqXHR) {
		     modal.ajaxError(jqXHR);
		   }
	  });
	} else {
	  console.log("Cannot restore ", data);
	}
      }
    },


    /**
     * Add a new tab from the provided source.  If there is a _select_
     * (new) tab, open the data in this tab.
     */
    tabFromSource: function(src) {
      var elem = this;
      var select = this.find("div.tabbed-select");

      if ( typeof(src) == "string" )
	src = {data:src};

      function inNewTab() {
	var tab = elem.tabbed('newTab', $("<span></span>"));
	if ( !elem.tabbed('setSource', tab, src) ) {
	  elem.tabbed('removeTab', tab.attr("id"));
	}
      }

      if ( select.length > 0 ) {
	var tab = select.first().closest(".tab-pane");
	this.tabbed('show', tab.attr("id"));
	this.tabbed('setSource', tab, src);
      } else if ( src.newTab || preferences.getVal("new-tab") ) {
	inNewTab();
      } else
      { var tab;

	this.find(".storage").each(function(i, st) {
	  if ( $(st).storage('setSource', src) ) {
	    tab = $(st).closest(".tab-pane");
	    return false;
	  }
        });

	if ( tab )
	  this.tabbed('show', tab.attr("id"));
	else
	  inNewTab();
      }

      return this;
    },

    /**
     * Transform the new tab into a tab that can hold the requested
     * source.
     * @return {Boolean} `true` if a suitable type was found
     */
    setSource: function(tab, src) {
      if ( typeof(src) == "object" &&
	   ((src.meta && src.meta.name) || src.url) )
      { var name = (src.meta && src.meta.name) ? src.meta.name : src.url;
	var tabType = tabbed.type(name);
	var content = $.el.div();

	tab.html("");
	tab.tabbed('title', tabType.label, tabType.dataType);
	tab.append(content);
	tabType.create(content);
	$(content).storage('setSource', src);
	return true;
      }

      return false;
    },

    /**
     * Show a tracer port. This implies finding the proper editor,
     * making sure it is visible and ask it to show to port or, if
     * no editor is displaying this source, create a new one.
     * @param {Object} prompt
     * @param {Object} [prompt.source]
     * @param {Object} [prompt.source.file] is the file associated
     * with the debug event.  Currently, we accept
     *
     *   - `pengine://<pengine>/src` refers to the editor that provided
     *     the source for pengine <pengine>
     *	 - `swish://<file>.pl` refers to an included file from the
     *	   store.
     */
    showTracePort: function(prompt) {
      if ( prompt && prompt.source && prompt.source.file ) {
	var file = prompt.source.file;
	var pengineID, store;
	var editors;

	function isPengineSrc() {
	  var id;

	  if ( file.startsWith("pengine://") )
	    return file.split("/")[2];
	}

	function isStoreSrc() {
	  var prefix = "swish://";
	  if ( file.startsWith(prefix) )
	    return file.slice(prefix.length);
	}

	if ( (pengineID=isPengineSrc()) ) {
	  editors = this.find(".prolog-editor")
			.filter(function(i, e) {
			  return $(e).prologEditor('pengine', {has:pengineID});
			});
	} else if ( (store=isStoreSrc()) ) {
	  editors = this.find(".storage")
			.storage('match', {file:store});

	  if ( !editors ) {
	    this.closest(".swish")
	        .swish('playFile',
		       { file: store,
			 newTab: true,
			 noHistory: true,
			 prompt: prompt
		       });
	    return this;
	  }
	}

	if ( editors )
	  editors.prologEditor('showTracePort', prompt);
      }

      return this;
    },


    /**
     * Add a new tab using content
     * @param {Object} content is the DOM node to use as content for the
     * tab.
     * @param {Object} options
     * @param {Boolean} [options.active] if `true`, make the new tab
     * active
     * @param {Boolean} [options.close] if `true`, allow closing the new
     * tab.
     * @return {jQuery} the created tab element
     */
    addTab: function(content, options) {
      var ul  = this.tabbed('navTabs');
      var id  = genId();
      var tab =	wrapInTab(content, id, options.close);

      this.tabbed('navContent').append(tab);

      var li  = this.tabbed('tabLabel', id, "New tab", close, "select");

      var create = ul.find("a.tab-new");
      if ( create.length == 1 )
	$(li).insertBefore(create.first().parent());
      else
	ul.append(li);

      if ( options.active )
	$(li).find("a").first().tab('show');

      return tab;
    },

    /**
     * Remove tab with given Id. If the tab is the active tab, make the
     * previous tab active, or if there is no previous, the next. If the
     * tabbed environment becomes empty, add a virgin tab.
     *
     * @param {String} id is the id of the tab to destroy
     */
    removeTab: function(id) {
      var li  = this.tabbed('navTabs').find("a[data-id='"+id+"']").parent();
      var tab = $("#"+id);
      var new_active;

      if ( tab.find(".storage").storage('unload', "closetab") == false )
	return;

      if ( tab.is(":visible") )
	new_active = li.prev() || li.next();
      li.remove();
					/* HACK: close embedded runners */
      tab.find(".prolog-runner").prologRunner('close');
      tab.find(".storage").storage('close');
      tab.remove();
      if ( new_active && new_active.length > 0 ) {
	new_active.find("a").first().tab('show');
      } else if ( this.tabbed('navContent').children().length == 0 ) {
	this.tabbed('newTab');
      }

      $(".storage").storage('chat_status', true);
    },

    /**
     * Show indicated tab.
     * @param {String} id is the id of the tab to show.
     */
    show: function(id) {
      var a = this.tabbed('navTab', id);
      if ( a ) {
	a.tab('show');
      }

      $(".storage").storage('chat_status', true);
    },

    /**
     * Move the argument tab or tab id to the right of all
     * tabs.
     */
    move_right: function(tab) {
      var id;
      var ul = this.find(">ul");

      if ( typeof(tab) == "string" )
	id = tab;
      else
	id = tab.attr('id');

      ul.find("a[data-id="+id+"]")
        .closest("li")
        .insertBefore(ul.children().last());
    },

    /**
     * Create a label (`li`) for a new tab.
     * @param {String} id is the identifier of the new tab
     * @param {String} label is the textual label of the new tab
     * @param {Boolean} close determines whether or nor a close button
     * is added to the tab.
     * @param {String} [type="pl"] indicates the type of the tab. This
     * is used for associating an icon with the tab.
     */
    tabLabel: function(id, label, close, type) {
      var close_button;
      var chat;

      if ( close )
      { close_button = glyphicon("remove", "xclose");
	$(close_button).attr("title", "Close tab");
      }
      type = type||"pl";

      var a1 = $.el.a({class:"compact", href:"#"+id, "data-id":id},
		      $.el.span({class:"tab-icon type-icon "+type}),
		      $.el.span({class:"tab-dirty",
		                 title:"Tab is modified. "+
				       "See File/Save and Edit/View changes"}),
	       chat = $.el.a({class:'tab-chat'}),
		      $.el.span({class:"tab-title"}, label),
		      close_button);
      var li = $.el.li({role:"presentation"}, a1);

      $(chat).chatbell()
             .on("click", function(ev) {
	var id = $(ev.target).closest("a.compact").data("id");
	$("#"+id).find(".storage").storage('chat');
	return false;
      });

      return li;
    },

    /**
     * Calling obj.tabbed('anchor') finds the <a> element
     * representing the tab label from the node obj that appears
     * somewhere on the tab
     */
    anchor: function() {
      var tab    = this.closest(".tab-pane");

      if ( tab.length == 0 ) {
	return undefined;		/* e.g., fullscreen mode */
      }

      var tabbed = tab.closest(".tabbed");
      var id     = tab.attr("id");
      var ul	 = tabbed.tabbed('navTabs');
      var a      = ul.find("a[data-id="+id+"]");

      return a;
    },

    /**
     * Find the storage objects in the tabbed environment in the
     * order of the tabs.  Note that the content divs maye be ordered
     * differently.
     */
    get_ordered_storage: function() {
      var elem = this;
      var result = [];

      this.find(">ul>li").each(function() {
	var id = $(this).find(">a").data('id');
	elem.find(">div.tab-content>div[id="+id+"] .storage").each(function() {
	  result.push(this);
	});
      });

      return $(result);
    },

    /**
     * This method is typically _not_ called on the tab, but on some
     * inner element of the tab.  It changes the title of the tab.
     * @param {String} title is the new title for the tab.
     * @param {String} [type="pl"] is the new type for the tab.
     */
    title: function(title, type) {
      var a = this.tabbed('anchor');

      if ( a ) {
	a.find(".tab-title").text(title);
	if ( type ) {
	  var icon = a.find(".tab-icon");
	  icon.removeClass();
	  icon.addClass("tab-icon type-icon "+type);
	}
      }

      return this;
    },

    /**
     * Set the chat message feedback for this tab
     * @param {Object} [chats]
     * @param {Number} [chats.count] number of available chat messages
     * on the document.
     */
    chats: function(chats) {
      var a = this.tabbed('anchor');

      if ( a ) {
	a.find(".chat-bell").chatbell('update', chats);
      }

      return this;
    },

    /**
     * Increment the chat count and possibly associate the bell
     * with the document identifier.
     * @param {String} [docid] is the document identifier to associate
     * with.
     */
    'chats++': function(docid) {
      var a = this.tabbed('anchor');

      if ( a ) {
	a.find(".chat-bell").chatbell('chats++', docid);
      }

      return this;
    },


    /**
     * Default empty tab content that allows the user to transform
     * the tab into the desired object.
     * @return {Object} containing content for the new tab
     */
    tabSelect: function() {
      var data = this.data(pluginName);
      var dom = $.el.div({class:"tabbed-select"},
			 $.el.div({class: "tabbed-create"},
				  $.el.label({class: "tabbed-left"},
					     "Create a "),
				  g=$.el.div({class:"btn-group",role:"group"}),
				  $.el.label({class: "tabbed-right"}, "here")));
      var types = [];

      for(var k in data.tabTypes) {
	if ( data.tabTypes.hasOwnProperty(k) &&
	     data.tabTypes[k].order )
	  types.push(k);
      }
      types.sort(function(a,b) {
	return data.tabTypes[a].order - data.tabTypes[b].order;
      });

      for(var i = 0; i<types.length; i++) {
	var type = data.tabTypes[types[i]];

	$(g).append($.el.button({ type:"button",
				  class:"btn btn-primary",
				  "data-type":type.typeName,
				  "data-ext":type.dataType
				},
				type.label));
      }

      $(g).on("click", ".btn", function(ev) {
	var type    = $(ev.target).data('type');
	var tab     = $(ev.target).closest(".tab-pane");
	var content = $.el.div();
	var options = $.extend({}, tabbed.tabTypes[type]);
	var profile = tab.find("label.active > input[name=profile]").val();

	if ( profile ) {
	  options.profile = profile;
	  options.value   = tab.tabbed('profileValue', profile,
				       tabbed.tabTypes[type].dataType);
	  if ( options.value != undefined )
	    preferences.setVal("default-profile", profile);
	}

	tab.html("");
	tab.tabbed('title', options.label, options.dataType);
	tab.append(content);
	tabbed.tabTypes[type].create(content, options);
      });
      $(g).addClass("swish-event-receiver");
      $(g).on("download save fileInfo print", function(ev) {
	var tab = $(ev.target).closest(".tab-pane");
	if ( tab.is(":visible") ) {
	  var typelabel = { "download" : "you wish to download",
			    "save"     : "you wish to save",
			    "print"    : "you wish to print",
			    "fileInfo" : "for which you want details"
	  };

	  modal.alert("Please activate the tab "+typelabel[ev.type]);
	  ev.stopPropagation();
	}
      });
      $(g).on("profile-selected", function(ev, profile) {
	$(ev.target).find("button").each(function() {
	  $(this).prop('disabled',
		       profile.type.indexOf($(this).data('ext')) < 0);
	});
      });

      return dom;
    },

    /**
     * Find sources
     */
    searchForm: function() {
      var sform = $.el.form({class: "search-sources"},
	$.el.label({class:"control-label"}, 'Open source file containing'),
        $.el.div(
	  {class: "input-group"},
	  $.el.input({ type: "text",
		       class: "form-control search",
		       placeholder: "Search sources",
		       'data-search-in': "sources store_content",
		     }),
	  $.el.div({ class: "input-group-btn" },
		   $.el.button({class:"btn btn-default", type:"submit"},
			       $.el.i({class:"glyphicon glyphicon-search"})))),
	$.el.div({class: "input-group"},
	  form.fields.radio("smatch",
	    [ { label:"Start of line", value:"sol"},
	      { label:"Start of word", value:"sow", active:true},
	      { label:"Anywhere", value:"anywhere" }
	    ])));
      $(sform).find("input.search").search();

      return sform;
    },

    sourceList: function() {


    },

    profileForm: function() {
      if ( config.swish.profiles && config.swish.profiles.length > 0 ) {
	var def;

	for(var i=0; i<config.swish.profiles.length; i++) {
	  delete config.swish.profiles[i].active;
	}

	if ( (def=preferences.getVal("default-profile")) ) {
	  for(var i=0; i<config.swish.profiles.length; i++) {
	    if ( config.swish.profiles[i].value == def )
	      config.swish.profiles[i].active = true
	  }
	} else {
	  config.swish.profiles[0].active = true;
	}

	var pform =
	$.el.div(
	  {class:"tabbed-profile"},
	  $.el.label({class: "tabbed-left"}, "based on"),
	  $.el.div({class: "input-group select-profile"},
		   form.fields.radio("profile", config.swish.profiles)),
	  $.el.label({class: "tabbed-right"}, "profile"));

	$(pform).on("click", function(ev) {
	  var select = $(ev.target).find("input").val();
	  var profile = profileObject(select);
	  $(ev.target).closest(".tab-pane")
		      .find(".tabbed-create .btn-group")
		      .trigger("profile-selected", profile);
	});

	return pform;
      }
    },

    profileValue: function(name, ext) {
      var url = config.http.locations.swish + "profile/" + name + "." + ext;
      return $.ajax({ url: url,
		      type: "GET",
		      data: {format: "raw"},
		      async: false,
		      error: function(jqXHR) {
			modal.ajaxError(jqXHR);
		      }
      }).responseText;
    },

    /**
     * Get the UL list that represents the nav tabs
     */
    navTabs: function() {
      return this.find("ul.nav-tabs").first();
    },

    navTab: function(id) {
      var a = this.find("ul.nav-tabs").first().find("a[data-id='"+id+"']");
      if ( a.length > 0 )
	return a;
    },

    navContent: function() {
      return this.find("div.tab-content").first();
    }
  }; // methods

  /**
   * Wrap a content element in a Bootstrap tab content.
   * @param {Object} dom is the object that must be wrapped
   * @param {String} id is the identifier to give to the new content
   * @param {Boolean} active sets the tab to active if `true`
   * @return {jQuery} `div` object of class `tab-pane` and the
   * passed `id`.
   */
  function wrapInTab(dom, id, active) {
    $(dom).wrap('<div role="tabpanel" class="tab-pane" id="'+id+'"></div>');
    var wrapped = $(dom).parent();

    if ( active )
      wrapped.addClass("active");

    return wrapped;
  }

  function glyphicon(glyph, className) {
    var span = $.el.span({class:"glyphicon glyphicon-"+glyph});

    if ( className )
      $(span).addClass(className);

    return span;
  }

  function genId()
  { return "tabbed-tab-"+tabid++;
  }

  function profileObject(name) {
    if ( config.swish.profiles ) {
      for(var i=0; i<config.swish.profiles.length; i++) {
	if ( config.swish.profiles[i].value == name )
	  return config.swish.profiles[i];
      }
    }
  }

  /**
   * <Class description>
   *
   * @class tabbed
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tabbed = function(method) {
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

  return tabbed;
});
