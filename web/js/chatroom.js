/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
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
 * Provide the chat window.  The communication is handled by chat.js
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "form", "cm/lib/codemirror", "utils", "config",
	 "modal", "links",
	 "laconic"
       ],
       function($, form, CodeMirror, utils, config, modal, links) {

(function($) {
  var pluginName = 'chatroom';
  var lasthangoutwarning = 0;

  /** @lends $.fn.chatroom */
  var methods = {
    /**
     * {Object} [options]
     * {String} [options.docid] Document identifier
     */

    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = { messages: [] };		/* private data */
	var btn, send;
	var close;
	var text;
	var hangout = "gitty:"+config.swish.hangout;

	data.docid = options.docid;
	elem.data(pluginName, data);	/* store with element */

	elem.addClass("chatroom each-minute swish-event-receiver");

					/* build DOM */

	btn  = $.el.div({class:"btn-group dropup"},
		 send = $.el.button({ type:"button",
				      class:"btn btn-primary btn-xs"
				    }, "Send"),
			$.el.button({ type:"button",
				      class:"btn btn-info btn-xs "+
				            "dropdown-toggle",
				      'data-toggle':"dropdown",
				      'aria-haspopup':true,
				      'aria-expanded':false
				    },
				    $.el.span({class:"caret"})),
		   ul = $.el.ul({class:"dropdown-menu pull-right"}));
	text = $.el.textarea({ placeholder:"Type chat message here ..."
			     }),

	elem.append($.el.div(
		      {class:"chat-conversation"},
		      $.el.div({class:"chat-outer-wrapper"},
			 $.el.div({class:"chat-inner-wrapper"},
			   $.el.div({class:"chat-content-wrapper"},
			      $.el.div({class:"chat-stretch"}),
			      $.el.div({class:"chat-content"}))))),
	    close = $.el.span({class:"glyphicon menu glyphicon-remove-circle"}),
		    $.el.div({class:"chat-input"},
			     $.el.table({class:"chat-input"},
					$.el.tr($.el.td({class:"chat-text"}, text),
						$.el.td({class:"chat-send"}, btn)))));

	$(send).on("click", function() {
	  elem.chatroom('send');
	});

					/* event handling */
	form.widgets.populateMenu($(btn), elem, {
	  "Include my query": function() {
	    var query = $(".prolog-query-editor").queryEditor('getQuery');
	    if ( query.trim() != "" ) {
	      this.chatroom('send',
			    {payload: [{type:"query", query:query}]});
	    } else {
	      modal.alert("Your query editor is empty");
	    }
	  }
	});
	if ( options.docid != hangout ) {
	  form.widgets.populateMenu($(btn), elem, {
	    "Broadcast to hangout": function() {
	      this.chatroom('send',
			    { broadcast: "gitty:"+config.swish.hangout
			    });
	    }
	  });
	}
	$(close).on("click", function() {
	  elem.tile('close');
	});
	if ( options.oneline ) {
	  $(text).keypress(function(ev) {
	    if ( ev.which == 13 ) {
	      elem.chatroom('send');
	      ev.preventDefault();
	      return false;
	    }
	  });
	} else {
	  $(text).on('keyup', function() {
	    var that = $(this);
	    var h;

	    if ( that.scrollTop() != 0 && (h=that.height()) < 500 ) {
	      h += parseFloat(that.css('line-height'));

	      that.animate({ height: h }, 200,
			   function() { elem.chatroom('scrollToBottom'); });
	    }
	  });
	}
	if ( options.docid == hangout ) {
	  $(text).focus(function() {
	    if ( $(text).val() == "" ) {
	      var now = new Date().getTime();

	      if ( now-lasthangoutwarning > 300000 ) {
		lasthangoutwarning = now;

		modal.help({file:"hangout.html", notagain:"hangout"});
	      }
	    }
	  });
	}
	elem.on("click", ".chat-message button", function(ev) {
	  var button = $(ev.target).closest("button");
	  var val;

	  if ( (val = button.data("commit")) ) {
	    elem.closest(".swish").swish('playFile', val);
	  } else if ( (val = button.data("diff")) ) {
	    elem.chatroom('diff', val);
	  }

	  ev.preventDefault();
	  return false;
	});
	elem.on("click", ".chat-content a", links.followLink);
	elem.on("pane.resize", function() {
	  elem.chatroom('scrollToBottom', true);
	});
	elem.on("minute", function() {
	  elem.chatroom('update_time');
	});
	elem.on("activate-tab", function() {
	  elem.chatroom('read_until');
	});

	$(text).height(parseFloat($(text).css('line-height'))+5);

	elem.chatroom('load_from_server');
      });
    },

    close: function() {
      return this.tile('close');
    },

    /**
     * Send a chat message.
     * @param {Object} [options]
     * @param {Array}  [options.payload] Payloads (queries, etc)
     * @param {String} [options.docid] Addressed document of not self
     * @param {String} [options.broadcast] Also broadcast the message
     * to the indicated document id.
     * @param {Bool}   [options.clear] if `false`, do not clear the
     * message window after sending.
     */
    send: function(options) {
      options = options||{};
      var data = this.data(pluginName);
      var msg = {type:"chat-message"};
      var ta = this.find("textarea");
      msg.text = ta.val().trim();
      var payload = options.payload||[];
      var has_payload = false;
      var selection = this.chatroom('storage').storage('getSelection');
      var hangout = "gitty:" + config.swish.hangout;

      if ( selection )
	payload.push({type:"selection", selection:selection});

      for(var i=0; i<payload.length; i++) {
	if ( payload[i].type != 'about' ) {
	  has_payload = true;
	  break;
	}
      }
					/* send first message to hangout */
      if ( !options.broadcast &&
	   data.docid != hangout &&
	   data.messages.length == 0 )
	options.broadcast = hangout;

      if ( msg.text != "" || has_payload ) {
	msg.uuid    = utils.generateUUID();
	msg.payload = payload;
	msg.docid   = options.docid||data.docid;
	if ( options.class )
	  msg.class = options.class;

	if ( options.clear == true ) {
	  this.chatroom('clear');
	} else {
	  data.clear = msg.uuid;
	}

	$("#chat").chat('send', msg);
	if ( options.broadcast ) {
	  msg.payload.unshift({type:"about", docid:data.docid});
	  msg.docid = options.broadcast;
	  $("#chat").chat('send', msg);
	}
      } else if ( !options.payload ) {
	modal.alert("No message to send");
      }
    },

    /**
     * Get the related storage object
     */
    storage: function() {
      return this.closest(".tab-pane").find(".storage");
    },

    /**
     * @param {Object} msg is the chat message object
     * @return {Bool} `true` if the msg is sent by the current user.
     */
    is_self: function(msg) {
      var muser = msg.user||{};
      var suser = config.swish.user||{};

      var self = $("#chat").chat('self');

      return ((muser.id && muser.id == self.id) ||
	      (muser.avatar && muser.avatar == self.avatar) ||
	      (muser.profile_id && muser.profile_id == suser.profile_id));
    },

    /**
     * Indicate we have read all content
     */
    read_until: function(msg) {
      var data;

      if ( msg == undefined &&
	   (data = this.data(pluginName)) &&
	   data.messages.length > 0 )
	msg = data.messages[data.messages.length-1];

      if ( msg ) {
	$("#chat").chat('read_until', msg.docid, msg.time);
	$(".chat-bell").chatbell('read_until', msg.docid, msg.time);
      }
    },

    /**
     * Render a chat message.
     * @param {Object} msg
     * @param {String} msg.html is the HTML content of the object
     * @param {String} msg.text is the ext of the object
     * @param {Object} msg.user Sender description
     */
    render: function(msg) {
      var muser = msg.user||{};
      var suser = config.swish.user||{};

      if ( msg.is_self === undefined )
	msg.is_self = this.chatroom('is_self', msg);

      elem = $($.el.div({class:"chat-message"+(msg.is_self ? " self" : ""),
			 'data-userid':muser.wsid}));
      if ( !msg.is_self && muser.avatar ) {
	elem.append($.el.img({ class:"avatar", src:muser.avatar }));
      }
      elem.append($.el.span({class:"chat-sender"},
			    msg.is_self ? "Me" : muser.name));

      if ( msg.time ) {
	var title = new Date(msg.time*1000).toLocaleString();
	elem.append($.el.span({class:"chat-time", title:title},
			      "(", utils.ago(msg.time), ") "));
	elem.data('time', msg.time);
      }

      if ( msg.payload ) {
	for(var i=0; i<msg.payload.length; i++) {
	  var pl = msg.payload[i];
	  if ( payload_handlers[pl.type] )
	    payload_handlers[pl.type].call(elem, pl);
	  else
	    console.log(pl);
	}
      }

      var html;
      if ( msg.html ) {
	html = msg.html;
      } else if ( msg.text ) {
	html = $($.el.span(msg.text)).html();
	html = markdown(html);
      }

      if ( html ) {
	var span = $.el.span({class:"chat-message html"});
	$(span).html(html);
	elem.append(span);
      }

      return elem;
    },

    /**
     * Add a message to the chatroom.
     * @param {Object} msg is the message to display
     * @param {Bool} [seen] if `true`, claim that the message is read.
     * When omitted it is `true` if the chatroom is visible.
     */
    add: function(msg, seen) {
      var data = this.data(pluginName);

      if ( msg.docid == data.docid )
      { var elem;

	if ( seen == undefined )
	  seen = this.is(":visible");

	if ( msg.is_self == undefined )
	  msg.is_self = this.chatroom('is_self', msg);
	data.messages.push(msg);

	elem = this.chatroom('render', msg);
	this.find(".chat-content").append(elem);
	this.chatroom('scrollToBottom');

	if ( seen )
	  this.chatroom('read_until', msg);
      }

      if ( msg.uuid && msg.uuid == data.clear ) {
	this.chatroom('clear');
      }

      return this;
    },

    clear: function() {
      var ta = this.find("textarea");

      ta.val("");
      ta.height(parseFloat(ta.css('line-height')+5));
    },

    load_from_server: function(ifempty) {
      var data = this.data(pluginName);
      var elem = $(this);

      $.get(config.http.locations.chat_messages,
	    { docid: data.docid
	    },
	    function(messages) {
	      if ( messages.length == 0 ) {
		if ( ifempty )
		  elem.chatroom('close');
		else if ( data.docid != "gitty:"+config.swish.hangout )
		  modal.help({file:"newchat.html", notagain:"newchat"});
	      } else {
		for(var i=0; i<messages.length; i++) {
		  elem.chatroom('add', messages[i], i == messages.length-1 );
		}
	      }
	    }).fail(function(jqXHR, textStatus, errorThrown) {
	      modal.ajaxError(jqXHR);
	    });

      return this;
    },

    update_time: function() {
      return this.find(".chat-message").each(function() {
	var elem = $(this);
	var time;
	if ( (time=elem.data('time')) )
	  elem.find(".chat-time").text("("+utils.ago(time)+") ");
      });
    },

    /**
     * Show diff between versions
     * @param {Object} options
     * @param {String} options.from Base commit
     * @param {String} options.to Target commit
     * @param {String} options.name Name of the file
     */

    diff: function(options) {
      function error(jqXHR) {
	modal.ajaxError(jqXHR);
      }

      $.ajax({
        url: config.http.locations.web_storage + options.from,
	data: {format: "raw"},
	success: function(from) {
	  $.ajax({
	    url: config.http.locations.web_storage + options.to,
	    data: {format: "raw"},
	    success: function(to) {

	      function diffBody() {
		var diff = $.el.div();

		this.append(diff);
		$(diff).diff({
		  base: from,
		  head: to,
		  baseName: options.name + " (before)",
		  headName: options.name + " (after)"
		});
		this.parents("div.modal-dialog").addClass("modal-wide");
	      }

	      form.showDialog({
	        title: "Update differences",
		body:  diffBody
	      });
	    },
	    error: error
	  })
	},
	error: error
      });
    },


    /**
     * Associate with a new document
     */
    docid: function(docid, ifempty) {
      var data = this.data(pluginName);

      if ( data.docid != docid ) {
	this.find(".chat-content").html("");
	data.docid = docid;
	this.chatroom('load_from_server', ifempty);
      }
    },

    /**
     * @param {String} docid docid of rooms we are looking for
     * @returns {jQuery} set of chatrooms pointing at document id
     */
    rooms: function(docid) {
      var rooms = [];

      this.each(function() {
	var room = $(this);
	var data = room.data(pluginName);
	if ( data.docid == docid )
	  rooms.push(this);
      });

      return $(rooms);
    },

    scrollToBottom: function(onlydown) {
      this.each(function() {
	var elem = $(this);
	scroll  =   elem.find("div.chat-inner-wrapper");
	wrap    = scroll.find("div.chat-content-wrapper");
	content =   wrap.find("div.chat-content");
	var height = content[0].scrollHeight;
	var room   = wrap.height() - height - 8;

	if ( room > 0 || onlydown !== true ) {
	  wrap.find("div.chat-stretch").height(room > 0 ? room : 0);
	  scroll.scrollTop(height);
	}
      });

      return this;
    }
  }; // methods


		 /*******************************
		 *	 PAYLOAD HANDLERS	*
		 *******************************/

  var payload_handlers = {
    selection: function(selection) {
      var label   = $().storage('getSelectionLabel', selection.selection);
      var btn = $($.el.button({ class:"btn btn-xs btn-primary"
			      },
			      label + " ",
			      form.widgets.glyphIcon("eye-open")));
      btn.on("click", function(ev) {
	$(ev.target).chatroom('storage')
	            .storage('restoreSelection', selection.selection);
      });

      this.append(" ", btn, " ");
    },

    query: function(query) {
      var btn = $($.el.button({ class:"btn btn-xs btn-primary"
			      },
			      "Query ",
			      form.widgets.glyphIcon("download")));
      btn.on("click", function() {
	var qe = $(".prolog-query-editor");

	qe.queryEditor('setQuery', query.query);
	utils.flash(qe.find(".CodeMirror"));
      });
      sourceToolTip(btn, query.query);

      this.append(" ", btn, " ");
    },

    update: function(update) {
      var old, dif, nwe;

      this.append(" ", $.el.span(
        {class:"update"},
	old = btn("play",    "btn-primary", "Open old version"),
	dif = btn("zoom-in", "btn-info",    "View changes"),
        nwe = btn("play",    "btn-primary", "Open new version")), " ");

      $(old).data('commit', update.previous);
      $(dif).data('diff',   {from:update.previous, to:update.commit,
			     name:update.name});
      $(nwe).data('commit', update.commit);
    },

    about: function(about) {
      var file = about.docid.replace("gitty:", "");

      this.append(" ",
	$.el.a({
	  href:config.http.locations.web_storage+file,
	  class:"store btn btn-primary btn-xs"
	}, file), " ");
    }
  };


  // private functions

  function sourceToolTip(elem, src) {
    var pre = $.el.pre({class:"cm-s-prolog"});

    CodeMirror.runMode(src, "prolog", pre);

    elem.attr("title", $.el.div(pre).innerHTML);
    elem.data("html", true);
    elem.data("placement", "bottom");
    elem.data("trigger", "hover");
    elem.tooltip();
  }

  function btn(glyph, type, title) {
    return form.widgets.glyphIconButton(glyph,
					{class:"btn-xs "+type, title:title});
  }

  /**
   * Perform very simple regex based markdown processing
   */
  function markdown(text) {
    var replace = [
      { regex: /[a-z][a-zA-Z0-9_]*\/[0-9]/g,
        func:  function(match) {
	  return '<a class="builtin" href="/pldoc/man?predicate='+match+'">'
	         +match+'</a>';
	}
      },
      { regex: /[a-zA-Z0-9_-]+\.(pl|swinb)\b/g,
        func:  function(match) {
	  return '<a class="builtin" href="'+
		 config.http.locations.web_storage+
	         match+'">'
	         +match+'</a>';
	}
      },
      { regex: /`(.)`/g,
        func:  function(match, content) {
	  return '<code>'+content+'</code>';
	}
      },
      { regex: /`([\w\[\{\(][^`]*[\w\]\}\)])`/g,
        func:  function(match, content) {
	  return '<code>'+content+'</code>';
	}
      },
      { delim: "\\*", a: "\\*\\b", z: "\\b\\*", tag: "b" },
      { delim: "__",  a: "\\b__",  z: "__\\b",  tag: "b" },
      { delim: "_",   a: "\\b_",   z: "_\\b",   tag: "i" }
    ];

    function wrap(tag) {
      return function(match, content) {
	return "<"+tag+">"+content+"</"+tag+">";
      };
    }

    for(var i=0; i<replace.length; i++) {
      var r = replace[i];

      if ( r.regex ) {
	text = text.replace(r.regex, r.func);
      } else if ( r.delim ) {
	text = text.replace(RegExp(r.a+"([^"+r.delim+"]+)"+r.z,"g"),
			    wrap(r.tag));
      }
    }

    return text;
  }

  /**
   * <Class description>
   *
   * @class chatroom
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.chatroom = function(method) {
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
});
