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
	 "modal",
	 "laconic"
       ],
       function($, form, CodeMirror, utils, config, modal) {

(function($) {
  var pluginName = 'chatroom';

  /** @lends $.fn.chatroom */
  var methods = {
    /**
     * {Object} [options]
     * {String} [options.docid] Document identifier
     */

    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	var btn;
	var close;
	var text;

	data.docid = options.docid;
	elem.data(pluginName, data);	/* store with element */

	elem.addClass("chatroom");

					/* build DOM */

	btn  = $.el.div({class:"btn-group dropup"},
			$.el.button({ type:"button",
				      class:"btn btn-primary btn-xs "+
				            "dropdown-toggle",
				      'data-toggle':"dropdown"
				    },
				    "Send ",
				    $.el.span({class:"caret"})),
		   ul = $.el.ul({class:"dropdown-menu pull-right"}));
	text = $.el.textarea({ placeholder:"Type chat message here ..."
			     }),

	elem.append($.el.div({class:"chat-conversation"},
			     $.el.div({class:"stretch"}),
			     $.el.div({class:"inner"})),
	    close = $.el.span({class:"glyphicon menu glyphicon-remove-circle"}),
		    $.el.div({class:"chat-input"},
			     $.el.table({class:"chat-input"},
					$.el.tr($.el.td({class:"chat-text"}, text),
						$.el.td({class:"chat-send"}, btn)))));

					/* event handling */
	form.widgets.populateMenu($(btn), elem, {
	  "Message": function() {
	    this.chatroom('send');
	  },
	  "Send my query": function() {
	    var query = $(".prolog-query-editor").queryEditor('getQuery');
	    if ( query.trim() != "" ) {
	      this.chatroom('send', [{type:"query", query:query}]);
	    } else {
	      modal.alert("Your query editor is empty");
	    }
	  }
	});
	$(close).on("click", function() {
	  elem.tile('close');
	});
	$(text).keypress(function(ev) {
	  if ( ev.which == 13 ) {
	    elem.chatroom('send');
	    ev.preventDefault();
	    return false;
	  }
	});
	elem.on("click", "button", function(ev) {
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
	elem.on("pane.resize", function() {
	  elem.chatroom('scrollToBottom', true);
	});

	elem.chatroom('load_from_server');
      });
    },

    close: function() {
      return this.tile('close');
    },

    /**
     * Send a chat message.
     * @param {Object} [extra] provides additional fields to attach
     * to the message
     */
    send: function(payload) {
      var data = this.data(pluginName);
      var msg = {type:"chat-message"};
      var ta = this.find("textarea");
      msg.text = ta.val().trim();

      if ( msg.text != "" || payload !== undefined ) {
	ta.val("");

	msg.payload = payload;
	msg.docid   = data.docid;
	msg.user    = $("#chat").chat('self');
	$("#chat").chat('send', msg);
      }
    },

    /**
     * Add a chat object to the conversation.
     * @param {Object} msg
     * @param {String} msg.html is the HTML content of the object
     * @param {String} msg.text is the ext of the object
     * @param {Object} msg.user Sender description
     */
    add: function(msg) {
      var data = this.data(pluginName);

      if ( msg.docid != data.docid )
	return this;

      var self = $("#chat").chat('self', []);
      user = msg.user||{};
      var is_self = ( user.id == self.id ||
		      user.avatar == self.avatar );

      elem = $($.el.div({class:"chat-message"+(is_self ? " self" : ""),
			 'data-userid':user.id}));
      if ( !is_self && user.avatar ) {
	elem.append($.el.img({ class:"avatar", src:user.avatar }));
      }
      elem.append($.el.span({class:"chat-sender"},
			    is_self ? "Me" : user.name));

      if ( msg.html ) {
	var span = $.el.span({class:"chat-message html"});
	$(span).html(msg.html);
	elem.append(span);
      } else if ( msg.text ) {
	elem.append($.el.span({class:"chat-message text"}, msg.text));
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

      this.find(".inner").append(elem);
      this.chatroom('scrollToBottom');

      return this;
    },

    load_from_server: function(ifempty) {
      var data = this.data(pluginName);
      var elem = $(this);

      $.get(config.http.locations.chat_messages,
	    { docid: data.docid
	    },
	    function(messages) {
	      if ( ifempty && messages.length == 0 ) {
		elem.chatroom('close');
	      } else {
		for(var i=0; i<messages.length; i++) {
		  elem.chatroom('add', messages[i]);
		}
	      }
	    }).fail(function(jqXHR, textStatus, errorThrown) {
	      modal.ajaxError(jqXHR);
	    });

      return this;
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
	this.find(".inner").html("");
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
	inner = elem.find("div.inner");
	conv  = elem.find(".chat-conversation");
	var height = inner.height();
	var room   = conv.height() - height - 4 - 4;

	if ( room > 0 || onlydown !== true ) {
	  conv.find("div.stretch").height(room > 0 ? room : 0);
	  conv.scrollTop(height);
	}
      });

      return this;
    }
  }; // methods


		 /*******************************
		 *	 PAYLOAD HANDLERS	*
		 *******************************/

  var payload_handlers = {
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

      function btn(glyph, title) {
	return form.widgets.glyphIconButton(glyph,
					    {class:"btn-xs", title:title});
      }

      this.append(" ", $.el.span(
        {class:"update"},
	old = btn("play",    "Open old version"),
	dif = btn("zoom-in", "View changes"),
        nwe = btn("play",    "Open new version")));

      $(old).data('commit', update.previous);
      $(dif).data('diff',   {from:update.previous, to:update.commit,
			     name:update.name});
      $(nwe).data('commit', update.commit);
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
