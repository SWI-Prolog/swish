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

define([ "jquery", "form", "laconic" ],
       function($, form) {

(function($) {
  var pluginName = 'chatroom';

  /** @lends $.fn.chatroom */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */
	var btn;
	var close;
	var text;


	elem.data(pluginName, data);	/* store with element */
	elem.addClass("chatroom");

					/* build DOM */

	btn  = $.el.div({class:"btn-group"},
			$.el.button({ type:"button",
				      class:"btn btn-primary btn-xs "+
				            "dropdown-toggle",
				      'data-toggle':"dropdown"
				    },
				    "Send ",
				    $.el.span({class:"caret"})),
		   ul = $.el.ul({class:"dropdown-menu pull-right"}));

	elem.append($.el.div({class:"chat-conversation"},
			     $.el.div({class:"stretch"}),
			     $.el.div({class:"inner"})),
	    close = $.el.span({class:"glyphicon menu glyphicon-remove-circle"}),
		    $.el.div({class:"chat-input"},
		      text = $.el.textarea({ class:"chat-input",
					     placeholder:"Type chat message here ..."
					   }),
			     btn));

					/* event handling */
	form.widgets.populateMenu($(btn), elem, {
	  "Message": function() {
	    this.chatroom('send');
	  },
	  "Send my query": function() {
	    var query = $(".prolog-query-editor").queryEditor('getQuery');
	    if ( query.trim() != "" ) {
	      this.chatroom('send', {query:query});
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
	elem.on("pane.resize", function() {
	  elem.chatroom('scrollToBottom', true);
	});

					/* add pending messages */
	var pending = $("#chat").chat('queued_chat_messages', true);
	for(var i=0; i<pending.length; i++) {
	  elem.chatroom('add', pending[i]);
	}
      });
    },

    /**
     * Send a chat message.
     * @param {Object} [extra] provides additional fields to attach
     * to the message
     */
    send: function(extra) {
      var msg = {type:"chat-message"};
      var ta = this.find("textarea");
      msg.text = ta.val().trim();

      if ( msg.text != "" || extra ) {
	msg = $.extend(msg, extra||{});
	var users = $("#chat").chat('users');

	ta.val("");

	msg.users = users.users;
	msg.user  = users.self;
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
      var self = $("#chat").chat('self');
      user = msg.user;
      var is_self = (user.id == self);

      elem = $($.el.div({class:"chat-message"+(is_self ? " self" : "")}));
      if ( !is_self && user.avatar ) {
	elem.append($.el.img({class:"avatar", src:user.avatar}));
      }
      elem.append($.el.span({class:"chat-sender"},
			    is_self ? "Me" : user.name));

      if ( msg.html )
	elem.html(msg.html);
      else if ( msg.text )
	elem.append($.el.span(msg.text));

      if ( msg.query ) {
	var query = msg.query;
	var btn = $.el.button({ class:"btn btn-xs btn-primary"
			      },
			      "Query ",
			      form.widgets.glyphIcon("download"));
	$(btn).on("click", function() {
	  $(".prolog-query-editor").queryEditor('setQuery', query);
	});

	elem.append($.el.br(), btn);
      }

      this.find(".inner").append(elem);
      this.chatroom('scrollToBottom');

      return this;
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
