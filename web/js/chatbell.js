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
 * <Description of the File>
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "form", "modal", "config", "preferences",
	 "laconic", "chatroom"
       ],
       function($, form, modal, config, preferences) {

(function($) {
  var pluginName = 'chatbell';

  /** @lends $.fn.chatbell */
  var methods = {
    /**
     * @param {Object} [options]
     * @param {String} [options.docid] Associate with a document id.
     * If default, try the `data-document` attribute.
     * @param {String} [options.empty_title] Title attribute if there
     * are no new messages
     */
    _init: function(options) {
      options = options||{};

      return this.each(function() {
	var elem = $(this);
	var data = $.extend({}, options); /* private data */

	data.docid = options.docid||elem.data('document');
	elem.data(pluginName, data);	/* store with element */

	elem.addClass("chat-bell");
	elem.attr('title', "Messages available");
	elem.append(form.widgets.glyphIcon("bell"),
		    $.el.span({class:"chat-bell-count"}, "-"));
	elem.chatbell('update');
      });
    },

    /**
     * Set the chat counter and optionally associate the chat bell
     * with a document (`docid`)
     * @param {Object} chats
     * @param {Number} chats.count number of chat messages to report
     * @param {String} [chats.docid] associate bell with document
     */
    chats: function(chats) {
      var data = this.data(pluginName);
      var span = this.find(".chat-bell-count");
      var elem = this;

      function empty() {
	span.text("-");
	elem.removeClass('chat-available chat-alert');
	elem.attr('title', data.empty_title||"No messages available");
      }

      if ( chats == undefined ) {
	delete data.docid;
	delete data.count;
	delete data.total;

	empty();
      } else {
	var count = chats.count == undefined ? chats.total : chats.count;

	if ( chats.docid ) data.docid = chats.docid;
	if ( chats.count ) data.count = chats.count;
	if ( chats.total ) data.total = chats.total;

	if ( chats.total > 0 ) {
	  this.addClass('chat-available');
	  if ( count > 0 ) {
	    span.text(count);
	    this.addClass('chat-alert');
	    this.attr('title', count + " new messages");
	  } else {
	    span.text(chats.total);
	    this.removeClass('chat-alert');
	    this.attr('title', chats.total + " old messages");
	  }
	} else {
	  empty();
	}
      }

      return this;
    },

    'chats++': function(docid) {
      var data = this.data(pluginName);

      if ( data.total != undefined ) data.total++; else data.total = 1;
      if ( data.count != undefined ) data.count++;
      if (      docid != undefined ) data.docid = docid;

      if ( data.total ) {
	this.chatbell('chats', {
	  total: data.total,
	  count: data.count
	});
      }

      return this;
    },

    /**
     * Update the chat bell.
     * @param {Object} [chats]
     * @param {Number} [chats.total]
     * @param {Number} [chats.count]
     * @param {Number} [chats.docid]
     */
    update: function(chats) {
      var data = this.data(pluginName);

      chats = chats||{};

      if ( chats.total != undefined &&
	   chats.count != undefined ) {
	this.chatbell('chats', chats);
      } else {
	var docid = chats.docid||data.docid;
	var after = preferences.getDocVal(docid, 'chatBar', 0);

			/* fetch if we want unread or we don't know total */
	if ( docid && (after || chats.total == undefined) ) {
	  var elem = $(this);

	  $.get(config.http.locations.chat_status,
		{ docid: docid,
		  after: after
		},
		function(chats) {
		  elem.chatbell('chats', chats);
		});
	} else if ( chats.total != undefined ) {
	  this.chatbell('chats', chats);
	}
      }

      return this;
    },

    /**
     * Sent by the chatroom if the user saw the last message.
     */
    read_until: function(docid, time) {
      return this.each(function() {
	var elem = $(this);
	var data = elem.data(pluginName);

	if ( data.docid == docid && data.total ) {
	  elem.chatbell('chats', {total: data.total, count:0});
	}
      });
    },

    /**
     * Handle an incomming chat message.  If the message is not from
     * myself, display as a short notification.
     */
    'chat-message': function(msg) {
      if ( msg.is_self == undefined )
	msg.is_self = this.chatroom('is_self', msg);

      this.chatbell('chats++');

      if ( !msg.is_self ) {
	var elem = this.chatroom('render', msg);
	var options = {
	  dom: elem
	};

	modal.notify(this, options);
      }
    }
  }; // methods

  /**
   * <Class description>
   *
   * @class chatbell
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.chatbell = function(method) {
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
