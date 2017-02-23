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

define([ "jquery", "laconic" ],
       function() {

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

	elem.addClass("chatroom");
	elem.append($.el.div({class:"chat-conversation"}),
	    close = $.el.span({class:"glyphicon glyphicon-remove-circle"}),
		    $.el.div({class:"chat-input"},
			     $.el.textarea({ class:"chat-input",
					     placeholder:"Chat"
					   }),
		       btn = $.el.button({class:"btn btn-primary btn-xs"}, "Send")));

	$(btn).on("click", function(ev) {
	  elem.chatroom('send');
	});
	$(close).on("click", function() {
	  elem.tile('close');
	});
	elem.data(pluginName, data);	/* store with element */
      });
    },

    send: function() {
      var msg = {};
      var ta = this.find("textarea");
      msg.text = ta.val();
      ta.val("");
      this.chatroom('add', msg);
    },

    /**
     * Add a chat object to the conversation.
     * @param {Object} options
     * @param {String} options.html is the HTML content of the object
     * @param {String} options.text is the ext of the object
     * @param {String} options.user is the user (wsid) id of the sender
     */
    add: function(options) {
      elem = $.el.div({class:"chat-message"}, options.text||"");

      if ( options.html )
	$(elem).html(options.html);
      this.find(".chat-conversation").append(elem);

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
