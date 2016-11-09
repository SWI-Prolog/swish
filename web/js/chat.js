/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, VU University Amsterdam
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
 * Deal with cooperation
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config" ],
       function($, config) {

(function($) {
  var pluginName = 'chat';

  /** @lends $.fn.chat */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};			/* private data */

	elem.data(pluginName, data);	/* store with element */

	if ( config.swish.chat ) {
	  elem.chat('connect');
	}

	elem.on("click", function(ev) {
	  var li = $(ev.target).closest("li.user");

	  if ( li.length == 1 )
	    elem.chat('unnotify', li.attr("id"));
	});
	elem.on("send", function(ev, msg) {
	  elem.chat('send', msg);
	})
      });
    },

		 /*******************************
		 *	      WEBSOCKET		*
		 *******************************/

    /**
     * Create a websocket connection to /chat on the SWISH server.
     */
    connect: function() {
      var elem = this;
      var data = this.data(pluginName);
      var url  = window.location.host + config.http.locations.swish_chat;

      data.connection = new WebSocket("ws://" + url, ['chat']);
      data.connection.onerror = function(error) {
	console.log('WebSocket Error ' + error);
      }

      data.connection.onmessage = function(e) {
	var msg = JSON.parse(e.data);
	msg.origin = e.origin;
	if ( msg.type )
	  elem.chat(msg.type, msg);
	else
	  console.log(e);
      }
    },


		 /*******************************
		 *	   BASIC MESSAGES	*
		 *******************************/

    /**
     * @param {Object} msg is the JSON object to broadcast
     */
    send: function(msg) {
      var data = $(this).data(pluginName);

      if ( data && data.connection )
	data.connection.send(JSON.stringify(msg));

      return this;
    },

    subscribe: function(channel, sub_channel) {
      var msg = { type: "subscribe", channel: channel };

      if ( sub_channel )
	msg.sub_channel = sub_channel;

      this.chat('send', msg);
    },

    unsubscribe: function(channel, subchannel) {
      var msg = { type: "unsubscribe", channel: channel };

      if ( sub_channel )
	msg.sub_channel = sub_channel;

      this.chat('send', msg);
    },

		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

    /**
     * The welcome message is sent by SWISH immediately after opening
     * the websocket connection.  It provides the session UID for this
     * user
     */
    welcome: function(e) {
      if ( !e.name )
	e.name = "Me";
      this.chat('addUser', e.uid, e);
    },

    /**
     * Display a notification by some user.
     */
    notify: function(e) {
      this.chat('notifyUser', e.uid, e);
    },

    /**
     * A user has left
     */
    left: function(e) {
      $("#"+e.uid).hide(400, function() {this.remove();});
    },

		 /*******************************
		 *	        UI		*
		 *******************************/

    /**
     * Present a notification associated with a user
     *
     * @param {Oject} options
     * @param {String} options.html provides the inner html of the message.
     * @param {Number} [options.fadeIn=400] provide the fade in time.
     * @param {Number} [options.fadeOut=400] provide the fade out time.
     * @param {Number} [options.time=5000] provide the show time.  The
     * value `0` prevents a timeout.
     */
    notifyUser: function(uid, options) {
      var elm  = $("#"+uid);

      if ( elm.length == 0 ) {
	this.chat('addUser', uid, options);
	elm = $("#"+uid);
      }

      if ( elm.length > 0 ) {
	var div  = $.el.div({ class:"notification notify-arrow",
			      id:"ntf-"+uid
			    });
	var epos = elm.offset();

	$("body").append(div);
	$(div).html(options.html)
	      .css({ left: epos.left+elm.width()-$(div).outerWidth()-5,
		     top:  epos.top+elm.height()+5
		   })
	      .on("click", function(){$(div).remove();})
	      .show(options.fadeIn||400);
	if ( options.time !== 0 ) {
	  setTimeout(function() {
	    $(div).hide(options.fadeOut||400, function(){this.remove();});
	  }, options.time||5000);
	}
      }
    },

    unnotify: function(uid) {
      $("#ntf-"+uid).remove();
      return this;
    },

    /**
     * Add a new user to the notification area
     * @param {String} uid is the identifier of the user
     * @param {Object} [options]
     * @param {String} [options.name] is the name of the user
     */
    addUser: function(uid, options) {
      return this.append(li_user(uid, options));
    }

  }; // methods

  /**
   * Add an entry for a user to the notification area
   */
  function li_user(id, options) {
    options = options||{};

    if ( !options.name )
      options.name = id;

    function avatar(options) {
      if ( options.avatar ) {
	return $.el.img({class:"avatar", src:options.avatar, alt:options.name});
      } else {
	return $.el.span({class:"avatar glyphicon glyphicon-user"})
      }
    }

    var li = $.el.li({class:"dropdown user", id:id},
		     $.el.a({ class:"dropdown-toggle avatar",
			      'data-toggle':"dropdown",
			    },
			    avatar(options),
			    $.el.b({class:"caret"})),
		     $.el.ul({ class:"dropdown-menu pull-right" },
			     $.el.li(options.name)));

    return li;
  }


  /**
   * <Class description>
   *
   * @class chat
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.chat = function(method) {
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
