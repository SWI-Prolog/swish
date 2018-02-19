/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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
 * Handle JavaScript based login
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "modal", "config", "form", "laconic" ],
       function($, modal, config, form) {

var DEFAULT_USER_FIELDS = ["display_name", "email", "avatar"];

(function($) {
  var pluginName = 'login';

  /** @lends $.fn.login */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var data = {};

	data.url = elem.attr("href");
	elem.removeAttr("href");

	elem.on("click", function(ev) {
	  if ( elem.hasClass("login") )
	    elem.login('login', ev);
	});

	elem.data(pluginName, data);
	elem.login('update', "startup");
      });
    },

    /**
     * Update the status of the login element
     */
    update: function(why) {
      var elem = $(this);
      $.get(config.http.locations.user_info, {reason:why},
	    function(obj) {
	      if ( obj ) {
		config.swish.user = obj;
		elem.removeClass("login").addClass("logout");

		var span = elem.find("span.logout span.value");
		var icon;

		if ( obj.avatar ) {
		  icon = $.el.img({ class: "profile-picture",
				    src: obj.avatar
				  });
		} else {
		  icon = $.el.span({class:"glyphicon glyphicon-user"});
		}
		icon = $.el.span(icon, $.el.b({class: "caret"}));
		span.html("");
		span.append(form.widgets.dropdownButton(icon, {
		  divClass:"user-menu btn-transparent",
		  ulClass:"pull-right",
		  client: elem,
		  actions: {
		    "Logout":  elem.hasClass('no-logout') ? undefined :
			       function() {
		      this.login('logout');
		    },
		    "Profile": function() {
		      this.login('profile');
		    }
		  }
		}));

		if ( why == "logout_by_http" )
		  modal.alert("Failed to logout from HTTP login.  Logout from HTTP "+
			      "is known not to work for Chrome.  For some browsers "+
			      "there is a plugin to logout.  Otherwise stopping your "+
			      "browser completely (all pages) and restarting is the "+
			      "only way to logout.  HTTP logout does work for FireFox "+
			      "and IE.");

	      } else
	      { delete config.swish.user;
		elem.removeClass("logout").addClass("login");
	      }
	      $(".sourcelist").trigger("login");
	    },
	    "json");
    },

    /**
     * Perform the login
     */
    login: function(ev) {
      var elem   = $(this);
      var data   = this.data(pluginName);
      var target = $(ev.target);
      var url    = data.url;
      var server = target.closest("[data-server]").data("server");
      var frame  = target.closest("[data-frame]").data("frame")||"iframe";

      if ( server )
	url += "?server="+encodeURIComponent(server);

      if ( frame == "popup" ) {
	openPopup(url, "_blank",
		  'location=true,status=true,height=400,width=800',
		  function() {
		    elem.login('logged_in');
		  });
      } else {
	modal.show({
	  title: "Login",
	  body: function() {
	    var button = $.el.button({ name:"ok",
				       class:"btn btn-primary login-cont",
				       "data-dismiss":"modal"
				     },
				     "Continue");
	    this.append($.el.iframe({class:"login", src:url}),
			button);
	  },
	  onclose: function() {
	    elem.login('logged_in');
	  }
	});
      }
    },

    /**
     * User closed the login modal window.  Check the login.
     */
    logged_in: function() {
      this.login('update', "login");
    },

    /**
     * Examine/edit the user profile.  Opens a modal window that is
     * filled through an AJAX call on the server.
     */
    profile: function() {
      var login = $(this);

      modal.server_form({
	title: "User profile",
	url: config.swish.user.swish_profile_url ||
	     config.http.locations.user_profile,
	onreply: function() { login.login('update', "profile"); }
      });
    },

    /**
     * Get information about the current user. If possible we get this
     * from the logged in user.  Else we get the name and avatar from
     * #chat.
     * @param [fields] is an array with fields we want to have
     * @return {Object} info about current user
     */
    get_profile: function(fields) {
      var info = {};
      var obj = config.swish.user||{};

      function set_from_chat(key, chat_key) {
	var chat = $("#chat");
	if ( chat.length == 1 ) {
	  var v = chat.chat('self', [chat_key]);
	  if ( v[chat_key] ) info[key] = v[chat_key];
	}
      }

      fields = fields||DEFAULT_USER_FIELDS;
      for(var i=0; i<fields.length; i++) {
	var key = fields[i];

	if ( obj[key] ) {
	  info[key] = obj[key];
	} else if ( key == 'display_name' ) {
	  if ( obj.name )
	    info.display_name = obj.name;
	  else if ( obj.given_name && obj.family_name )
	    info.display_name = obj.given_name + " " + obj.family_name;
	  else if ( obj.family_name )
	    info.display_name = obj.family_name;
	  else if ( obj.given_name )
	    info.display_name = obj.given_name;
	  else if ( obj.nick_name )
	    info.display_name = obj.nick_name;
	  else
	    set_from_chat('display_name', 'name');
	} else if ( key == 'identity' ) {
	  if ( obj.external_identity && obj.identity_provider )
	    info.identity = obj.identity_provider + ":" + obj.external_identity;
	} else if ( key == 'avatar' ) {
	  set_from_chat('avatar', 'avatar');
	}
      }

      return info;
    },

    /**
     * Logout the current user
     */
    logout: function() {
      var user = config.swish.user;
      var elem = $(this);

      if ( user ) {
	if ( user.logout_url ) {
	  $.ajax({ url: user.logout_url,
	           success: function() {
		     elem.login('update', "logout_by_url");
		   },
		   error: function(jqXHDR) {
		     modal.ajaxError(jqXHDR);
		   }
	         });
	} else if ( user.auth_method == "basic" ||
		    user.auth_method == "digest" ) {
	  clearAuthenticationCache(config.http.locations.http_logout,
				   config.swish.user.auth_method,
				   function() {
				     elem.login('update', "logout_by_http");
				   });
	} else {
	  alert("Don't know how to logout");
	}
      }
    }
  }; // methods

  /**
   * @see https://trac-hacks.org/wiki/TrueHttpLogoutPatch
   * @see http://stackoverflow.com/questions/233507/how-to-log-out-user-from-web-site-using-basic-authentication
   */
  function clearAuthenticationCache(page, method, oncomplete) {
    // Default to a non-existing page (give error 500).
    // An empty page is better, here.
    if (!page) page = '.force_logout';
    try{
      var agt=navigator.userAgent.toLowerCase();

      if ( agt.indexOf("msie") != -1 ) {
	document.execCommand("ClearAuthenticationCache");
      } else if ( agt.indexOf("webkit") != -1 && method == "basic" ) {
	var xmlhttp = createXMLObject(oncomplete);

	if ( xmlhttp ) {
	  xmlhttp.open("GET", page, true);
	  xmlhttp.setRequestHeader("Authorization", "Basic logout");
	  xmlhttp.send();
	}
      } else {
	var xmlhttp = createXMLObject(oncomplete);

	if ( xmlhttp ) {
	  xmlhttp.open("GET", page, true, "logout", "logout");
	  xmlhttp.send("");
	  xmlhttp.abort();
	}
      }
    } catch(e) {
      // There was an error
      return;
    }
  }

  function createXMLObject(oncomplete) {
    var xmlhttp;

    try {
      if (window.XMLHttpRequest) {
	xmlhttp = new XMLHttpRequest();
      } else if (window.ActiveXObject) {
	xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
      }

      if ( oncomplete ) {
	xmlhttp.addEventListener("load",  oncomplete);
	xmlhttp.addEventListener("error", oncomplete);
	xmlhttp.addEventListener("abort", oncomplete);
      }
    } catch (e) {
    }

    return xmlhttp;
  }

  /**
   * Open a popup window for dealing with the federated login.  We
   * must check the login status after the user completes the popup.
   * Unfortunately the code below does not always work as `win.closed`
   * is not always set.  An example is FF 51.0 using Cinamon.
   */
  function openPopup(uri, name, options, closeCallback) {
    var win = window.open(uri, name, options);
    var interval = window.setInterval(function() {
      try {
	if (win == null || win.closed) {
	  window.clearInterval(interval);
	  closeCallback(win);
	}
      }
      catch (e) {
      }
    }, 1000);

    if ( window.focus )
      win.focus();

    return win;
  };


  /**
   * <Class description>
   *
   * @class login
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.login = function(method) {
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
