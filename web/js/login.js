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

define([ "jquery", "modal", "config", "laconic" ],
       function($, modal, config) {

(function($) {
  var pluginName = 'login';

  /** @lends $.fn.login */
  var methods = {
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);

	elem.on("click", function(ev) {
	  if ( elem.hasClass("login") )
	    elem.login('login', ev);
	  else
	    elem.login('logout', ev);
	  ev.preventDefault();
	  return false;
	});
	elem.hover(function(ev) { elem.login('details', true); },
		   function(ev) { elem.login('details', false); });

	elem.login('update');
      });
    },

    /**
     * Update the status of the login element
     */
    update: function() {
      var elem = $(this);
      $.get(config.http.locations.user_info, {},
	    function(obj) {
	      if ( obj ) {
		config.swish.user = obj;
		elem.removeClass("login").addClass("logout");

		var span = elem.find("span.logout span.value");

		if ( obj.picture ) {
		  span.html("")
		      .append($.el.img({ class: "profile-picture",
					 src: obj.picture
				       }));
		} else {
		  span.html("Logout");
		}
	      } else
	      { delete config.swish.user;
		elem.removeClass("logout").addClass("login");
		$("#login-details").remove();
	      }
	    },
	    "json");
    },

    details: function(show) {
      var elem = $(this);
      var user = config.swish.user;

      if ( show && user ) {
	var offset = elem.offset();
	var table  = $.el.table();

	function add(field, label) {
	  if ( user[field] ) {
	    $(table).append($.el.tr($.el.th(label+":"), $.el.td(user[field])));
	  }
	}

	add("user",  "User");
	add("name",  "Name");
	add("group", "Group");
	add("email", "Email");

	var modal = $.el.div({id: "login-details"},
			     $.el.div({class:"login-caption"}, "Current user"),
			     table);

	$("body").append(modal);
	$(modal).offset({ left: offset.left + elem.outerWidth()
					    - $(modal).outerWidth() - 5,
			  top:  offset.top  + elem.outerHeight()
			});
      } else if ( !show ) {
	$("#login-details").remove();
      }
    },

    /**
     * Perform the login
     */
    login: function(ev) {
      var elem   = $(this);
      var target = $(ev.target);
      var url    = target.closest("a").attr("href");
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
      this.login('update');
    },

    /**
     * Logout the current user
     */
    logout: function(ev) {
      var user = config.swish.user;
      var elem = $(this);

      if ( user ) {
	if ( user.logout_url ) {
	  $.ajax({ url: user.logout_url,
	           success: function() {
		     elem.login('update');
		   },
		   error: function(jqXHDR) {
		     modal.ajaxError(jqXHR);
		   }
	         });
	} else if ( user.auth_method == "basic" ||
		    user.auth_method == "digest" ) {
	  clearAuthenticationCache(config.http.locations.http_logout,
				   config.swish.user.auth_method,
				   function() {
				     elem.login('update');
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
