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
 *
 * Small utilities
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define(["jquery"],
       function($) {

  var utils = {
    /**
     * @param   {String} text is the text to be encoded
     * @returns {String} HTML encoded version of text
     */
    htmlEncode: function(text) {
      if ( !text ) return "";
      return document.createElement('a')
                     .appendChild(document.createTextNode(text))
		     .parentNode
		     .innerHTML;
    },

    /**
     * @returns {String} (random) UUID
     */
    generateUUID: function() {
      var d = new Date().getTime();
      var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
	.replace(/[xy]/g, function(c) {
	  var r = (d + Math.random()*16)%16 | 0;
	  d = Math.floor(d/16);
	  return (c=='x' ? r : (r&0x7|0x8)).toString(16);
	});
      return uuid;
    },

    flash: function(obj) {
      obj.addClass("flash");
      setTimeout(function() { obj.removeClass("flash"); }, 1500);
    },

    ago: function(time) {
      var ago = ((new Date().getTime())/1000) - time;

      if ( ago < 20  ) return "just now";
      if ( ago < 60  ) return "less then a minute ago";
      ago = Math.round(ago/60);
      if ( ago < 120 ) return ago + " minutes ago";
      ago = Math.round(ago/60);
      if ( ago < 48 )  return ago + " hours ago";
      ago = Math.round(ago/24);
      if ( ago < 360 ) return ago + " days ago";
      ago = Math.round(ago/365);
      return ago + " years ago";
    },

    basename: function(path) {
      return path ? path.split('/').pop() : null;
    }
  } // end of methods

  if (typeof String.prototype.startsWith != 'function') {
    String.prototype.startsWith = function(str) {
      return this.lastIndexOf(str, 0) === 0;
    };
  }

  return utils;
});
