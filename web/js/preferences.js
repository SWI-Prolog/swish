/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2016, VU University Amsterdam
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
 * Manage persistent data such as preferences.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define(["jquery"],
       function($) {
  var hasLocalStore = (typeof(Storage) !== "undefined");
  var defaults = {};
  var inform = {};

  var preferences = {
    /**
     * @returns {Boolean} indicating whether persistent storage is
     * supported.
     */
    persistent: function() {
      return hasLocalStore;
    },

    /**
     * Store that we do not want to see info dialogue with a given
     * identifier again.
     * @param {String} id
     */
    setNotAgain: function(id) {
      if ( hasLocalStore ) {
	var data = readNotAgain();

	if ( data.indexOf(id) < 0 ) {
	  data.push(id);
	  localStorage.setItem("notagain", JSON.stringify(data));
	}
      }
    },

    /**
     * @returns {Boolean} `true` if the user choose not to see this
     * dialogue again
     * @param {String} id identifier to test
     */
    notagain: function(id) {
      if ( hasLocalStore ) {
	var data = readNotAgain();
	return data.indexOf(id) >= 0;
      }
      return false;
    },

    /**
     * Broadcast the change of a preference.
     */
    broadcast: function(name, value) {
      var sel;

      if ( inform.name == undefined )
	sel = ".swish-event-receiver";
      else if ( inform.name == null )
	return;
      else
	sel = inform.name;

      $(sel).trigger("preference", { name: name, value: value });
    },

    /**
     * Set the value of a preference and broadcast it.
     * FIXME: we should only broadcast if the value has changed.
     * @param {String} name describes the name of the preference
     * @param {Any} value describes the value.  Values are stored
     * using JSON serialization.
     */
    setVal: function(name, value) {
      if ( hasLocalStore ) {
	localStorage.setItem(name, JSON.stringify(value));
      }
      this.broadcast(name, value);
    },

    /**
     * @param {String} name describes the name of the preference
     * @param {Any} value describes the default value.
     */
    setDefault: function(name, value) {
      defaults[name] = value;
    },

    /**
     * @param {String} name describes the name of the preference
     * @param {String} jQuery selector for elements to inform.  If
     * `null`, nobody is informed.
     */
    setInform: function(name, value) {
      inform[name] = value;
    },

    /**
     * @param {String} name describes the name of the preference
     */
    getVal: function(name) {
      if ( hasLocalStore ) {
	var str;

	if ( (str = localStorage.getItem(name)) ) {
	  value = JSON.parse(str);
	  return value;
	}
      }
      return defaults[name];
    },

    /**
     * Set a preference value for a document.
     */
    setDocVal: function(docid, name, value) {
      var prefs = preferences.getVal(docid)||{};
      prefs[name] = value;
      preferences.setVal(docid, prefs);
    },

    /**
     * Get a preference value for a document.
     */
    getDocVal: function(docid, name, def) {
      var prefs = preferences.getVal(docid)||{};
      return prefs[name] === undefined ? def : prefs[name];
    }
  }

  function readNotAgain() {
    var str = localStorage.getItem("notagain") || "[]";
    var notagain;

    try {
      data = JSON.parse(str);
      if ( typeof(data) != "object" )
	data = [];
    } catch(err) {
      data = [];
    }

    return data;
  }

  return preferences;
});
