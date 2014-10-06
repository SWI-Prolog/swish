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
      $(".swish-event-receiver").trigger("preference",
					 { name: name,
					   value: value
					 });
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
