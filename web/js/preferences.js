/**
 * @fileOverview
 *
 * Manage persistent data such as preferences.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 */

define([],
       function($) {
  var hasLocalStore = (typeof(Storage) !== "undefined");

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
