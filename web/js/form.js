/**
 * @fileOverview
 *
 * RequireJS module providing some general support methods for handling
 * forms and functions to build Bootstrap forms easily.
 *
 * @version 0.1.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "laconic", "tagmanager" ], function($) {
  var form = {
    /**
     * Serialize a form as an object. Form fields that have an empty
     * string are ignored and the value from a `<input type="checkbox">`
     * is converted into a JavaScript boolean.
     * @returns {Object} holding the name/value pairs of the form
     */
    serializeAsObject: function(form) {
      var arr = form.serializeArray(0);
      var obj = {};

      for(var i=0; i<arr.length; i++) {
	var value = arr[i].value;

	if ( value != "" ) {
	  if ( form.find('[name="'+arr[i].name+'"]').prop("type") == "checkbox" ) {
	    value = value == "on" ? true : false;
	  }
	  obj[arr[i].name] = value;
	}
      }

      console.log(obj);

      return obj;
    },

    showDialog: function(data) {
      $(".swish-event-receiver").trigger("dialog", data);
    },

    /**
     * Invoke the central broadcasting of SWISH
     * @param {String} event is the event name
     * @param {any} [data] is the associated data
     */
    formBroadcast: function(event, data) {
      $(".swish-event-receiver").trigger(event, data);
    },

    fields: {
      fileName: function(name, public) {
	var elem =
	$.el.div({class:"form-group"},
		 label("name", "Public name"),
		 $.el.div({class:"col-xs-10"},
			  $.el.div({class:"input-group"},
				   $.el.span({class:"input-group-addon"},
					     checkbox("public", public)),
				   textInput("name", "Name", name)),
			  helpBlock("Make saved file public and give it a meaningful name")));
	return elem;
      },

      title: function(title) {
	var elem =
	$.el.div({class:"form-group"},
		 label("title", "Title"),
		 $.el.div({class:"col-xs-10"},
			  textInput("title", "Title", title)));
	return elem;
      },

      description: function(description) {
	var elem =
	$.el.div({class:"form-group"},
		 label("description", "Description"),
		 $.el.div({class:"col-xs-10"},
			  textarea("description", description)));
	return elem;
      },

      tags: function(tags) {
	var elem =
	$.el.div({class:"form-group"},
		 label("tags", "Tags"),
		 $.el.div({class:"col-xs-10"},
			  tagInput("tags", "Tag", tags)));
	return elem;
      },

      buttons: function() {
	var elem =
	$.el.div({class:"form-group"},
		 $.el.div({class:"col-xs-offset-2 col-xs-10"},
			  $.el.button({name:"save",
				       class:"btn btn-primary"},
				      "Save program"),
			  $.el.button({name:"cancel",
				       class:"btn btn-danger",
				       'data-dismiss':"modal"},
				      "Cancel")));
	return elem;
      }
    }
  };

  function label(elemName, text) {
    return $.el.label({class:"control-label col-xs-2", for:elemName}, text);
  }

  function checkbox(name, checked) {
    var attrs = {name:name, type:"checkbox"};
    if ( checked ) attrs.checked = "checked";
    return $.el.input(attrs);
  }

  function textInput(name, placeholder, value) {
    var attrs = {name:name, type:"text", class:"form-control"};
    if ( placeholder ) attrs.placeholder = placeholder;
    if ( value )       attrs.value       = value;
    return $.el.input(attrs);
  }

  function tagInput(name, placeholder, tags) {
    var attrs = { name:name, type:"text",
                  class:"tm-input"
                };
    if ( placeholder ) attrs.placeholder = placeholder;
    var elem = $.el.input(attrs);
    if ( tags )
      $(elem).data("prefilled", tags);
    return elem;
  }

  function helpBlock(help) {
    return $.el.p({class:"help-block"},
		  "Make saved file public and give it a meaningful name");
  }

  function textarea(name, text) {
    return $.el.textarea({name:name, class:"form-control"}, text||"");
  }

  return form;
});
