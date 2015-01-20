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
     * Serialize a form as an object. The following normalizations are
     * performed:
     *   - Form fields that have an empty string are ignored
     *   - The value from a `<input type="checkbox">`is converted
     *     into a JavaScript boolean.
     *	 - The value of a tag-list is converted into a list of strings.
     * @returns {Object} holding the name/value pairs of the form
     */
    serializeAsObject: function(form) {
      var arr = form.serializeArray(0);
      var obj = {};

      for(var i=0; i<arr.length; i++) {
	var name  = arr[i].name;
	var value = arr[i].value;
	var input = form.find('[name="'+name+'"]');
	var type  = input.prop("type");

	if ( value != "" ) {
	  // deal with tag lists
	  if ( type == "hidden" && name.indexOf("hidden-") == 0 ) {
	    name = name.slice("hidden-".length);
	    if ( obj[name] == undefined ) {
	      obj[name] = value.split(",");
	    } else {
	      obj[name] = value.split(",").concat(obj[name]);
	    }
	  } else if ( type == "text" && input.hasClass("tag-list") ) {
	    if ( value != "" ) {
	      if ( obj[name] !== undefined )
		obj[name].push(value);
	      else
		obj[name] = [value];
	    }
	  } else if ( type == "checkbox" ) {
	    obj[name] = (value == "on" ? true : false);
	  } else {
	    obj[name] = value;
	  }
	}
      }

      // unchecked checkboxes are not reported
      form.find("[type=checkbox]").each(function() {
	var checkbox = $(this);
	var name = checkbox.prop('name');
	if ( obj[name] === undefined )
	  obj[name] = false;
      });

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
      fileName: function(name, public, disabled) {
	var elem =
	$.el.div({class:"form-group"},
		 label("name", "Public | name"),
		 $.el.div({class:"col-xs-10"},
			  $.el.div({class:"input-group"},
				   $.el.span({class:"input-group-addon",
				              title:"If checked, other users can find this program"
				             },
					     checkbox("public",
						      { checked: public
						      })),
				   textInput("name",
					     {placeholder:"Name (leave empty for generated random name)",
					      title:"Public name of your program",
					      value:name,
					      disabled:disabled})),
			  helpBlock("Make saved file public or give it a meaningful name")));
	return elem;
      },

      title: function(title) {
	var elem =
	$.el.div({class:"form-group"},
		 label("title", "Title"),
		 $.el.div({class:"col-xs-10"},
			  textInput("title",
				    {placeholder:"Descriptive title",
				     value:title})));
	return elem;
      },

      author: function(author) {
	var elem =
	$.el.div({class:"form-group"},
		 label("author", "Author"),
		 $.el.div({class:"col-xs-10"},
			  textInput("author",
				    {placeholder:"Your name", value:author})));
	return elem;
      },

      date: function(stamp, labels, name) {
	name = name||label;
	var elem =
	$.el.div({class:"form-group"},
		 label(name, labels),
		 $.el.div({class:"col-xs-10"},
			  textInput(name,
				    {disabled: true,
				     value:new Date(stamp*1000).toLocaleString()
				    })));
	return elem;
      },

      description: function(description) {
	var elem =
	$.el.div({class:"form-group"},
		 label("description", "Description"),
		 $.el.div({class:"col-xs-10"},
			  textarea("description", {value:description})));
	return elem;
      },

      commit_message: function(msg) {
	var elem =
	$.el.div({class:"form-group"},
		 label("commit_message", "Changes"),
		 $.el.div({class:"col-xs-10"},
			  textarea("commit_message",
				   { value:msg,
				     placeholder:"Describe your changes here"
				   })));
	return elem;
      },

      tags: function(tags) {
	var elem =
	$.el.div({class:"form-group"},
		 label("tags", "Tags"),
		 $.el.div({class:"col-xs-10"},
			  tagInput("tags", "Tags help finding this code", tags)));
	return elem;
      },

      /**
       * @param {Object} options
       * @param {String} options.label is the label used for the
       * primary button.
       * @param {Function} options.action is called with two arguments,
       * the _event_ and the serialized data from the embedded form
       */
      buttons: function(options) {
	options    = options||{};
	var label  = options.label||"Save program";
	var button = $.el.button({ name:"save",
				   class:"btn btn-primary"
				 },
				 label);

	$(button).on("click", function(ev) {
	  var elem = $(ev.target).parents("form")[0];
	  var data = form.serializeAsObject($(elem));

	  options.action(ev, data);
	  $(ev.target).parents(".modal").modal('hide');
	  ev.preventDefault();
	  return false;
	});

	var elem =
	$.el.div({class:"form-group"},
		 $.el.div({class:"col-xs-offset-2 col-xs-10"},
			  button,
			  $.el.button({name:"cancel",
				       class:"btn btn-danger",
				       'data-dismiss':"modal"},
				      "Cancel")));
	return elem;
      }
    },

    widgets: {
      glyphIconButton: function(glyph, options) {
	var attrs = {class:"btn btn-info", type:"button"};

	if ( options.action ) attrs['data-action'] = options.action;
	if ( options.title )  attrs.title          = options.title;

	elem =
	$.el.button(attrs,
		    $.el.span({class:"glyphicon "+glyph}));
	return elem;
      }
    }
  };

  function label(elemName, text) {
    return $.el.label({class:"control-label col-xs-2", for:elemName}, text);
  }

  function checkbox(name, options) {
    var attrs = {name:name, type:"checkbox"};
    options = options||{};
    if ( options.checked ) attrs.checked = "checked";
    if ( options.title   ) attrs.title	 = options.title;
    return $.el.input(attrs);
  }

  function textInput(name, options) {
    var attrs = {name:name, type:"text", class:"form-control"};
    options = options||{};
    if ( options.placeholder ) attrs.placeholder = options.placeholder;
    if ( options.title )       attrs.title       = options.title;
    if ( options.value )       attrs.value       = options.value;
    if ( options.disabled )    attrs.disabled    = options.disabled;
    return $.el.input(attrs);
  }

  function tagInput(name, placeholder, tags) {
    var attrs = { name:name, type:"text",
                  class:"tm-input tag-list"
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

  function textarea(name, options) {
    var attrs = {name:name, class:"form-control"};
    options = options||{};

    if ( options.placeholder ) attrs.placeholder = options.placeholder;

    return $.el.textarea(attrs, options.value||"");
  }

  return form;
});
