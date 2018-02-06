/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2017, VU University Amsterdam
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
 * RequireJS module providing some general support methods for handling
 * forms and functions to build Bootstrap forms easily.
 *
 * @version 0.1.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "modal", "laconic", "tagmanager" ],
       function($, config, modal) {

  var LABELWIDTH = 3;

  var form = {
    /**
     * Serialize a form as an object. The following normalizations are
     * performed:
     *   - Form fields that have an empty string are ignored unless
     *     `ignore_empty` is `true`
     *   - The value from a `<input type="checkbox">`is converted
     *     into a JavaScript boolean.
     *	 - The value of a tag-list is converted into a list of strings.
     * @returns {Object} holding the name/value pairs of the form
     */
    serializeAsObject: function(form, ignore_empty) {
      var arr = form.serializeArray();
      var inset = [];
      var obj = {};

      // get arrays of checkboxes
      form.find("div.checkboxes.array").each(function() {
	var elem = $(this);
	var set = [];

	elem.find("input:checked").each(function() {
	  var name = $(this).attr("name");
	  set.push(name);
	});
	elem.find("input").each(function() {
	  var name = $(this).attr("name");
	  inset.push(name);
	});

	obj[elem.attr("name")] = set;
      });

      for(var i=0; i<arr.length; i++) {
	var name  = arr[i].name;
	var value = arr[i].value;
	var input = form.find('[name="'+name+'"]');
	var type  = input.prop("type");
	var jvalue;

	if ( (jvalue = input.data('json-value')) ) {
	  obj[name] = jvalue;
	} else if ( value != "" || ignore_empty == true ) {
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
	  } else if ( type == "number" ) {
	    obj[name] = parseInt(value);
	  } else if ( type == "checkbox" ) {
	    if ( inset.indexOf(name) == -1 )
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
	if ( checkbox.prop("disabled") != true &&
	     obj[name] === undefined &&
	     inset.indexOf(name) == -1 )
	  obj[name] = false;
      });

      return obj;
    },

    /**
     * Provide feedback about problems with form elements
     * @param form is the form to decorate
     * @param error is a pengine error message created by lib/form.pl
     */

    formError: function(formel, error) {
      formel.find(".has-error").removeClass("has-error");
      formel.find(".help-block.with-errors").remove();

      if ( error ) {
	if ( error.code == "form_error" || error.code == "input_error" ) {
	  errors = error.data.split("\n");
	  for(var i=0; i<errors.length; i++) {
	    var el = errors[i].split(/:\s*(.*)?/);

	    form.fieldError(formel, el[0], el[1]);
	  }
	} else
	{ modal.alert(error.data);
	}
      }
    },

    fieldError: function(form, field, msg) {
      var input = form.find("input[name="+field+"]");

      if ( input.length > 0 ) {
	var group = input.closest(".form-group");

	if ( input.parent().hasClass("input-group") )
	  input = input.parent();

	group.addClass("has-error");
	input.after($.el.p({class:"help-block with-errors"}, msg));
      } else
      { alert("Missing value for "+field);
      }
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

    dyn_clear: function(form, onclear) {
      form.find('.has-clear input[type="text"]').on('input propertychange',
						    function() {
	var $this = $(this);
	var visible = Boolean($this.val());
	$this.siblings('.form-control-clear').toggleClass('hidden', !visible);
      }).trigger('propertychange');

      form.find('.form-control-clear').click(function() {
	var input = $(this).siblings('input[type="text"]');
	input.val('').trigger('propertychange').focus();
	if ( onclear )
	  onclear.call(input);
      });
    },

    fields: {
      fileName: function(name, public, example, disabled) {
	var labeltext;
	var empty = "(leave empty for generated random name)"
	var fork, input;
	var community_examples = config.swish.community_examples && example != undefined;

	if ( community_examples )
	  labeltext = "Public | Example | name";
	else
	  labeltext = "Public | name";

        var elem =
	$.el.div({class:"form-group"},
		 label("name", labeltext),
		 $.el.div({class:valgridw()},
			  $.el.div({class:"input-group"},
				   $.el.span({class:"input-group-addon",
				              title:"If checked, other users can find this program"
				             },
					     checkbox("public",
						      { checked: public
						      })),
				   community_examples ?
				   $.el.span({class:"input-group-addon",
				              title:"If checked, add to examples menu"
				             },
					     checkbox("example",
						      { checked: example
						      })) : undefined,
			   input = textInput("name",
					     {placeholder:"Name " + empty,
					      title:"Public name of your program",
					      value:name,
					      disabled:disabled}),
			   name ?
			     fork = $.el.span({class:"input-group-btn"
					      },
					      $.el.button({ class: "btn btn-success",
							    type: "button"
							  }, "Fork")) : undefined
				  )));

	if ( fork ) {
	  $(fork).on("click", function() {
	    var btn = $(input).closest("form").find(".btn.btn-primary");
	    $(input).attr("placeholder", "Fork as " + empty);
	    $(input).val("");
	    btn.text(btn.text().replace("Update", "Fork"));
	  });
	}

	return elem;
      },

      title: function(title) {
	var elem =
	$.el.div({class:"form-group"},
		 label("title", "Title"),
		 $.el.div({class:valgridw()},
			  textInput("title",
				    {placeholder:"Descriptive title",
				     value:title})));
	return elem;
      },

      /**
       * @param {String} [identity] if provided, this indicates that the
       * author cannot be changed.
       */
      author: function(author, identity) {
	var options = { placeholder:"Your name", value:author };

	if ( author && identity ) {
	  options.readonly = true;
	  options.title    = "Verified author name";
	}

	var elem =
	$.el.div({class:"form-group"},
		 label("author", "Author"),
		 $.el.div({class:valgridw()},
			  textInput("author", options)));
	return elem;
      },

      link: function(link) {
	var options = {
	  readonly: true,
	  title: "Permalink",
	  value: link
	};
	var elem =
	$.el.div({class:"form-group"},
		 label("link", "Link"),
		 $.el.div({class:valgridw()},
			  textInput("link", options)));
	return elem;
      },

      date: function(stamp, labels, name) {
	name = name||label;
	var elem =
	$.el.div({class:"form-group"},
		 label(name, labels),
		 $.el.div({class:valgridw()},
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
		 $.el.div({class:valgridw()},
			  textarea("description", {value:description})));
	return elem;
      },

      commit_message: function(msg) {
	var elem =
	$.el.div({class:"form-group"},
		 label("commit_message", "Changes"),
		 $.el.div({class:valgridw()},
			  textarea("commit_message",
				   { value:msg,
				     placeholder:"Describe your changes here"
				   })));
	return elem;
      },

      description: function(msg) {
	var elem =
	$.el.div({class:"form-group"},
		 label("description", "Description"),
		 $.el.div({class:valgridw()},
			  textarea("description",
				   { value:msg,
				     placeholder:"Description"
				   })));
	return elem;
      },

      tags: function(tags) {
	var elem =
	$.el.div({class:"form-group"},
		 label("tags", "Tags"),
		 $.el.div({class:valgridw()},
			  tagInput("tags", "Tags help finding this code", tags)));
	return elem;
      },

      /**
       * Provide checkboxes for determining who may save a new version
       * of this file
       */
      modify: function(who, canmodify) {
	var fields = [];
	var opts = { name:"modify", label:"Can save new version",
		     type:"array"
		   };

	function add(key, label) {
	  fields.push({ name:key,
			label:label,
			value:who.indexOf(key) != -1,
			readonly: !canmodify
		      });
	}

	add("any",   "Anyone");
	add("login", "Logged in users");
	add("owner", "Only me");

	if ( !canmodify )
	  opts.title = "Only logged in users and owners can set permissions";
	else
	  opts.title = "Specify who can save an updated version of this file";

	return form.fields.checkboxes(fields, opts);
      },

      follow: function(email) {
	return form.fields.checkboxes(
		 [ { name: "follow", label: "Follow this document",
		     value:!!email, readonly:!email
		   }
		 ],
		 { name:"options", label:"",
		   title: "Notify about activity (updates, chat)\n"+
			  "Requires being logged in with valid email"
		 });
      },

      projection: function(projection) {
	var elem =
	$.el.div({class:"form-group"},
		 label("projection", "Projection"),
		 $.el.div({class:valgridw()},
			  textInput("projection",
				    {placeholder:"Columns", value:projection})));
	return elem;
      },

      csvFormat: function(list, format) {
	var elem;

	list = list||["prolog"];
	format = format||list[0];

	if ( list.length == 1 ) {
	  elem = $.el.input({type:"hidden", name:"format", value:list[0]});
	} else {
	  elem = $.el.div({class:"form-group"},
			  label("format", "Format"),
			  $.el.div({class:valgridw()},
				   select("format",
					  list,
					  {value:format})));
	}

	return elem;
      },

      /**
       * Ask for limit and distinct to modify the solution set.
       * @param {Number} [limit] is the max number of solutions to
       * return
       * @param {Boolean} [distinct] requests only to return distinct
       * solutions.
       */
      limit: function(limit, distinct) {
	var elem =
	$.el.div({class:"form-group"},
		 label("name", "Distinct | limit"),
		 $.el.div({class:valgridw()},
			  $.el.div({class:"input-group"},
				   $.el.span({class:"input-group-addon",
				              title:"If checked only return distinct results"
				             },
					     checkbox("distinct",
						      { checked: distinct
						      })),
				   textInput("limit",
					     {placeholder:"Maximum result count (blank for unlimited)",
					      title:"Limit results",
					      value:limit}))));
	return elem;
      },

      /**
       * @param {Array} boxes is a list of checkbox specifications.
       * Uses .name, .label, .value (Boolean) and .readonly
       */
      checkboxes: function(boxes, options) {
	var boxel;

	options = $.extend({name:"options", label:"Options", col:LABELWIDTH},
			   options||{});

	var dopts = { class: "checkboxes col-xs-"+(12-options.col),
	              name:  options.name
		    };
	if ( options.title ) dopts.title = options.title;
	if ( options.type  ) dopts.class += " "+options.type;
	var elem =
	$.el.div({class:"form-group"},
		 label(options.name, options.label, options.col),
		 boxel = $.el.div(dopts));

	for(var k=0; k<boxes.length; k++) {
	  var box = boxes[k];
	  var opts = {type: "checkbox", name:box.name, autocomplete:"false"};
	  if ( box.value )
	    opts.checked = "checked";
	  if ( box.readonly )
	    opts.disabled = "disabled";
	  $(boxel).append($.el.label({class:"checkbox-inline"},
				     $.el.input(opts), box.label));
	}

	return elem;
      },

      chunk: function(value) {
	var elem =
	$.el.div({class:"form-group"},
		 label("count", "Initial solutions", 3),
		 $.el.div({class:"col-xs-9"},
			  $.el.div({class:"input-group"},
				   textInput("chunk",
					     { title:"Initial number of solutions",
					       type:"number",
					       value:value}))));
	return elem;
      },

      name: function(name, col) {
	col = col||3;
	var elem =
	$.el.div({class:"form-group"},
		 label("name", "Name", col),
		 $.el.div({class:"col-xs-"+(12-col)},
			  textInput("name",
				    {placeholder:"Name",
				     value:name})));
	return elem;
      },

      filename: function(name, col) {
	col = col||3;
	var elem =
	$.el.div({class:"form-group"},
		 label("filename", "File name", col),
		 $.el.div({class:"col-xs-"+(12-col)},
			  textInput("filename",
				    {placeholder:"File name",
				     value:name})));
	return elem;
      },

      hidden: function(name, value) {
	if ( value !== undefined )
	  return $.el.input({type:"hidden", name:name, value:value});
      },

      /**
       * @param {Object} options
       * @param {String} options.label is the label used for the
       * primary button.
       * @param {Function} options.action is called with two arguments,
       * the _event_ and the serialized data from the embedded form
       * @param {Number} options.offset determines the begin column in
       * the grid (default 2)
       */
      buttons: function(options) {
	options    = options||{};
	var label  = options.label||"Save program";
	var offset = options.offset||LABELWIDTH;
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
		 $.el.div({class:"col-xs-offset-"+offset+" col-xs-"+(12-offset)},
			  button,
			  $.el.button({name:"cancel",
				       class:"btn btn-danger",
				       'data-dismiss':"modal"},
				      "Cancel")));
	return elem;
      },

      /**
       * Bootstrap radio button.  To get the value, use
       * `$("label.active > input[name=Name]").val();
       * @param {String} name is the name of the radio button
       * @param {Array(Object)} buttons is an array of objects with
       * .active, .label and .value
       */
      radio: function(name, buttons, type) {
	var elem = $.el.div({class:"btn-group", "data-toggle":"buttons"});
	type = type||"radio"

	for(var i=0; i<buttons.length; i++) {
	  var cls = "btn btn-default btn-xs";
	  if ( buttons[i].active )
	    cls += " active";

	  var opts = { type:type, name:name,
	               autocomplete:"off",
		       value:buttons[i].value
		     };
	  var lblopts = {class:cls};
	  if ( buttons[i].title )
	    lblopts.title = buttons[i].title;
	  $(elem).append($.el.label(lblopts,
				    $.el.input(opts),
				    buttons[i].label));
	}

        return elem;
      }
    },

    widgets: {
      glyphIcon: function(glyph) {
	return $.el.span({class:"glyphicon glyphicon-"+glyph});
      },

      typeIcon: function(type) {
	return $.el.span({class:"dropdown-icon type-icon "+type});
      },

      glyphIconButton: function(glyph, options) {
	var attrs = {class:"btn", type:"button"};

	if ( options.action ) attrs['data-action'] = options.action;
	if ( options.title )  attrs.title          = options.title;
	if ( options.class )  attrs.class	  += " "+options.class;

	return $.el.button(attrs, form.widgets.glyphIcon(glyph));
      },

      /**
       * Turn an icon into a dropdown button.
       * @param {Object} options
       * @param {Any}	 options.client is the `this` for the menu
       *		 functions.
       * @param {String} [options.divClass] additional class for the
       * returned `div` element
       * @param {String} [options.ulClass] additional class for the
       * `ul` element that defines the menu.
       * @param {Object} [options.actions] defines the menu items.
       * this is passed to populateMenu()
       * @returns {DIV} the downdown button
       */
      dropdownButton: function(icon, options) {
	if ( !options ) options = {};
	var cls     = options.divClass;
	var ulClass = options.ulClass;

	var dropdown = $.el.div(
	  {class: "btn-group dropdown"+(cls?" "+cls:"")},
	  $.el.button(
	    {class:"dropdown-toggle",
	     "data-toggle":"dropdown"},
	    icon),
	  $.el.ul({class:"dropdown-menu"+(ulClass?" "+ulClass:"")}));

	if ( options.actions )
	  form.widgets.populateMenu($(dropdown), options.client, options.actions);

	return dropdown;
      },

      populateMenu: function(menu, client, actions) {
	var ul = menu.find(".dropdown-menu");
	var data = ul.data("menu")||{};

	function runMenu(ev, a) {
	  var action = $(a).data('action');

	  if ( action )
	    action.call(client, a);
	}

	function addMenuItem(label, onclick) {
	  if ( onclick !== undefined ) {
	    if ( label.indexOf("--") == 0 ) {
	      ul.append($.el.li({class:"divider"}));
	    } else {
	      var a = $.el.a(label);

	      $(a).data('action', onclick);
	      ul.append($.el.li(a));
	    }
	  }
	}

	for(var a in actions) {
	  if ( actions.hasOwnProperty(a) ) {
	    addMenuItem(a, actions[a]);
	  }
	}

	if ( !data.bound ) {
	  data.bound = true;
	  ul.on("click", "a", function(ev) { runMenu(ev, this); } );
	}

	ul.data("menu", data);

	return menu;
      }
    }
  };

		 /*******************************
		 *	     FUNCTIONS		*
		 *******************************/

  function valgridw(n) {
    if ( n === undefined ) n = LABELWIDTH;
    return "col-xs-"+(12-n);
  }
  function colgridw(n) {
    if ( n === undefined ) n = LABELWIDTH;
    return "col-xs-"+n;
  }

  function label(elemName, text, width) {
    width = width || LABELWIDTH;
    return $.el.label({class:"control-label col-xs-"+width+"", for:elemName}, text);
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
    if ( options.readonly )    attrs.readonly    = options.readonly;
    if ( options.type )        attrs.type        = options.type;
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

  /**
   * Create a bootstrap <select> element from a list of options
   * @param {String} name is the name of the select element
   * @param {Array} from is an array of options. Each options is a
   * string or an object with keys `value` and `label`.
   * @param {Object} [options]
   * @param {Object} [options.value] If provided, the corresponding
   * option is selected
   */

  function select(name, from, options) {
    var select = $($.el.select({class:"form-control", name:name}));

    options=options||{};

    function addSelect(e) {
      if ( typeof(e) == "string" ) {
	if ( e == options.value ) {
	  select.append($.el.option({selected:"selected"}, e));
	} else {
	  select.append($.el.option(e));
	}
      } else {
	var opts = {value:e.value};
	if ( e.value == options.value )
	  opts.selected = "selected";

	select.append($.el.option(opts, e.label));
      }
    }

    for(var i=0; i<from.length; i++)
      addSelect(from[i]);

    return select[0];
  }

  return form;
});
