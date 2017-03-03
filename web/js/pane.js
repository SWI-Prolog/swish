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
 * Deal with subwindow layout
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "splitter" ],
       function() {

(function($) {
  var pluginName = 'tile';

  /** @lends $.fn.tile */
  var methods = {
    /**
     * @param {Object} [options] currently ignored
     */
    _init: function(options) {
      return this.each(function() {
	var elem = $(this);
	var dir   = elem.hasClass("horizontal") ? "vertical" : "horizontal";
	var pos   = elem.attr("data-split");
	var panes = elem.children();

	pos = pos||"50%";

	panes.each(function() {
	  $(this).wrap('<div class="pane-wrapper"></div>')
	});
	elem.addClass("pane-container");
	elem.split({ orientation:dir,
	             position:pos,
		     limit:10,
		     onDragStart: function() { elem.tile('resize_start'); },
		     onDrag: function(ev) { panes.trigger("pane.resize"); },
		     onDragEnd: function() { elem.tile('resize_save'); }
	           });
	elem.tile('resize_save');

	elem.on('fullscreen', function(ev, val) {
	  elem.tile('resize');
	});
      });
    },

    resize_start: function() {
      return this.each(function() {
	var elem    = $(this);
	var info    = paneInfo(elem);

	elem.find(".reactive-size").trigger("reactive-resize-start",
					    info.splitter.orientation);
      });
    },

    /**
     * Save the current split location as a percentage, so we can
     * maintain this percentage at subsequent resize events.  This
     * is normally called after establishing the tile and after a
     * user-initiated resize.
     */
    resize_save: function() {
      this.each(function() {
	var elem    = $(this);
	var info    = paneInfo(elem);
	var length, pos;

	if ( info.splitter.orientation == 'horizontal' ) {
	  length = elem.height();
	  pos    = $(info.first).height();
	} else {
	  length = elem.width();
	  pos    = $(info.first).width();
	}

	var percent = Math.round(((100 * pos) / length)) + "%";

	info.splitter.resizestart = percent;
      });

      this.find(".reactive-size").trigger("reactive-resize");
      return this;
    },

    /**
     * Act on a resize by keeping the relative distribution and respect
     * min/max style properties. This assumes that {@link resizestart}
     * is called at the start of the windowresize.
     * @example $(window).resize(function() { $(".tile").tile('resize'); });
     */
    resize: function() {
      return this.each(function() {
	var elem     = $(this);
	var splitter = elem.split();

	if ( splitter.resizestart ) {
	  splitter.position(splitter.resizestart);
	  splitter.settings.onDrag(elem);
	}
      });
    },

    /**
     * Split a pane, adding a new pane above/below/left/right of the
     * splitted pane.  `this` must be the pane content!
     * @param {Element} pane is a `<div>` element providing the content
     * for the new tile.
     * @param {String} [rel] is one of `above`/`below`/`left`/`right`.
     * Default is `"below"`
     * @param {Number} [pos] is percentage of the height/width taken by
     * the new pane
     * @param {Number} [minheight] is the minimum height of the new tab
     * in the case of a vertical split.
     * @return {jQuery} parent `pane-container` that hold me and the note
     * that was added next to me.
     */
    split: function(pane, rel, pos, minheight) {
      rel = rel||"below";

      var relto  = this;
      var dir    = (rel == "above" || rel == "below") ? "horizontal" : "vertical";
      var parent = relto.wrap('<div class="pane-container tile "' +
			      flipdir(dir) +
			      '></div>')
                        .parent();

      if ( pos == undefined )
	pos = 50;
      else if ( pos < 10 )
	pos = 10;
      else if ( pos > 90 )
	pos = 90;

      if ( minheight && dir == "horizontal" ) {
	sumh = this.height();
	left = sumh*pos/100;
	if ( left < minheight && minheight < sumh*0.9 ) {
	  pos = (minheight*100/sumh);
	}
      }

      if ( rel == "above" || rel == "left" ) {
	parent.prepend(pane);
      } else {
	pos = 100 - pos;
	parent.append(pane);
      }

      var panes = $(relto).add(pane);
      panes.wrap('<div class="pane-wrapper"></div>');

      parent.split({ orientation:dir,
		     position:pos+"%",
		     limit:10,
		     onDragStart: function() { parent.tile('resize_start'); },
		     onDrag:      function() { panes.trigger("pane.resize"); },
		     onDragEnd:   function() { parent.tile('resize_save'); }
		   });
      parent.tile('resize_save');
      panes.trigger("pane.resize");

      return parent;
    },

    /**
     * Remove a tile from the DOM, causing the remaining half to occupy
     * the whole space.  Again, `this` is the content pane.
     */
     close: function() {
      var pane = this;
      var splitContainer = pane.closest(".pane-container");

      splitContainer.split().destroy();
      pane.parent().remove();
      splitContainer.children().first().children().first().unwrap().unwrap();
    }
  }; // methods

  function paneInfo(pane) {
    var panes = pane.children();

    return { splitter: pane.split(),
             first:    $(panes[0]).children()[0],
	     second:   $(panes[2]).children()[0]
           };
  }

  function flipdir(dir) {
    return dir == "horizontal" ? "vertical" : "horizontal";
  }

  /**
   * Generate a tiled subwindow layout from a hierarchy of `<div>`
   * elements.  Below is the HTML that creates the SWISH 2.0 subwindow
   * layout.  This plugin uses the class `horizontal` or `vertical` to
   * decide on the direction of the split and the attribute `data-split`
   * to locate the split location.
   *
   *     <div class="tile horizontal" data-split="60%">
   *       <div class="prolog-editor"></div>
   *       <div class="tile vertical" data-split="70%">
   *         <div class="prolog-runners"></div>
   *         <div class="prolog-query"></div>
   *       </div>
   *     </div>
   *
   * @class tile
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @example $(".tile").tile();
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.tile = function(method) {
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
