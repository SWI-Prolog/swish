/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2019, VU University Amsterdam
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
 * Load SWISH. Just provides the RequireJS config, requires jswish.js
 * and initialises this on the body.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

window.globalSettings = { // 전역 변수 설정
  user: null,
  logged_in: false,
  role : null
};


require.config({
  urlArgs: "ts="+new Date().getTime(),  /* prevent caching during development */
  waitSeconds: 60,      /* swish-min.js is big */
  paths:
  { jquery:      "../node_modules/jquery/dist/jquery.min",
    "jquery-ui": "../node_modules/jquery-ui/jquery-ui.min",
    laconic:     "../node_modules/laconic/laconic",
    bootstrap:   "../node_modules/bootstrap/dist/js/bootstrap.min",
    bloodhound:  "../node_modules/typeahead.js/dist/bloodhound",
    typeahead:   "../node_modules/typeahead.js/dist/typeahead.jquery",
    splitter:    "../node_modules/jquery.splitter/js/jquery.splitter-0.15.0",
    tagmanager:  "../node_modules/tagmanager/tagmanager",
    sha1:        "../node_modules/js-sha1/src/sha1",
    c3:          "../node_modules/c3/c3",
    d3:          "../node_modules/d3/d3",
    printThis:   "../node_modules/printthis/printThis",
    "svg-pan-zoom": "../node_modules/svg-pan-zoom/dist/svg-pan-zoom.min",
    sparkline:   "../node_modules/sparkline/dist/jquery.sparkline",

          /* CodeMirror extensions */
    "cm/mode/prolog": "codemirror/mode/prolog",
    "cm/addon/hover/prolog-hover": "codemirror/addon/hover/prolog-hover",
    "cm/addon/hover/text-hover": "codemirror/addon/hover/text-hover",
    "cm/addon/hint/templates-hint": "codemirror/addon/hint/templates-hint",
    "cm/addon/hint/show-context-info": "codemirror/addon/hint/show-context-info",

          /* Standard CodeMirror */
    "cm" : "../node_modules/codemirror"
  },
  shim:
  { bootstrap:
    { deps:["jquery"]
    },
    typeahead: /* HACK: See https://github.com/twitter/typeahead.js/issues/1211 */
    { deps:["jquery"],
      init: function ($) {
  return require.s.contexts._.registry['typeahead.js'].factory($);
      }
    },
    bloodhound:
    { deps:["jquery"]
    },
    splitter:
    { deps:["jquery"]
    },
    laconic:
    { deps:["jquery"]
    },
    tagmanager:
    { deps:["jquery"]
    },
  }
}); //require.config

/*
 * Create the SWISH application.  Note that we need two levels of
 * require because the first gives us the location of the pengine
 * API, while the second fetches the pengines and starts the
 * application.
 */
require(["jquery", "config", "jswish", "plugin", "mypage"],
  function($, config, swish, plugin, mypage) {
    var deps = plugin.load();

    deps.push(config.http.locations.pengines + "/pengines.js");

    require(deps, function() {
      $(function() {
        $("body").swish(config.swish || {});
        hideUploadButton();
      });
    });
  }
);

  
  // 업로드 버튼 숨기기
  function hideUploadButton() {
    $.ajax({
      url: '/user_info',
      method: 'GET',
      success: function(response) {
        if (response.logged_in) {
          $('#uploadButton').show();  // 로그인이 되어 있으면 업로드 버튼을 표시
        } else {
          $('#uploadButton').hide();  // 로그인이 되어 있지 않으면 업로드 버튼을 숨김
        }
      },
      error: function() {
        $('#uploadButton').hide();  // 오류가 발생하면 업로드 버튼을 숨김
      }
    });
    // 업로드 버튼에 id를 추가
    $('a:contains("Upload ...")').attr('id', 'uploadButton');
  }