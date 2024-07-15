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
  logged_in: false
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
        setupNavigationBar();
        hideUploadButton();
      });
    });
  }
);

function setupNavigationBar() {
  // 네비게이션 바에 Login 메뉴 항목을 한 번만 추가
  if ($('#login-menu').length === 0) {
    $('#navbar > ul.nav.navbar-nav.menubar').append(`
      <li class="dropdown" id="login-menu">
        <a href="#" id="login" class="dropdown-toggle" data-toggle="dropdown">Login</a>
      </li>
    `);
  }
  // 로그인 상태 업데이트 함수 호출
  updateLoginStatus();
}

function showLoginModal() {
  $("body").swishModal('server_form', {
      title: "Login",
      url: "/login", // 로그인 폼을 제공하는 서버 URL
      onreply: function(response) {
          if (response.success) {
              alert('Login successful!');
              $('#ajaxModal').modal('hide');
              updateLoginStatus(); // 로그인 성공 후 상태 업데이트
          } else {
              alert('Error: ' + response.message);
          }
      }
  });
}

function updateLoginStatus() {
  $.get("/user_info", function(response) {
      if (response.logged_in) {
          window.globalSettings.user = response.user;
          window.globalSettings.logged_in = true;
          setMypageButton(true);
          $('#login').text('Logout').removeClass('login').addClass('logout');
          $('#login').off('click').on('click', function(event) {
              event.preventDefault();
              $.post("/logout", function(logoutResponse) {
                  if (logoutResponse.success) {
                      alert('Logout successful!');
                      updateLoginStatus(); // 로그아웃 후 상태 업데이트
                      location.reload();

                  } else {
                      alert('Error: ' + logoutResponse.message);
                  }
              }, "json");
          });
      } else {
          window.globalSettings.user = null;
          window.globalSettings.logged_in = false;
          setMypageButton(false);
          $('#login').text('Login').removeClass('logout').addClass('login');
          $('#login').off('click').on('click', function(event) {
              event.preventDefault();
              showLoginModal();
          });
      }
  });
}

// 로그인 폼 제출 시 JSON 형식으로 데이터를 전송하는 이벤트 핸들러 설정
$(document).on('submit', '#login-form', function(event) {
  event.preventDefault(); // 폼 제출 기본 동작 막기

  const formData = {
    id: $('#login-form #id').val(),
    password: $('#login-form #password').val()
  };

  fetch('/authenticate', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(formData)
  })
  .then(response => response.json())
  .then(data => {
    if (data.success) {
      alert('Login successful!');
      $('#ajaxModal').modal('hide');
      updateLoginStatus(); // 로그인 후 상태 업데이트
      location.reload();
    } else {
      alert('Error: ' + data.message);
    }
  })
  .catch(error => {
    console.error('Error:', error);
  });
});

  // Sign up 메뉴 항목에 이벤트 핸들러 설정
  $("body").on("click", "#sign-up-button", function(event) {
    event.preventDefault();
    $('#ajaxModal').modal('hide'); // 로그인 모달 닫기
    showSignUpModal();
  });


function showSignUpModal() {
  const modalContent = `
    <div class="modal fade" id="signupModal" tabindex="-1" role="dialog" aria-labelledby="signupModalLabel" aria-hidden="true">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <h5 class="modal-title" id="signupModalLabel">Sign Up</h5>
            <button type="button" class="close" data-dismiss="modal" aria-label="Close">
              <span aria-hidden="true">&times;</span>
            </button>
          </div>
          <div class="modal-body">
            <form id="signup-form">
              <div class="form-group">
                <label for="signup-id">ID</label>
                <input type="text" class="form-control" id="signup-id" name="id" required>
              </div>
              <div class="form-group">
                <label for="signup-password">Password</label>
                <input type="password" class="form-control" id="signup-password" name="password" required>
              </div>
              <div class="form-group">
                <label for="signup-username">Username (Optional)</label>
                <input type="text" class="form-control" id="signup-username" name="username">
              </div>
              <div class="form-group">
                <label for="signup-email">Email</label>
                <input type="email" class="form-control" id="signup-email" name="email" required>
              </div>
              <button type="submit" class="btn btn-primary">Sign up</button>
            </form>
          </div>
        </div>
      </div>
    </div>`;

  $('body').append(modalContent);
  $('#signupModal').modal('show');

  $('#signup-form').on('submit', function(event) {
    event.preventDefault();
    const formData = {
      id: $('#signup-id').val(),
      password: $('#signup-password').val(),
      username: $('#signup-username').val() || '',  // Optional field
      email: $('#signup-email').val()
    };

    $.ajax({
      type: 'POST',
      url: '/signup',
      data: JSON.stringify(formData),
      contentType: 'application/json',
      success: function(response) {
        if (response.success) {
          alert('Sign up successful!');
          $('#signupModal').modal('hide');
          $('#signupModal').on('hidden.bs.modal', function () {
            $('.modal-backdrop').remove();
            $('#signupModal').remove(); // 모달 제거
          });
        } else {
          alert(response.message);
        }
      }
    });
  });

  $('#signupModal').on('hidden.bs.modal', function () {
    $('.modal-backdrop').remove();
    $('#signupModal').remove(); // 모달 제거
  });
}


  /**
     * Login시에만 mypage 버튼이 보이도록 조절 - 임시
     */
  function setMypageButton(isLogin) {
    if (isLogin) { // 로그인이 true일 때 
        $('.mypage-button').show();
        
      } else {
        $('.mypage-button').hide();
      }
  }

  
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