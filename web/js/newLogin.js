define(["jquery", "modal"], function($) {
    (function($, modal) {
        var pluginName = 'newLogin';

        /** @lends $.fn.newLogin */
        var methods = {

            _init: function() {
                return this.each(function() {
                    var elem = $(this); // #login and plugin

                    // fetch Login status
                    elem.newLogin('fetchStatus'); 

                    // set Login click event handler
                    elem.on('click', function(event) {
                        if(elem.text() ==='Login'){ // login일 때 
                            event.preventDefault(); // 기본 동작 방지
                            showLoginModal(); 
                        }
                        if(elem.text() ==='Logout'){ // logout일 때 
                            elem.newLogin('logout');
                        }
                    });

                    // set Login submit event handler
                    $(document).on('submit', '#login-form', function(event){
                        event.preventDefault(); // 폼 제출 기본 동작 막기
                        elem.newLogin('login');

                    });

                    // set Sign up event handdler
                    $("body").on("click", "#sign-up-button", function(event) {
                        event.preventDefault();
                        $('#ajaxModal').modal('hide'); // 로그인 모달 닫기
                        showSignUpModal();
                        elem.newLogin('signUp');
                    });
                
                });
            },

            /**
             * fetch Login status
             */
            fetchStatus: function() {
                const loginButton = $(this);
                $.ajax({
                    url: '/user_info',
                    method: 'GET',
                    success: function(response) {
                        if (response.logged_in) { // true
                            window.globalSettings.user = response.user;
                            window.globalSettings.logged_in = true;
                            window.globalSettings.role = response.role;
                            setMypageButton(true);
                            loginButton.text('Logout');
                        
                        } else { // false
                            window.globalSettings.user = null;
                            window.globalSettings.logged_in = false;
                            window.globalSettings.role = null;
                            setMypageButton(false);
                            loginButton.text('Login');
                        }
                    },
                    error: function(xhr, status, error) {
                        console.error('Login error :', error);
                    }
                });
            },

            login: function(){
                const formData = {
                    id: $('#login-form #id').val(),
                    password: $('#login-form #password').val()
                };

                $.ajax({
                    url: '/authenticate',
                    method: 'POST',
                    contentType: 'application/json',
                    data: JSON.stringify(formData),
                    success: function(data) {
                        if (data.success) {
                            alert('Login successful!');
                            $('#ajaxModal').modal('hide');
                            location.reload();
                        } else {
                            alert('Error: ' + data.message);
                        }
                    },
                    error: function(xhr, status, error) {
                        console.error('Error:', error);
                    }
                });
            },

            logout: function(){
                $.ajax({
                    url: '/logout',
                    method: 'GET',
                    success: function(response){
                        if (response.success) {
                            alert('Logout successful!');
                            location.reload();
      
                        } else {
                            alert('Error: ' + logoutResponse.message);
                        }
                    },
                    error: function(xhr, status, error) {
                        console.error('Logout error :', error);
                    }
                })
            },

            signUp: function(){
                $(document).on('submit', '#signup-form', function(event) {
                    event.preventDefault();
                    const formData = {
                        id: $('#signup-id').val(),
                        password: $('#signup-password').val(),
                        username: $('#signup-username').val() || '',  // Optional field
                        email: $('#signup-email').val()
                    };

                    console.log(formData);
                
                    $.ajax({
                        url: '/signup',
                        method: 'POST',
                        contentType: 'application/json',
                        data: JSON.stringify(formData),
                        success: function(response) {
                            if (response.success) {
                                alert('Sign up successful!');
                                $('#signupModal').modal('hide');
                                $('#signupModal').on('hidden.bs.modal', function () {
                                $('.modal-backdrop').remove();
                                $('#signupModal').remove(); // 모달 제거
                            });
                            location.reload();    // 페이지 새로고침
                            } else {
                            alert(response.message);
                            }
                        }
                    });
                });
            },
            
    }; // methods

    function showLoginModal() {
        $("body").swishModal('server_form', {
            title: "Login",
            url: "/login", 
            onreply: function(response) {
                if (response.success) {
                    console.log(response);
                } else {
                    alert('Error: ' + response.message);
                }
            }
        });
    }
    
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

    $.fn.newLogin = function(method) {
        if (methods[method]) {
            return methods[method].apply(this, Array.prototype.slice.call(arguments, 1));
        } else if (typeof method === 'object' || !method) {
            return methods._init.apply(this, arguments);
        } else {
            $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
        }
    };
    }(jQuery));
});
