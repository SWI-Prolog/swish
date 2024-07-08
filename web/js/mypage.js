define(["jquery"], function($) {
    // Ensure DOM is ready before initializing mypage plugin
    $(document).ready(function() {
        // Check if element with id 'mypage' exists
        if ($("#mypage").length) {
            $("#mypage").mypage();
        }
    });

    (function($) {
        var pluginName = 'mypage';
        const inputFields = $('.profile-input');
        const passwordFields = $('.password-group');
        const editButton = $('#edit_button');
        const editPasswordButton = $('#new_password');
        const cancelButton = $('#cancel_button');
        const updateButton = $('#update_button');
        const passwordCancelButton = $('#password_update_button');
        const passwordUpdateButton = $('#password_cancel_button');

        /** @lends $.fn.mypage */
        var methods = {
            prevValues: {},

            _init: function(options) {
                return this.each(function() {                   
                    var elem = $(this);
                    
                    // fetch userInfo
                    methods.fetchProfile(); 
                    
                    // handle button event
                    elem.on("click", "#edit_button", function(ev) {
                        updateButton.show();
                        cancelButton.show();
                        editButton.hide();
                        inputFields.prop('disabled', false);
                    });

                    elem.on("click", "#new_password", function(ev) {
                        passwordFields.show();
                        passwordCancelButton.show();
                        passwordUpdateButton.show();
                        editPasswordButton.hide();
                    });

                    elem.on("click", "#cancel_button", function(ev) {
                        methods.cancelEdit();
                    });

                    elem.on("click", "#password_cancel_button", function(ev) {
                        $('.password-input').val('');
                        passwordFields.hide();
                        passwordCancelButton.hide();
                        passwordUpdateButton.hide();
                        editPasswordButton.show();
                    });

                    elem.on("click", "#update_button", function(ev) {
                        const profileData = {
                            id: methods.prevValues.id,
                            username: $('input[name="username"]').val(),
                            email: $('input[name="email"]').val()
                        };

                        methods.updateProfile(profileData);
                    });

                    elem.on("click", "#password_update_button", function(ev) {
                        const passwordData = {
                            id: methods.prevValues.id,
                            current_password: $('input[name="current_password"]').val(),
                            new_password: $('input[name="new_password"]').val(),
                            confirm_new_password: $('input[name="confirm_new_password"]').val()
                        };

                        methods.updatePassword(passwordData);
                    });

                    elem.on("click", "#delete_button", function(ev) {
                        methods.deleteAccount();
                    });
                });
            },
            
            /**
             * fetch the user profile
             */
            fetchProfile: function() {
                $.ajax({
                    url: '/load_profile',
                    method: 'GET',
                    success: function(data) {
                        $('#userid').text('HI! ' + data.id);
                        $('input[name="username"]').val(data.username);
                        $('input[name="email"]').val(data.email);

                        methods.prevValues = {
                            id : data.id,
                            username: data.username,
                            email: data.email
                        };
                    },
                    error: function(xhr, status, error) {
                        console.error('사용자 정보를 가져오는 중 오류 발생:', error);
                    }
                });
            },
            
            /**
             * Update the user profile
             */
            updateProfile: function(profileData) {
                $.ajax({
                    url: '/update_info',
                    method: 'POST',
                    contentType: 'application/json',
                    data: JSON.stringify(profileData),
                    success: function(response) {
                        if (response.success) {
                            inputFields.prop('disabled', true);
                            updateButton.hide();
                            cancelButton.hide();
                            editButton.show();
                            methods.fetchProfile(); 
                            alert("Profile updated successfully!");
                          } else {
                            alert(response.message);
                          }
                    },
                    error: function(jqXHR) {
                        alert('Error updating profile: ' + jqXHR.responseText);
                    }
                });
            },

             /**
             * Update the password 
             */
             updatePassword: function(passwordData) {
                $.ajax({
                    url: '/update_password',
                    method: 'POST',
                    contentType: 'application/json',
                    data: JSON.stringify(passwordData),
                    success: function(response) {
                        if (response.success) {
                            $('.password-input').val('');
                            passwordFields.hide();
                            passwordCancelButton.hide();
                            passwordUpdateButton.hide();
                            editPasswordButton.show();
                            alert("Password updated successfully!");
                          } else {
                            alert(response.message);
                          }
                    },
                    error: function(jqXHR) {
                        alert('Error updating profile: ' + jqXHR.responseText);
                    }
                });
            },

            /**
             * Cancel edit mode
             */
            cancelEdit: function() {
                $('input[name="username"]').val(methods.prevValues.username);
                $('input[name="email"]').val(methods.prevValues.email);
                inputFields.prop('disabled', true);
                editButton.show();
                updateButton.hide();
                cancelButton.hide();
            },

            /**
             * Delete the user account
             */
            deleteAccount: function() {
                if (confirm("Are you sure you want to delete your account?")) {
                    const id = methods.prevValues.id;
                    console.log(id);
                    $.ajax({
                        url: '/delete_info',
                        method: 'POST',
                        contentType: 'application/json',
                        data: JSON.stringify({ userId: id}),
                        success: function(response) {
                            window.location.href = '/';
                            alert(response.message);
                        },
                        error: function(jqXHR) {
                            alert('Error deleting account!!!: ' + jqXHR.responseText);
                        }
                    });
                }
            }
        };

        $.fn.mypage = function(method) {
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
