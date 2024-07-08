/*  Part of SWISH

    Author:        Uijeong Jeon
    E-mail:        juijeong8324@gmail.com
    Copyright (C): 2024, Your Organization
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

:- module(swish_mypage, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/json)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(library(error)).

:- use_module(config).
:- use_module(authenticate).
:- use_module(page).
:- use_module(user_management). 

:- http_handler(swish(mypage), mypage_handler, [id(mypage)]).
:- http_handler('/load_profile', load_profile_handler, []).

mypage_handler(_Request) :-
    reply_html_page(
        title('SWISH - Mypage'),
        [
          \mypage_content
        ]).

mypage_content -->
    swish_page:swish_resources,
    html([
        body(id('mypage'), [
            h1([id(userid)], ''),
            div(class('container'), [
                div(class('form-group'), [
                    label([for(username)], 'Username'),
                    input([type(text), class('profile-input'), name(username), disabled])
                ]),
                div(class('form-group'), [
                    label([for(email)], 'Email'),
                    input([type(email), class('profile-input'), name(email), disabled])
                ]),
                div(class('form-actions'), [
                    button([id(edit_button)], 'Edit Profile'), % 수정 버튼
                    button([id(update_button), style('display:none')], 'Update'), % 업데이트 버튼
                    button([id(cancel_button), style('display:none')], 'Cancel') % 취소 버튼
                ])
            ]),
            div([class('container'), id('password_container')], [
                div(class('form-group'), [
                    label([for(password)], 'Password')
                ]),
                div([class('password-group'), style('display:none')], [
                    label([for(current_password)], 'Current Password'),
                    input([type(password), class('password-input'), name(current_password)]),
                    label([for(new_password)], 'New Password'),
                    input([type(password), class('password-input'), name(new_password)]),
                    label([for(confirm_new_password)], 'Confirm New Password'),
                    input([type(password), class('password-input'), name(confirm_new_password)])
                ]),
                div(class('form-actions'), [
                    button([id(new_password)], 'New Password'), % New Password 버튼
                    button([id(password_update_button), style('display:none')], 'Update'), % 업데이트 버튼
                    button([id(password_cancel_button), style('display:none')], 'Cancel') % 취소 버튼
                ])
            ]),
            button([id(delete_button)], 'Delete Account') % 삭제 버튼
        ])
    ]).


% get User Details
get_user_details(UserID, UserDetails) :-
    user_management:load_user_info(Users),          
    (   member(User, Users),
        User.id == UserID
    ->  UserDetails = User
    ;   UserDetails = _{error: "User not found"} 
    ).

% load User Details using Session
load_userDetails_session(_Request, UserDetails) :-
    (   catch(http_session_data(user(UserID)), _, fail) % check session
    ->  get_user_details(UserID, UserDetails)
    ;   UserDetails = _{error: "No active session"}
    ).

% load_userInfo handler
load_profile_handler(Request) :-
    load_userDetails_session(Request, UserDetails),
    reply_json_dict(UserDetails).