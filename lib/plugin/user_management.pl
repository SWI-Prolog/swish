:- module(user_management,
          [ signup_handler/1,
            login_handler/1,
            logout_handler/1,
            user_info_handler/1,
            initialize_db/0,
            save_user_info/4,
            load_user_info/1,
            id_exists/2,
            email_exists/2,
            username_exists/3
          ]).

:- use_module(library(http/json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)).
:- use_module(library(crypt)).
:- use_module(library(filesex)).  % make_directory_path/1를 사용하기 위해 필요
:- use_module(library(debug)). % 디버깅을 위해 추가

% 서버 시작 시 초기화
:- initialization(initialize_db).

initialize_db :-
    File = '/swish/lib/plugin/users.json',  % 절대 경로로 설정
    (   exists_file(File)
    ->  true
    ;   open(File, write, Stream),
        json_write_dict(Stream, [], []),
        close(Stream)
    ),
    % 모든 세션 초기화
    http_session_retractall(_).

% 사용자 정보를 파일에 저장
save_user_info(ID, Password, Username, Email) :-
    File = '/swish/lib/plugin/users.json',
    load_user_info(Users),
    \+ id_exists(ID, Users),  % 중복 ID 확인
    \+ email_exists(Email, Users),  % 중복 이메일 확인
    crypt(Password, Hash),  % 비밀번호 해싱
    append(Users, [_{id: ID, password: Hash, username: Username, email: Email}], NewUsers),
    open(File, write, Stream),
    json_write_dict(Stream, NewUsers, []),
    close(Stream).

% 사용자 정보를 파일에서 로드
load_user_info(Users) :-
    File = '/swish/lib/plugin/users.json',
    open(File, read, Stream),
    json_read_dict(Stream, Users, []),
    close(Stream).

% 사용자 정보를 파일에 업데이트 
update_user_info(ID, NewUsername, NewEmail) :-
    File = '/swish/lib/plugin/users.json',
    load_user_info(Users),
    \+ username_exists(NewUsername, ID, Users), % 중복 Username 확인
    \+ email_exists(NewEmail, ID, Users), % 중복 Email 확인
    (   select(User, Users, RestUsers),
        User.id == ID
    ->  (
            append(RestUsers, [_{id: ID, password: User.password, email: NewEmail, username: NewUsername}], NewUsers)
        )
    ;   % 사용자를 찾지 못한 경우
        format('User with ID ~w not found.', [ID]),
        fail
    ),
    open(File, write, Stream),
    json_write_dict(Stream, NewUsers, []),
    close(Stream).

% 사용자 정보 삭제 
delete_user_info(ID) :-
    File = '/swish/lib/plugin/users.json',
    load_user_info(Users),
    (   select(User, Users, RestUsers),
        User.id == ID
    ->  % ID에 해당하는 사용자를 찾으면
        open(File, write, Stream),
        json_write_dict(Stream, RestUsers, []),
        close(Stream)
    ;   % 사용자를 찾지 못한 경우
        format('User with ID ~w not found.', [ID]),
        fail
    ).

% 비밀번호 업데이트 함수
update_user_password(ID, NewPassword, Users) :-
    File = '/swish/lib/plugin/users.json',
    crypt(NewPassword, Hash),
    (   select(User, Users, RestUsers),
        User.id == ID
    ->  append(RestUsers, [_{id: ID, password: Hash, email: User.email, username: User.username}], NewUsers),
        open(File, write, Stream),
        json_write_dict(Stream, NewUsers, []),
        close(Stream)
    ;   format('User with ID ~w not found.', [ID]), fail
    ).

% ID 중복 확인
id_exists(ID, Users) :-
    member(User, Users),
    User.id == ID.

% 이메일 중복 확인
email_exists(Email, Users) :-
    member(User, Users),
    User.email == Email.

% 이메일 중복 확인 (사용자 업데이트 용)
email_exists(Email, ID, Users) :-
    member(User, Users),
    User.id \= ID,  % ID와 다른 경우만 고려
    User.email == Email.

% Username 중복 확인
username_exists(Username, ID, Users) :-
    Username \= "", % Username이 빈 값이 아닌 경우에 대해서만
    member(User, Users),
    User.id \= ID, % ID가 다른 경우만
    User.username == Username.

% 현재 비밀번호 확인
check_password(CurrentPassword, ID, Users) :-
    member(User, Users),
    User.id == ID, % 입력된 ID와 일치하는 사용자를 찾음
    crypt(CurrentPassword, User.password). % 입력된 현재 비밀번호를 해시된 비밀번호와 비교


% 회원가입 요청을 처리하는 핸들러
signup_handler(Request) :-
    http_read_json_dict(Request, Dict),
    (   _{id: ID, password: Password, username: Username, email: Email} :< Dict
    ->  load_user_info(Users),
        (   id_exists(ID, Users)
        ->  reply_json_dict(_{success: false, message: "ID already exists"})
        ;   email_exists(Email, Users)
        ->  reply_json_dict(_{success: false, message: "Email already exists"})
        ;   (   save_user_info(ID, Password, Username, Email)
            ->  reply_json_dict(_{success: true})
            ;   reply_json_dict(_{success: false, message: "Failed to save user information"})
            )
        )
    ;   reply_json_dict(_{success: false, message: "Invalid request data"})
    ).

% 로그인 요청을 처리하는 핸들러
login_handler(Request) :-
    http_read_json_dict(Request, Dict),
    _{id: ID, password: Password} :< Dict,
    load_user_info(Users),
    (   member(User, Users),
        User.id == ID,
        crypt(Password, User.password)  % 입력된 비밀번호를 해시하여 비교
    ->  http_session_assert(user(ID)),
        reply_json_dict(_{success: true})
    ;   reply_json_dict(_{success: false, message: "Invalid ID or password"})
    ).

% user 정보 업데이트를 요청을 처리하는 핸들러
update_user_handler(Request) :-
    http_read_json_dict(Request, Dict),
    (   catch(http_session_data(user(UserID)), _, fail)
    ->  (   _{id: ID, username: Username, email: Email} :< Dict,
            ID == UserID  % 세션의 사용자와 요청의 사용자가 일치하는지 확인
        ->  load_user_info(Users),
            (   username_exists(Username, ID, Users)
            ->  reply_json_dict(_{success: false, message: "Username already exists"})
            ;   email_exists(Email, ID, Users)
            ->  reply_json_dict(_{success: false, message: "Email already exists"})
            ;   (   update_user_info(ID, Username, Email)
                ->  reply_json_dict(_{success: true})
                ;   reply_json_dict(_{success: false, message: "Failed to save user information"})
                )
            )
        ;   reply_json_dict(_{success: false, message: "Invalid request data"})
        )
    ;   reply_json_dict(_{success: false, message: "User not logged in"})
    ).

% user 삭제 핸들러
delete_user_handler(Request) :-
    http_read_json_dict(Request, Dict),
    (   catch(http_session_data(user(UserID)), _, fail)
    ->  (   _{userId: ID} :< Dict,
            ID == UserID  % 세션의 사용자와 요청의 사용자가 일치하는지 확인
        ->  (   delete_user_info(ID)
            ->  (   http_session_retract(user(UserID)), % 세션 삭제
                    reply_json_dict(_{success: true, message: "User deleted successfully!"})
                )
            ;  reply_json_dict(_{success: false, message: "Failed to delete user"})
            )
        ;   reply_json_dict(_{success: false, message: "Invalid request data"})
        )
    ;   reply_json_dict(_{success: false, message: "User not logged in"})
    ).

% 비밀번호 업데이트 핸들러
update_password_handler(Request) :-
    http_read_json_dict(Request, Dict),
    (   catch(http_session_data(user(UserID)), _, fail)
    ->  (   _{id: ID, current_password: CurrentPassword, new_password: NewPassword, confirm_new_password: ConfirmNewPassword} :< Dict,
            ID == UserID  % 세션의 사용자와 요청의 사용자가 일치하는지 확인
        ->  load_user_info(Users),
            (   check_password(CurrentPassword, ID, Users)
            ->  (   NewPassword == ConfirmNewPassword
                ->  (   update_user_password(ID, NewPassword, Users)
                    ->  reply_json_dict(_{success: true})
                    ;   reply_json_dict(_{success: false, message: "Failed to update password"})
                    )
                ;   reply_json_dict(_{success: false, message: "New password and confirmation do not match"})
                )
            ;   reply_json_dict(_{success: false, message: "Current password is incorrect"})
            )
        ;   reply_json_dict(_{success: false, message: "Invalid request data"})
        )
    ;   reply_json_dict(_{success: false, message: "User not logged in"})
    ).

% 로그아웃 요청을 처리하는 핸들러
logout_handler(_Request) :-
    http_session_retract(user(_)),
    reply_json_dict(_{success: true}).
    http_redirect(moved, '/', Request).

% 로그인 상태 확인 핸들러
user_info_handler(_Request) :-
    (   catch(http_session_data(user(UserID)), _, fail)
    ->  reply_json_dict(_{logged_in: true, user: UserID})
    ;   reply_json_dict(_{logged_in: false})
    ).

% HTTP 핸들러 등록
:- http_handler('/signup', user_management:signup_handler, [method(post)]).
:- http_handler('/login', user_management:login_handler, [method(post)]).
:- http_handler('/logout', user_management:logout_handler, [method(post)]).
:- http_handler('/user_info', user_management:user_info_handler, [method(get)]).
:- http_handler('/update_info', user_management:update_user_handler, [method(post)]).
:- http_handler('/delete_info', user_management:delete_user_handler, [method(post)]).
:- http_handler('/update_password', user_management:update_password_handler, [method(post)]).