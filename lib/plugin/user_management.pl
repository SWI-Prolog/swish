:- module(user_management,
          [ signup_handler/1,
            login_handler/1,
            logout_handler/1,
            user_info_handler/1,
            initialize_db/0,
            save_user_info/4,
            load_user_info/1,
            id_exists/2,
            email_exists/2
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

% ID 중복 확인
id_exists(ID, Users) :-
    member(User, Users),
    User.id == ID.

% 이메일 중복 확인
email_exists(Email, Users) :-
    member(User, Users),
    User.email == Email.

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
:- http_handler('/signup', user_management:signup_handler, []).
:- http_handler('/logout', user_management:logout_handler, []).
:- http_handler('/user_info', user_management:user_info_handler, []).
