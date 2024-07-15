:- module(upload, [handle_upload/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(debug)).

% 모듈을 상대 경로로 불러오기
:- use_module('/swish/lib/plugin/user_management').  % 상대 경로 사용
:- use_module(web_storage).  % 상대 경로 사용

% 디버그 토픽 정의
:- debug(upload).

% HTTP 핸들러 설정
:- http_handler(root(upload), handle_upload, [method(post)]).

% 파일 업로드 요청 처리
handle_upload(Request) :-
    multipart_post_request(Request), !,
    http_read_data(Request, Parts, [on_filename(save_file)]),
    memberchk(file=file(FileName, TempFile), Parts),
    (   get_user_id(UserID)
    ->  save_file_to_storage(FileName, TempFile, UserID, SavedPath),
        format('Content-type: application/json~n~n'),
        format('{"status":"success","filename":"~w","saved":"~w"}', [FileName, SavedPath])
    ;   throw(http_reply(bad_request('User not logged in')))
    ).
handle_upload(_Request) :-
    throw(http_reply(bad_request(bad_file_upload))).

% 멀티파트 폼 데이터 요청 확인
multipart_post_request(Request) :-
    memberchk(method(post), Request),
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)).

% 파일 저장 핸들러
:- public save_file/3.

save_file(In, file(FileName, TempFile), Options) :-
    option(filename(FileName), Options),
    setup_call_cleanup(
        tmp_file_stream(octet, TempFile, Out),
        copy_stream_data(In, Out),
        close(Out)).

% 파일을 storage.pl을 통해 저장하는 로직
save_file_to_storage(FileName, TempFile, UserID, SavedPath) :-
    web_storage:open_gittystore(Dir),
    setup_call_cleanup(
        open(TempFile, read, In, [type(binary)]),
        read_string(In, _, Data),
        close(In)
    ),
    Meta = _{author: UserID, public: true},  % 메타데이터 설정
    web_storage:gitty_create(Dir, FileName, Data, Meta, _Commit),
    directory_file_path(Dir, FileName, SavedPath).

% 사용자 정의 에러 메시지 정의
:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
    [ 'A file upload must be submitted as multipart/form-data using', nl,
      'name=file and providing a file-name'
    ].
