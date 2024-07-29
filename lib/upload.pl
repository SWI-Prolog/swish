:- module(upload, [handle_upload/1, handle_delete/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

% 모듈을 상대 경로로 불러오기
:- use_module('/swish/lib/plugin/user_management').  % 상대 경로 사용
:- use_module(gitty).  % gitty 모듈 사용

% 디버그 토픽 정의
:- debug(upload).

% HTTP 핸들러 설정
:- http_handler(root(upload), handle_upload, [method(post)]).
:- http_handler(root(delete), handle_delete, [method(post)]).

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

% 파일 삭제 요청 처리
handle_delete(Request) :-
    (   get_user_id(UserID)
    ->  http_read_json_dict(Request, Dict),
        atom_string(FileAtom, Dict.get(filename)),
        delete_file(FileAtom, UserID, Response),
        reply_json_dict(Response)
    ;   reply_json_dict(_{status: "failure", message: "User not logged in"})
    ).

delete_file(FileName, UserID, Response) :-
    web_storage:open_gittystore(Dir),
    format(string(DebugMsg1), 'Gitty store directory: ~w', [Dir]),
    (   gitty:gitty_file(Dir, FileName, Head) % 파일 이름을 사용하여 최신 커밋 해시 찾기
    ->  format(string(DebugMsg2), 'Found head: ~w for file: ~w', [Head, FileName]),
        gitty:gitty_commit(Dir, Head, Meta), % 해시를 사용하여 메타데이터 가져오기
        format(string(DebugMsg3), 'Found metadata: ~w', [Meta]),
        (   web_storage:owns(_{user_id: UserID}, Meta, user(me))
        ->  UpdatedMeta = Meta.put(delete, true), % delete 필드를 true로 설정
            gitty:gitty_update(Dir, FileName, "", UpdatedMeta, _Commit), % 데이터는 빈 문자열로 설정
            Response = _{status: "success", message: "File deleted successfully", debug: [DebugMsg1, DebugMsg2, DebugMsg3]}
        ;   Response = _{status: "failure", message: "Permission denied: You do not own this file", debug: [DebugMsg1, DebugMsg2, DebugMsg3]}
        )
    ;   format(string(DebugMsg4), 'File not found: ~w', [FileName]),
        Response = _{status: "failure", message: "File not found", debug: [DebugMsg1, DebugMsg4]}
    ).

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
    Meta = _{author: UserID, public: false},  % 메타데이터 설정
    web_storage:gitty_create(Dir, FileName, Data, Meta, _Commit),
    directory_file_path(Dir, FileName, SavedPath).

% 사용자 정의 에러 메시지 정의
:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
    [ 'A file upload must be submitted as multipart/form-data using', nl,
      'name=file and providing a file-name'
    ].
