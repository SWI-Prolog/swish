:- module(upload, [handle_upload/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(web_storage).
:- use_module(gitty).

:- http_handler(root(upload), handle_upload, [method(post)]).

handle_upload(Request) :-
    multipart_post_request(Request), !,
    http_read_data(Request, Parts, [on_filename(save_file)]),
    memberchk(file=file(FileName, TempFile), Parts),
    save_file_to_storage(FileName, TempFile, SavedPath),
    format('Content-type: application/json~n~n'),
    format('{"status":"success","filename":"~w","saved":"~w"}', [FileName, SavedPath]).
handle_upload(_Request) :-
    throw(http_reply(bad_request(bad_file_upload))).

multipart_post_request(Request) :-
    memberchk(method(post), Request),
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)).

:- public save_file/3.

save_file(In, file(FileName, TempFile), Options) :-
    option(filename(FileName), Options),
    setup_call_cleanup(
        tmp_file_stream(octet, TempFile, Out),
        copy_stream_data(In, Out),
        close(Out)).

save_file_to_storage(FileName, TempFile, SavedPath) :-
    web_storage:open_gittystore(Dir),
    setup_call_cleanup(
        open(TempFile, read, In, [type(binary)]),
        read_string(In, _, Data),
        close(In)
    ),
    Meta = _{author: 'user', public: true}, 
    gitty:gitty_create(Dir, FileName, Data, Meta, _Commit),
    directory_file_path(Dir, FileName, SavedPath).

% error message
:- multifile prolog:message//1.

prolog:message(bad_file_upload) -->
    [ 'A file upload must be submitted as multipart/form-data using', nl,
      'name=file and providing a file-name'
    ].
