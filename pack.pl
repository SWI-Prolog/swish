:- module(gitty_pack,
          [ pack_objects/2,             % +Store, +Pack
            pack_objects/3,             % +Store, +Objects, +Pack
            attach_pack/1,		% +Pack
            load_object_from_pack/4	% +Hash, -Data, -Type, -Size
          ]).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(zlib)).
:- use_module(lib/gitty_driver_files).

/** <module> Pack gitty objects

<pack file> := <header>
               <file>*
<header>    := "gitty(Version).\n" <object>* "end_of_header.\n"
<object>    := obj(Hash, Type, Size, FileSize)
*/

pack_version(1).

%!  pack_objects(+Store, +Pack) is det.
%!  pack_objects(+Store, +Objects, +Pack) is det.

pack_objects(Store, Pack) :-
    findall(Object, gitty_hash(Store, Object), Objects),
    pack_objects(Store, Objects, Pack).

pack_objects(Store, Objects, Pack) :-
    maplist(object_info(Store), Objects, Info),
    setup_call_cleanup(
        open(Pack, write, Out, [type(binary)]),
        (   write_signature(Out),
            maplist(write_header(Out), Info),
            format(Out, 'end_of_header.~n', []),
            maplist(add_file(Out, Store), Objects)
        ),
        close(Out)).

object_info(Store, Object, obj(Object, Type, Size, FileSize)) :-
    gitty_object_file(Store, Object, File),
    load_object_header(Store, Object, Type, Size),
    size_file(File, FileSize).

write_signature(Out) :-
    pack_version(Version),
    format(Out, "gitty(~d).~n", [Version]).

write_header(Out, obj(Object, Type, Size, FileSize)) :-
    format(Out, 'obj(~q,~q,~d,~d).~n', [Object, Type, Size, FileSize]).

add_file(Out, Store, Object) :-
    gitty_object_file(Store, Object, File),
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        copy_stream_data(In, Out),
        close(In)).

:- dynamic
    pack_object/5.                      % Hash, Type, Size, Offset, PackFile

%!  attach_pack(+Pack)
%
%   Load the index of Pack into memory.

attach_pack(Pack) :-
    retractall(pack_object(_,_,_,_,Pack)),
    setup_call_cleanup(
        open(Pack, read, In, [type(binary)]),
        ( read_header(In, Version, Objects),
          get_code(In, Code),
          assertion(Code == 0'\n),
          byte_count(In, DataOffset)
        ),
        close(In)),
    foldl(assert_object(Pack, Version), Objects, DataOffset, _).

read_header(In, Version, Objects) :-
    read(In, Signature),
    (   Signature = gitty(Version)
    ->  true
    ;   domain_error(gitty_pack_file, Objects)
    ),
    read(In, Term),
    read_index(Term, In, Objects).

read_index(end_of_header, _, []) :-
    !.
read_index(Object, In, [Object|T]) :-
    read(In, Term2),
    read_index(Term2, In, T).

assert_object(Pack, _Version,
              obj(Object, Type, Size, FileSize),
              Offset0, Offset) :-
    Offset is Offset0+FileSize,
    assertz(pack_object(Object, Type, Size, Offset0, Pack)).

%!  load_object_from_pack(+Hash, -Data, -Type, -Size)

load_object_from_pack(Hash, Data, Type, Size) :-
    pack_object(Hash, Type, Size, Offset, Pack),
    setup_call_cleanup(
        open(Pack, read, In, [type(binary)]),
        read_object_at(In, Offset, Data, Type, Size),
        close(In)).

read_object_at(In, Offset, Data, Type, Size) :-
    seek(In, Offset, bof, Offset),
    set_stream(In, encoding(utf8)),
    setup_call_cleanup(
        zopen(In, In2, [multi_part(false), close_parent(false)]),
        gitty_driver_files:read_object(In2, Data, Type, Size),
        close(In2)).
