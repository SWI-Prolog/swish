/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2017, VU University Amsterdam
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

:- module(content_filter,
          [ eval_content/3              % +Text, -Wordcount, -Score
          ]).
:- use_module(library(porter_stem)).
:- use_module(library(apply)).
:- use_module(library(debug)).

/** <module> Ban list content filter

@see https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
@see https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/
*/

:- dynamic
    black/1,
    white/1,
    wl_loaded/0.

%!  eval_content(+Text, -WordCount, -Score) is det.
%
%   Evaluate the content of Text. WordCount is the number of non-trivial
%   words and Score is the evaluation.

eval_content(Text, WordCount, Score) :-
    read_word_lists,
    tokenize_atom(Text, Tokens),
    wordlist(Tokens, Words),
    length(Words, WordCount),
    foldl(acc_score, Words, 0-0, Score0-_Acc),
    Score is max(-100, min(100, Score0)).

acc_score(Word, V0-A0, V-A) :-
    downcase_atom(Word, Lower),
    score(Lower, WScore),
    A is min(0, A0//2 + WScore),
    V is V0+A+WScore,
    debug(spam, '~w: ~w, ~w -> ~w', [Word, WScore, V0-A0, V-A]).

score(Word, 20) :-
    current_predicate(Word, system:_),
    !.
score(Word, Score) :-
    black(Word),
    !,
    Score = -50.
score(Word, Score) :-
    white(Word),
    !,
    Score = 10.
score(Word, Score) :-
    glued_identifier(Word),
    !,
    Score = 15.
score(_, -5).

glued_identifier(Word) :-
    id_part(Word),
    !.
glued_identifier(Word) :-
    atom_length(Word, Len),
    Len > 25,
    !,
    fail.
glued_identifier(Word) :-
    (   sub_atom(Word, _, _, _, '_'),
        atomic_list_concat(Parts, '_', Word),
        Parts = [_,_|_]
    ->  !
    ),
    maplist(id_part, Parts).
glued_identifier(Word) :-
    atom_concat(Pre, Rest, Word),
    atom_length(Pre, L),
    L > 2,
    id_part(Pre),
    glued_identifier(Rest),
    !.
glued_identifier(Word) :-
    (   atom_concat(Pre, Rest, Word),
        atom_number(Rest, _)
    ->  id_part(Pre)
    ).

id_part(Part) :-
    atom_length(Part, 1),
    !.
id_part(Part) :-
    atom_number(Part, _).
id_part(Part) :-
    downcase_atom(Part, Word),
    white(Word),
    !,
    \+ black(Word).


%!  wordlist(+Tokens, -WordList) is det.
%
%   Filter the token list. Removes  numbers   and  joins  typical escape
%   patterns such as 't h i s' or 't.h.i.s'.

wordlist([], []).
wordlist([H|T0], Words) :-
    single_char(H),
    !,
    single_chars(T0, Chars, T),
    (   make_word([H|Chars], Word)
    ->  Words = [Word|TW]
    ;   TW = Words
    ),
    wordlist(T, TW).
wordlist([H|T], Words) :-
    number(H),
    !,
    wordlist(T, Words).
wordlist([H|T0], [H|T]) :-
    wordlist(T0, T).

single_chars([H|T0], [H|T], Rest) :-
    single_char(H),
    !,
    single_chars(T0, T, Rest).
single_chars(List, [], List).

single_char(H) :-
    atom(H),
    !,
    atom_length(H, 1).
single_char(H) :-
    integer(H),
    between(0, 9, H).

make_word(List, Word) :-
    separated(List, _Sep, Chars),
    wordy(Chars),
    atomic_list_concat(Chars, Word).
make_word(List, Word) :-
    wordy(List),
    !,
    atomic_list_concat(List, Word).

separated([H,Sep|T0], Sep, [H|T]) :-
    char_type(Sep, punct),
    separated_([Sep|T0], Sep, T).

separated_([], _, []).
separated_([Sep,H|T0], Sep, [H|T]) :-
    separated_(T0, Sep, T).

wordy(Chars) :-
    wordy(Chars, 0, V),
    V >= 3.

wordy([H|T], V0, V) :-
    char_type(H, alnum),
    !,
    V1 is V0 + 1,
    wordy(T, V1, V).
wordy([_|T], V0, V) :-
    V1 is V0 - 1,
    wordy(T, V1, V).


		 /*******************************
		 *           WORD LISTS		*
		 *******************************/

read_word_lists :-
    wl_loaded,
    !.
read_word_lists :-
    with_mutex(content_filter, read_word_lists_sync).

read_word_lists_sync :-
    wl_loaded,
    !.
read_word_lists_sync :-
    read_word_list(wordlist('words'), white),
    read_word_list(wordlist('whitelist.txt'), white),
    read_word_list(wordlist('bad-words.txt'), black),
    read_word_list(wordlist('bad-words-google.txt'), black),
    assertz(wl_loaded).

:- multifile user:file_search_path/2.
user:file_search_path(wordlist, '/usr/share/dict').
user:file_search_path(wordlist, Dir) :-
    source_file(read_word_lists, SrcFile),
    file_directory_name(SrcFile, Dir).

%!  read_word_list(+FileSpec, +List) is det.
%
%   Read a list of words into a fact.

read_word_list(File, List) :-
    absolute_file_name(File, Path, [access(read), file_errors(fail)]),
    !,
    setup_call_cleanup(
        open(Path, read, In, [encoding(utf8)]),
        (   lazy_list(lazy_read_lines(In, [as(atom)]), Words),
            forall(member(Word, Words),
                   assert_word(List, Word))
        ),
        close(In)).
read_word_list(_, _).

assert_word(black, Word0) :- downcase_atom(Word0, Word), assertz(black(Word)).
assert_word(white, Word0) :- downcase_atom(Word0, Word), assertz(white(Word)).

