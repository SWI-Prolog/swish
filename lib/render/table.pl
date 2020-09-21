/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2020, VU University Amsterdam
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

:- module(swish_render_table,
          [ term_rendering//3                   % +Term, +Vars, +Options
          ]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(dicts)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module('../render').
:- use_module(library(error)).

:- register_renderer(table, "Render data as tables").

/** <module> SWISH table renderer

Render table-like data.
*/

%!  term_rendering(+Term, +Vars, +Options)//
%
%   Renders Term as  a  table.   This  renderer  recognises  several
%   representations of table-like data:
%
%     $ A list of dicts holding the same keys
%     $ A list of terms of equal arity :
%     $ A list of lists of equal length :
%
%   Options:
%
%     - header(+Header)
%     Specify the header row. This is either a row whose number of
%     columns must match the columns in the table or the atom
%     `first_row`, which causes the first row to be used as header.
%     Multiple `header` options may be present.  The first matching
%     row is used, next the `first_row` value and if nothing matches
%     no header is displayed.
%     - align(+Alignment)
%     Specify the column alignment as one of `l`, `c` or `r` or the
%     more verbose `left`, `center`, `right`.  Default is `left`,
%     except for columns holding only integers.
%
%   @tbd: recognise more formats

term_rendering(Term, _Vars, Options) -->
    { is_list_of_dicts(Term, _NRows, ColNames),
      !,
      partition(is_header, Options, _HeaderOptions, Options1),
      fix_op_priority(Options1, Options2),
      column_alignment(Term, Options2, Options3)
    },
    !,
    html(div([ style('display:inline-block'),
               'data-render'('List of dicts as a table')
             ],
             [ table(class('render-table'),
                     [ \header_row(ColNames),
                       \rows(Term, Options3)
                     ])
             ])).
term_rendering(Term, _Vars, Options) -->
    { is_list_of_terms(Term, _NRows, _NCols),
      header(Term, Rows, Header, Options, Options1),
      fix_op_priority(Options1, Options2),
      column_alignment(Rows, Options2, Options3)
    },
    !,
    html(div([ style('display:inline-block'),
               'data-render'('List of terms as a table')
             ],
             [ table(class('render-table'),
                     [ \header_row(Header),
                       \rows(Rows, Options3)
                     ])
             ])).
term_rendering(Term, _Vars, Options) -->
    { is_list_of_lists(Term, _NRows, _MCols),
      header(Term, Rows, Header, Options, Options1),
      fix_op_priority(Options1, Options2),
      column_alignment(Rows, Options2, Options3)
    },
    !,
    html(div([ style('display:inline-block'),
               'data-render'('List of lists as a table')
             ],
             [ table(class('render-table'),
                     [ \header_row(Header),
                       \rows(Rows, Options3)
                     ])
             ])).

%!  fix_op_priority(+Options0, -Options)
%
%   Bindings a normally printed with priority  699 (the right hand limit
%   for =/2). This is rather meaningless   for  tables. We therefore map
%   699  to  1200.  We  should  preserve    a   user  value  given  with
%   use_rendering/2, but we  cannot  distinguish   this  from  higher up
%   defaults.

fix_op_priority(Options0, Options) :-
    select_option(priority(699), Options0, Options1),
    !,
    Options = [priority(1200)|Options1].
fix_op_priority(Options, Options).


rows([], _) --> [].
rows([H|T], Options) -->
    { cells(H, Cells),
      option(align(Alignments), Options)
    },
    html(tr(\row(Cells, Alignments, Options))),
    rows(T, Options).

row([], [], _) --> [].
row([H|T], [AH|AT], Options) -->
    { functor(AH, Align, _),
      (   align_attrs(Align, Atts)
      ->  true
      ;   domain_error(alignment, Align)
      ),
      (   AH =.. [_,Format]
      ->  Options1 = [format(Format)|Options]
      ;   Options1 = Options
      )
    },
    html(td(Atts, \term(H, Options1))),
    row(T, AT, Options).

align_attrs(left, []).
align_attrs(right,  [style('text-align:right')]).
align_attrs(center, [style('text-align:center')]).
align_attrs(l, Attrs) :- align_attrs(left, Attrs).
align_attrs(r, Attrs) :- align_attrs(right, Attrs).
align_attrs(c, Attrs) :- align_attrs(center, Attrs).

cells(Row, Cells) :-
    is_list(Row),
    !,
    Cells = Row.
cells(Row, Cells) :-
    is_dict(Row),
    !,
    dict_pairs(Row, _Tag, Pairs),
    pairs_values(Pairs, Cells).
cells(Row, Cells) :-
    compound(Row),
    compound_name_arguments(Row, _, Cells).

		 /*******************************
		 *            ALIGNMENT		*
		 *******************************/

column_alignment([Row|_], Options0, Options) :-
    partition(is_align, Options0, AlignOptions, Options1),
    member(AlignOption, AlignOptions),
    align(AlignOption, Align),
    generalise(Row, GRow),
    generalise(Align, GRow),
    !,
    cells(Align, List),
    Options = [align(List)|Options1].
column_alignment(Rows, Options0, Options) :-
    partition(is_align, Options0, _AlignOptions, Options1),
    transpose_table(Rows, Cols),
    maplist(col_alignment, Cols, Alignments),
    Options = [align(Alignments)|Options1].

is_align(0) :- !, fail.
is_align(align(_)).
is_align(align=_).

align(align(H), H).
align(align=H, H).



%!  transpose_table(+Table, -Columns)
%
%   Transpose the table to a list of   columns, each column being a list
%   of cells.

transpose_table([], Cols) :-
    maplist(=([]), Cols).
transpose_table([Row|Rows], Cols) :-
    cells(Row, Cells),
    maplist(mkcol, Cells, Cols, Cols1),
    transpose_table(Rows, Cols1).

mkcol(Cell, [Cell|T], T).

col_alignment(Col, right('~D')) :-
    maplist(integer, Col),
    !.
col_alignment(Col, Align) :-
    maplist(non_rational_number, Col),
    !,
    (   maplist(nat_digits, Col, Digits),
        max_list(Digits, N),
        N < 6
    ->  format(atom(Fmt), '~~~df', [N]),
        Align = right(Fmt)
    ;   abs_min_max(Col, Min, Max),
        min_max_format(Min, Max, Align)
    ).
col_alignment(_, left).

non_rational_number(X) :-
    (   integer(X)
    ->  true
    ;   float(X)
    ).

abs_min_max([], 0, 0).
abs_min_max([H|T], Min, Max) :-
    abs_min_max(T, H, Min, H, Max).

abs_min_max([], Min, Min, Max, Max).
abs_min_max([H|T], Min0, Min, Max0, Max) :-
    Min1 is min(abs(H), Min0),
    Max1 is max(abs(H), Max0),
    abs_min_max(T, Min1, Min, Max1, Max).

min_max_format(Min, Max, right(Fmt)) :-
    (   Min =:= 0.0
    ;   Max/Min < 100 000
    ),
    !,
    Digits is round(max(2, 6-log10(Max))),
    format(atom(Fmt), '~~~df', [Digits]).
min_max_format(_, _, left('~q')).

%!  nat_digits(+Number, -Digits)
%
%   Determine the number of digits that   should  naturally be displayed
%   for a number.

nat_digits(F, N) :-
    integer(F),
    !,
    N = 0.
nat_digits(F, N) :-
    format(string(S), '~q', [F]),
    (   (   sub_string(S, Start, _, _, "0000")
        ;   sub_string(S, Start, _, _, "9999")
        ),
        !,
        sub_string(S, SDot, _, _, "."),
        N is Start - SDot - 1
    ;   sub_string(S, _, _, ADot, ".")
    ->  (   sub_string(S, _, _, 0, "0")
        ->  N = 0
        ;   N = ADot
        )
    ).



		 /*******************************
		 *            HEADER		*
		 *******************************/

%!  header(+Table, -Rows, -Header:list(Term), +Options, -RestOptions) is semidet.
%
%   Compute the header to use. Fails if   a  header is specified but
%   does not match.

header(Rows, Rows, _, Options, Options) :-
    \+ option(header(_), Options),
    !.
header(Rows, TRows, ColHead, Options0, Options) :-
    Rows = [Row|TRows0],
    partition(is_header, Options0, HeaderOptions, Options),
    (   member(HeaderOption, HeaderOptions),
        header(HeaderOption, Header),
        Header \== first_row,
        generalise(Row, GRow),
        generalise(Header, GRow)
    ->  header_list(Header, ColHead),
        TRows = Rows
    ;   member(HeaderOption, HeaderOptions),
        header(HeaderOption, Header),
        Header == first_row
    ->  header_list(Row, ColHead),
        TRows = TRows0
    ),
    !.

is_header(0) :- !, fail.
is_header(header(_)).
is_header(header=_).

header(header(H), H).
header(header=H, H).

generalise(List, VList) :-
    is_list(List),
    !,
    length(List, Len),
    length(VList0, Len),
    VList = VList0.
generalise(Compound, VCompound) :-
    compound(Compound),
    !,
    compound_name_arity(Compound, Name, Arity),
    compound_name_arity(VCompound0, Name, Arity),
    VCompound = VCompound0.

header_list(List, List) :- is_list(List), !.
header_list(Compound, List) :-
    Compound =.. [_|List].


%!  header_row(ColNames:list)// is det.
%
%   Include a header row  if ColNames is not unbound.

header_row(ColNames) -->
    { var(ColNames) },
    !.
header_row(ColNames) -->
    html(tr(class(hrow), \header_columns(ColNames))).

header_columns([]) --> [].
header_columns([H|T]) -->
    html(th(\term(H, []))),
    header_columns(T).


%!  is_list_of_terms(@Term, -Rows, -Cols) is semidet.
%
%   Recognises a list of terms with   the  same functor and non-zero
%   arity.

is_list_of_terms(Term, Rows, Cols) :-
    is_list(Term), Term \== [],
    length(Term, Rows),
    maplist(is_term_row(_Name, Cols), Term),
    Cols > 0.

is_term_row(Name, Arity, Term) :-
    compound(Term),
    compound_name_arity(Term, Name, Arity).

%!  is_list_of_dicts(@Term, -Rows, -ColNames) is semidet.
%
%   True when Term is a list of Rows dicts, each holding ColNames as
%   keys.

is_list_of_dicts(Term, Rows, ColNames) :-
    is_list(Term), Term \== [],
    length(Term, Rows),
    maplist(is_dict_row(ColNames), Term).

is_dict_row(ColNames, Dict) :-
    is_dict(Dict),
    dict_keys(Dict, ColNames).

%!  is_list_of_lists(@Term, -Rows, -Cols) is semidet.
%
%   Recognise a list of lists of equal length.

is_list_of_lists(Term, Rows, Cols) :-
    is_list(Term), Term \== [],
    length(Term, Rows),
    maplist(is_list_row(Cols), Term),
    Cols > 0.

is_list_row(Length, Term) :-
    is_list(Term),
    length(Term, Length).

