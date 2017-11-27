/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(swish_render_wordnet,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
:- use_module(library(http/html_write)).
:- use_module('../render').
:- use_module(library(wn)).			% from wordnet pack

:- register_renderer(wordnet, "Show WordNet synsets").

/** <module> SWISH wordnet renderer

Renders a WordNet synset-id (integer) as a list of words.
*/

%%	term_rendering(+Synset, +Vars, +Options)//
%
%	Renders  a Synset as a list of words.

term_rendering(Synset, _Vars, _Options) -->
	{   integer(Synset),
            Synset > 100000000,
            Synset < 500000000,
            findall(Word, wn_s(Synset, _, Word, _, _, _), Words),
            Words \== [],
            (   wn_g(Synset, Gloss)
            ->  Attr = [title(Gloss)]
            ;   Attr = []
            )
	},
        html(span([class(synset)|Attr],
                  [ span(class('synset-id'), Synset), ' (WN: ',
                    \words(Words), ')'
                  ])).

words([]) --> [].
words([H|T]) -->
    word(H),
    (   {T == []}
    ->  []
    ;   html(', '),
        words(T)
    ).

word(H) -->
    html(span(class('wn-word'), H)).
