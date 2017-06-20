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

:- module(swish_hdt,
          [ rdf/4,                      % ?Subject, ?Pred, ?Object, ?Graph

            rdf_subject/2,              % ?Subject, ?Graph
            rdf_predicate/2,            % ?Predicate, ?Graph
            rdf_object/2,               % ?Object, ?Graph
            rdf_shared/2,               % ?IRI, ?Graph

            rdf_suggestions/5,		% +Base, +Role, +MaxCount, -List, +Graph
            rdf_graph_property/2,	% -Property, +Graph

            rdf_meta/1,                 % +Declarations
            rdf_prefix/2,               % +Prefix, +URL

            op(110, xfx, @),            % must be above .
            op(650, xfx, ^^),           % must be above :
            op(1150, fx, rdf_meta)

          ]).
:- use_module(library(hdt)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf11),
              except([ rdf/4,
                       rdf_graph_property/2
                     ])).

:- rdf_register_prefix(hdt, 'hdt://data/').
:- rdf_meta
    rdf(r,r,o,r),
    rdf_subject(r,r),
    rdf_predicate(r,r),
    rdf_object(r,r),
    rdf_shared(r,r),
    rdf_suggestions(+,+,+,-,r),
    rdf_graph_property(-,r).

/** <module> Access RDF through HDTs
*/

%!  rdf(?S,?P,?O,+G)
%
%   True when <S,P,O> is a  triple  in   G.  G  typically takes the form
%   `hdt:Name`, searching the HDT file Name.hdt

rdf(S,P,O,G) :-
    graph_hdt(G, HDT),
    hdt_search(HDT, S, P, O).

%!  rdf_subject(?S,+G)
%
%   True when S is a subject in the graph G.

rdf_subject(S,G) :-
    graph_hdt(G, HDT),
    hdt_subject(HDT, S).

%!  rdf_predicate(?P,+G)
%
%   True when P is a predicate in the graph G.

rdf_predicate(P,G) :-
    graph_hdt(G, HDT),
    hdt_predicate(HDT, P).

%!  rdf_object(?O,+G)
%
%   True when O is a object in the graph G.

rdf_object(O,G) :-
    graph_hdt(G, HDT),
    hdt_object(HDT, O).

%!  rdf_shared(?IRI,+G)
%
%   True when IRI is both a subject and object in the graph G.

rdf_shared(S,G) :-
    graph_hdt(G, HDT),
    hdt_shared(HDT, S).

%!  rdf_suggestions(+Base, +Role, +MaxCount, -List, +Graph)
%
%   True when Results is a list of   suggestions  for Base in the triple
%   role Role. Some experimentation suggests it  performs a prefix match
%   on the internal string representation.   This  implies that literals
%   are only found if the first character of Base is `"`.
%
%   @arg Base is a string or atom
%   @arg Role is one of `subject`, `predicate` or `object`

rdf_suggestions(Base, Role, MaxCount, List, Graph) :-
    graph_hdt(Graph, HDT),
    hdt_suggestions(HDT, Base, Role, MaxCount, List).

%!  rdf_graph_property(-Property, +Graph) is nondet.
%
%   True when Property is a property of Graph

rdf_graph_property(Property, Graph) :-
    graph_hdt(Graph, HDT),
    hdt_property(HDT, Property).


:- dynamic
    hdt_graph/2.

graph_hdt(G, HDT) :-
    must_be(ground, G),
    hdt_graph(G, HDT0),
    !,
    HDT = HDT0.
graph_hdt(hdt:Local, HDT) :-
    !,
    rdf_global_id(hdt:Local, Global),
    graph_hdt(Global, HDT).
graph_hdt(G, HDT) :-
    atom_concat('hdt://data/', File, G),
    \+ sub_atom(File, _, _, _, '..'),
    absolute_file_name(hdt(File), Path,
                       [ extensions([hdt]),
                         access(read)
                       ]),
    hdt_open(HDT0, Path),
    assertz(hdt_graph(G, HDT0)),
    HDT = HDT0.

:- multifile sandbox:safe_primitive/2.

sandbox:safe_primitive(swish_hdt:graph_hdt(_,_)).
sandbox:safe_primitive(hdt:hdt_search(_,_,_,_)).
sandbox:safe_primitive(system:hdt_column_(_,_,_)).
sandbox:safe_primitive(system:hdt_suggestions(_,_,_,_,_)).
sandbox:safe_primitive(system:hdt_property_(_,_)).
