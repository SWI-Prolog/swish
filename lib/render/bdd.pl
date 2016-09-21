/*  Part of SWISH

    Author:        Markus Triska
    E-mail:        triska@metalevel.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016 Markus Triska
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

:- module(swish_render_bdd,
          [ term_rendering//3                   % +Term, +Vars, +Options
          ]).
:- use_module('../render').
:- use_module(graphviz, []).
:- use_module(library(clpb)).
:- use_module(library(varnumbers)).

:- register_renderer(bdd, "Render BDDs corresponding to CLP(B) constraints").

/** <module> Render Binary Decision Diagrams (BDDs)

This renderer draws Binary Decision Diagrams (BDDs) that express
CLP(B) constraints.

Both kinds of residual goals that `library(bdd)` can emit are
supported by this renderer: By default, `library(clpb)` emits `sat/1`
residual goals. These are first translated to BDDs by reposting the
goals while temporarily setting `clpb_residuals` to `bdd`. The
translation of such BDDs to `dot/1` terms is straight-forward.
Alternatively, you can set `clpb_residuals` to `bdd` yourself. In that
case, residual goals are directly emitted as BDDs and are again
translated to `dot/1` terms by this renderer.

In both cases, the graphviz renderer is used for the final output.

*/

%%      term_rendering(+Term, +Vars, +Options)//
%
%       Renders BDDs as emitted by library(clpb).

term_rendering(Goal, Vars, Options) -->
        (   { Goal = clpb:'$clpb_bdd'(Ns),
              is_list(Ns) } -> []
        ;   { Goal = sat(Sat0) } ->
            { current_prolog_flag(clpb_residuals, OldFlag),
              varnumbers_names(Sat0, Sat, VNames),
              term_variables(Sat, Vs),
              setup_call_cleanup(set_prolog_flag(clpb_residuals, bdd),
                                 catch((sat(Sat),
                                        copy_term(Vs, Vs, [clpb:'$clpb_bdd'(Ns)]),
                                        % reset all attributes
                                        maplist(del_attrs, Vs),
                                        throw(nodes(Vs,Ns))),
                                       % Vs recreate the bindings in nodes Ns
                                       nodes(Vs,Ns),
                                       true),
                                 set_prolog_flag(clpb_residuals, OldFlag)),
              % for displaying the labels, unify each variable with its name
              maplist(eq, VNames) }
        ),
        { nodes_dot_digraph(Ns, Dot) },
        swish_render_graphviz:term_rendering(Dot, Vars, Options).

eq(Name=Var) :- Name = Var.

nodes_dot_digraph(Nodes, dot(digraph(Stmts))) :-
        maplist(node_then_else, Nodes, Thens, Elses),
        phrase((nodes_labels(Nodes),
                [node(false, [fontname='Palatino-Bold']),
                 node(true, [fontname='Palatino-Bold'])],
                [edge([style='filled'])],
                list(Thens),
                [edge([style='dotted'])],
                list(Elses)), Stmts).

nodes_labels([]) --> [].
nodes_labels([node(N)-(v(Var,_) -> _ ; _)|Nodes]) -->
        [node(N, [label=Var])],
        nodes_labels(Nodes).

node_then_else(Node-(_->Then0;Else0), (ID->Then), (ID->Else)) :-
        maplist(node_id, [Node,Then0,Else0], [ID,Then,Else]).

node_id(node(N), N).
node_id(true, true).
node_id(false, false).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).
