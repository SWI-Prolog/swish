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

:- module(swish_projection,
          [ projection/1
          ]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).

/** <module> Define the projection

This module redefines variables that are   included  in the SWISH result
set.

@tbd	Removed variables should be removed from the template as well
	for efficiency reasons.
*/

:- multifile
    reserved_var/1.                             % +VarName

%!  projection(+Spec:list)
%
%   Specify the result variables. Using projection/1   at the start of a
%   query specifies which variables are part of  the result set, in what
%   order they are displayed and, optionally,   whether the results must
%   be ordered on one or  more  variables   or  the  solutions should be
%   distinct.  Each element of Spec is one of the following:
%
%     - Var
%     Include Var in the result.  Var must appear in the remainder of
%     the body.
%     - Var:Annotation
%     As Var, respecting Annotation. Valid annotations are below.
%     Annotations may be abbreviated, e.g. `asc`, `desc`
%
%       - ascending
%       Order solutions in ascending order.
%       - descending
%       Order solutions is descending order
%       - distinct
%       Remove duplicates wrt. this argument.
%       - AnnA+AnnB
%       Multiple annotations
%
%     - +Var
%     Equivalent to `Var:ascending`
%     - -Var
%     Equivalent to `Var:descending`
%
%   If ordering is specified for multiple   variables, the result set is
%   ordered starting with the left-most variable   for which ordering is
%   defined.

projection(_).

swish:goal_expansion((Projection,Body), Aggregate) :-
    nonvar(Projection),
    Projection = projection(Spec),
    must_be(list, Spec),
    aggregation(Spec, Vars, Body, Aggregate),
    !,
    ignore(set_projection(Vars)).
swish:goal_expansion(projection(Vars), true) :-
    set_projection(Vars).

set_projection(Vars) :-
    nb_current('$variable_names', Bindings),
    debug(projection, 'Got ~p; Asking ~p', [Bindings, Vars]),
    preverse_vars(Bindings, NewVars, SelectedBindings),
    maplist(select_binding(Bindings), Vars, SelectedBindings),
    debug(projection, 'Filtered ~p', [NewVars]),
    b_setval('$variable_names', NewVars).

%!  preverse_vars(+Bindings, -ReservedBindings, ?Tail) is det.
%
%   Preserve some of the _pseudo   bindings_ that communicate additional
%   information from the Pengine. May be   extended by adding clauses to
%   reserved_var/1.

preverse_vars([], L, L).
preverse_vars([Name=Var|T0], [Name=Var|T], L) :-
    reserved_var(Name),
    !,
    preverse_vars(T0, T, L).
preverse_vars([_|T0], T, L) :-
    preverse_vars(T0, T, L).

reserved_var('_residuals').
reserved_var('_swish__permahash').

select_binding(Bindings, Var, Name=Var) :-
    member(Name=X, Bindings),
    Var == X,
    !.

%!  aggregation(+Projection:list, -Vars, +Goal0, -Goal) is semidet.
%
%   Determine the final projection variables as well as ordering and
%   distinct wrapper from the projection argument.

aggregation(Projection, Vars, Goal0, Goal) :-
    modifiers(Projection, Vars, Unique, Order),
    munique(Unique, Goal0, Goal1),
    morder(Order, Goal1, Goal).

munique([], Goal, Goal) :-
    !.
munique(Vars, Goal0, distinct(Term, Goal0)) :-
    Term =.. [v|Vars].

morder([], Goal, Goal) :-
    !.
morder(Vars, Goal0, order_by(Vars, Goal0)).

modifiers(Projection, Vars, Unique, Order) :-
    phrase(annotations(Projection, Vars), Annot),
    Annot \== [],
    partition(unique_anot, Annot, Unique, Order).

unique_anot(distinct(_)).

annotations([], []) -->
    [].
annotations([H|T], [V|VT]) -->
    annotations1(H, V),
    annotations(T, VT).

annotations1(V, V)  --> {var(V)}, !.
annotations1(+V, V) --> !, [asc(V)].
annotations1(-V, V) --> !, [desc(V)].
annotations1(V:Ann, V) --> !, var_annotations(Ann, V).
annotations1(V, V)  --> [].

var_annotations(Var, _) --> {var(Var), instantiation_error(Var)}.
var_annotations(A+B, V) --> !, var_annotations(A,V), var_annotations(B,V).
var_annotations(Anot, V) -->
    { var_annotation(Anot, Can),
      Term =.. [Can,V]
    },
    [ Term ].

var_annotation(Anot, Cann) :-
    var_anot1(Anot, Cann),
    !,
    (   var_anot1(Anot, Cann2),
        Cann \== Cann2
    ->  domain_error(projection_annotation, Anot)
    ;   true
    ).
var_annotation(Anot, _Cann) :-
    domain_error(projection_annotation, Anot).

var_anot1(Anot, Cann) :-
    var_annot(Long, Cann),
    sub_atom(Long, 0, _, _, Anot).

var_annot(ascending,  asc).
var_annot(descending, desc).
var_annot(distinct,   distinct).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_projection:projection(_)).
