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

%!  projection(+Spec:list)
%
%   Specify the result variables. Using projection/1   at the start of a
%   query specifies which variables are part of  the result set, in what
%   order they are displayed and, optionally,   whether the results must
%   be ordered on one or  more   variables.  Ordering is specified using
%   `+Var` (ascending) or `-Var` (descending).  If ordering is specified
%   for multiple variables, the result set  is ordered starting with the
%   left-most variable for which ordering is defined.

projection(_).

swish:goal_expansion((Projection,Body), Ordered) :-
    nonvar(Projection),
    Projection = projection(Spec),
    must_be(list, Spec),
    phrase(order(Spec, Vars), Order),
    Order \== [],
    Ordered = order_by(Order, Body),
    ignore(set_projection(Vars)).
swish:goal_expansion(projection(Vars), true) :-
    set_projection(Vars).

set_projection(Vars) :-
    nb_current('$variable_names', Bindings),
    debug(projection, 'Got ~p; Asking ~p', [Bindings, Vars]),
    memberchk('_residuals'=Var, Bindings),
    maplist(select_binding(Bindings), Vars, NewBindings),
    debug(projection, 'Filtered ~p', [NewBindings]),
    b_setval('$variable_names', ['_residuals'=Var|NewBindings]).

select_binding(Bindings, Var, Name=Var) :-
    member(Name=X, Bindings),
    Var == X,
    !.

order([], []) -->
    [].
order([H|T], [V|VT]) -->
    order1(H, V),
    order(T, VT).

order1(V, V)  --> {var(V)}, !.
order1(+V, V) --> !, [asc(V)].
order1(-V, V) --> !, [desc(V)].
order1(V, V)  --> [].

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(swish_projection:projection(_)).
