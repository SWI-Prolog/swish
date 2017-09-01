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

:- module(swish_attvar,
          [ put_attr/2                  % +Var, :Attr.
          ]).
:- meta_predicate
    put_attr(-, :).

%!  put_attr(+Var, :Value) is det.
%
%   Put an attribute on  the  current  module.   This  is  the  same  as
%   put_attr(Var, Module, Value), where Module is the calling context.

put_attr(Var, M:Value) :-
    put_attr(Var, M, Value).

:- multifile sandbox:safe_meta/3.

sandbox:safe_meta(swish_attvar:put_attr(Var,Value), Context, Called) :-
    Value \= _:_,
    !,
    attr_hook_predicates([ attr_unify_hook(Value, _),
                           attribute_goals(Var,_,_),
                           project_attributes(_,_)
                         ], Context, Called).


attr_hook_predicates([], _, []).
attr_hook_predicates([H|T], M, Called) :-
    (   predicate_property(M:H, defined)
    ->  Called = [M:H|Rest]
    ;   Called = Rest
    ),
    attr_hook_predicates(T, M, Rest).

