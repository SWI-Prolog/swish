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

:- module(swish_data_sparql, []).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(option)).

:- use_module('../data_source').

/** <module> Data source handler for SPARQL endpoints

This handler deals with from a  SPARQL   endpoint.  Given a SELECT query
with variables Name1, Name2, ... it creates   a data source with columns
Name1, Name1_t, Name2, Name2_t, ... The   `_t` column contains the type,
which is either `iri`, a  language  name   or  a  type  name. Values are
translated  into  their  Prolog  native    representation  according  to
library(semweb/rdf11).
*/

:- multifile
    swish_data_source:source/2.

swish_data_source:source(sparql(Query, Options),
                         swish_data_sparql:import_sparql(Query, Options)).


		 /*******************************
		 *         SPARQL IMPORT	*
		 *******************************/

:- public import_sparql/3.

import_sparql(Query, Options, Hash) :-
    add_varnames(Options, VarNames, Options1),
    State = state(-),
    setup_call_catcher_cleanup(
        true,
        once(( forall(sparql_query(Query, Row, Options1),
                      assert_row(Hash, Row, State, VarNames)),
               arg(1, State, Signature),
               Signature \== (-)
             )),
        Catcher,
        finalize(Catcher, Hash, State, import_sparql(Query, Options))).

finalize(exit, Hash, state(Signature), _Action) :-
    'data materialized'(Hash, Signature, version{}).
finalize(Reason, Hash, state(Signature), Action) :-
    (   Signature == (-)
    ->  true
    ;   'data failed'(Hash, Signature)
    ),
    (   Reason = exception(Ex)
    ->  throw(Ex)
    ;   throw(error(failure_error(Action)))
    ).


add_varnames(Options, VarNames, Options) :-
    option(variable_names(VarNames), Options),
    !.
add_varnames(Options, VarNames, [variable_names(VarNames)|Options]).

assert_row(Hash, Row, State, VarNames) :-
    create_signature(State, Hash, VarNames),
    Row =.. [_|Values],
    make_row(Values, Columns),
    DBRow =.. [Hash|Columns],
    'data assert'(DBRow).

make_row([], []).
make_row([V|VT], [Value,Type|RT]) :-
    type_value(V, Type, Value),
    make_row(VT, RT).

type_value(literal(Literal), Type, Value) :-
    !,
    literal_type_value(Literal, Type, Value).
type_value(IRI, iri, IRI).

literal_type_value(lang(Lang, Text), Lang, String) :-
    !,
    atom_string(Text, String).
literal_type_value(type(Type, Text), Type, Val) :-
    !,
    (   catch(rdf11:out_type(Type, Val, Text), _, fail)
    ->  true
    ;   atom_string(Text, Val)
    ).
literal_type_value(Plain, Type, Value) :-
    (   atom_number(Plain, Value)
    ->  (   integer(Value)
        ->  rdf_equal(Type, xsd:integer)
        ;   rdf_equal(Type, xsd:double)
        )
    ;   atom_string(Plain, Value),
        rdf_equal(Type, xsd:string)
    ).

create_signature(State, Hash, VarNames) :-
    State == state(-),
    !,
    add_types(VarNames, ColNames),
    Signature =.. [Hash|ColNames],
    nb_setarg(1, State, Signature).
create_signature(_, _, _).

add_types([], []).
add_types([H|T0], [H,Type|T]) :-
    atom_concat(H, '_t', Type),
    add_types(T0, T).
