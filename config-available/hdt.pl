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

:- module(swish_config_hdt, []).
:- use_module(library(broadcast)).
:- use_module(swish(lib/plugin/hdt)).
:- use_module(swish:swish(lib/plugin/hdt)).
:- use_module(library(semweb/rdf11),
              [ rdf_register_prefix/2,
                rdf_global_id/2,
                (rdf_meta)/1
              ]).
:- use_module(swish(lib/render/rdf), []).

:- multifile user:file_search_path/2.

user:file_search_path(hdt, '../hdt-data').

:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- rdf_meta(prefix(r, r)).

rdf_prefix(hdt:geonames,      gn,    'http://www.geonames.org/').
rdf_prefix(hdt:geonames,      gno,   'http://www.geonames.org/ontology#').
rdf_prefix(hdt:linkedgeodata, lgo,   'http://linkedgeodata.org/ontology/').
rdf_prefix(hdt:wordnet,       wn,    'http://wordnet-rdf.princeton.edu/wn31/').
rdf_prefix(hdt:wordnet,       wno,   'http://wordnet-rdf.princeton.edu/ontology#').
rdf_prefix(hdt:wordnet,       lemon, 'http://lemon-model.net/lemon#').

register_prefixes :-
    forall(rdf_prefix(_G, Prefix, IRI),
           rdf_register_prefix(Prefix, IRI)).

:- listen(rdf_label(IRI, Label), label(IRI, Label)).

label(IRI, Label) :-
    rdf_global_id(Prefix:_Local, IRI),
    rdf_prefix(Graph, Prefix, _),
    rdf(IRI, rdfs:label, Label@en, Graph).

:- initialization
    register_prefixes.
