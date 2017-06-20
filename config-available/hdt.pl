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
:- use_module(library(semweb/rdf11),
              [ rdf_register_prefix/2,
                rdf_global_id/2,
                (rdf_meta)/1
              ]).
:- use_module(swish(lib/render/rdf), []).

/** <module> Make background RDF data available using HDT

This config file makes possibly very large amounts of RDF data available
from swish using the Header Dictionary Triple (HDT) format. Steps to use
this:

  1. Install an activate the HDT library using the commands below.  This
     should work on Ubuntu out of the box.  On other Unix systems you
     may need to tweak a little.  Installing on Windows is most likely
     a challenge.

     ```
     sudo apt-get install libserd-dev libraptor-dev
     git submodule update --init pack/hdt
     swipl run.pl
     ?- pack_rebuild(hdt).
     ```

  3. Create a directory with HDT files in them you want to make
     available.  You can later add files without restarting SWISH.

  4. Copy this file to `config-enabled` and go through the `EDIT`
     marked sections to configure HDT for your data.
*/

:- multifile user:file_search_path/2.

% EDIT: location of your HDT files
user:file_search_path(hdt, '../hdt-data').

% EDIT: uncomment to make hdt available without including the library
% :- use_module(swish:swish(lib/plugin/hdt)).

% EDIT: Define common RDF prefixes
:- rdf_register_prefix(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- rdf_meta(prefix(r, r)).

% EDIT: define prefixes used in the various HDT datasets.  The RDF
% rendering library will use to display the (english) label for
% resources when available.
rdf_prefix(hdt:geonames,      gn,    'http://www.geonames.org/').
rdf_prefix(hdt:geonames,      gno,   'http://www.geonames.org/ontology#').
rdf_prefix(hdt:linkedgeodata, lgo,   'http://linkedgeodata.org/ontology/').
rdf_prefix(hdt:wordnet,       wn,    'http://wordnet-rdf.princeton.edu/wn31/').
rdf_prefix(hdt:wordnet,       wno,   'http://wordnet-rdf.princeton.edu/ontology#').
rdf_prefix(hdt:wordnet,       lemon, 'http://lemon-model.net/lemon#').
rdf_prefix(hdt:dblp,          swrc,  'http://swrc.ontoware.org/ontology#').
rdf_prefix(hdt:wikidata,      wdp,   'http://www.wikidata.org/prop/').
rdf_prefix(hdt:wikidata,      wde,   'http://www.wikidata.org/entity/').
rdf_prefix(hdt:wikidata,      wds,   'http://www.wikidata.org/prop/statement/').
rdf_prefix(hdt:wikidata,      wdt,   'http://www.wikidata.org/prop/direct/').

register_prefixes :-
    forall(rdf_prefix(_G, Prefix, IRI),
           rdf_register_prefix(Prefix, IRI)).

:- listen(rdf_label(IRI, Label), label(IRI, Label)).

% EDIT: Define how to obtain the label for an IRI. The default is to use
% the rules above to find the HDT from   which the IRI originates and as
% this HDT for rdfs:label in English.
label(IRI, Label) :-
    rdf_global_id(Prefix:_Local, IRI),
    rdf_prefix(Graph, Prefix, _),
    rdf(IRI, rdfs:label, @(Label,en), Graph).

:- initialization
    register_prefixes.
