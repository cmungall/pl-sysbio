/* -*- Mode: Prolog -*- */

:- module(test_util,
          [cvt/2]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).

:- ensure_loaded('prolog/sysbio/bp2lego').
:- ensure_loaded('prolog/sysbio/lego_ns').
:- ensure_loaded('prolog/sysbio/render/sbtext').

cvt(File,G) :-
        rdf_load('ontologies/go.owl'),
        rdf_load('ontologies/ro.owl'),
        atom_concat('t/data/',File,Path),
        rdf_load(Path,[graph(test)]),
        debug(test,'Loaded',[]),
        convert_biopax_to_lego(lego),
        debug(test,'Anonymizied',[]).
