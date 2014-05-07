/* -*- Mode: Prolog -*- */

:- begin_tests(cvt).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).

:- ensure_loaded('prolog/sysbio/bp2lego').
:- ensure_loaded('prolog/sysbio/lego_ns').
:- ensure_loaded('prolog/sysbio/render/sbtext').

cvt(File,G) :-
        rdf_load('ontologies/go.owl'),
        atom_concat('t/data/',File,Path),
        rdf_load(Path,[graph(test)]),
        %materialize_views(G,true),
        debug(test,'Loaded',[]),
        %anonymize_non_processes(lego),
        convert_biopax_to_lego(lego,true),
        debug(test,'Anonymizied',[]).


        

w(S,P,O) :-
        rdf_global_id(PP,P),
        format('  ~w -[~w]-> ~w~n',[S,PP,O]).

test(convert) :-
        debug(test),
        debug(sbtext),
        debug(render),
        debug(bp2lego),
        cvt('ExtrinsicApoptosis.ttl',lego),
        %forall(rdf(S,P,O,lego),
        %       w(S,P,O)),
        check,
        debug(test,'rendering...',[]),
        write_model,
        rdf_save('target/apoptosis-lego.owl',[graph(lego)]),
        true.

% NOTE: in reactome, multiple levels all linked to the same biological process
check :-
        rdf(_,rdf:type,'obo':'GO_0097190').
foo(0). %'

:- end_tests(cvt).
