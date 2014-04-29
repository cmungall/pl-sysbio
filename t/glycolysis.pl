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
        materialize_views(G,true),
        debug(test,'Loaded',[]).
        



        

w(S,P,O) :-
        rdf_global_id(PP,P),
        format('  ~w -[~w]-> ~w~n',[S,PP,O]).

test(convert) :-
        debug(test),
        debug(sbtext),
        cvt('Glycolysis.ttl',lego),
        %forall(rdf(S,P,O,lego),
        %       w(S,P,O)),
        debug(test,'rendering...',[]),
        write_model,
        true.


:- end_tests(cvt).
