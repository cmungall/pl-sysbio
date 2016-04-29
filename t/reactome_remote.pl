/* -*- Mode: Prolog -*- */

:- begin_tests(cvt).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).

:- ensure_loaded('prolog/sysbio/bp2lego').
:- ensure_loaded('prolog/sysbio/lego_ns').
:- ensure_loaded('prolog/sysbio/reactome_util').
:- ensure_loaded('prolog/sysbio/render/sbtext').

cvt(ID,G) :-
        rdf_load('ontologies/go.owl'),
        remote_load_reactome(ID,[fresh(true),lego(G),cleanup(true)]),
        debug(test,'Done',[]).

        

w(S,P,O) :-
        rdf_global_id(PP,P),
        format('  ~w -[~w]-> ~w~n',[S,PP,O]).

test(convert) :-
        debug(test),
        debug(sbtext),
        debug(bp2lego),
        debug(reactome),
        cvt(109581,lego),
        check,
        debug(test,'rendering...',[]),
        write_model,
        rdf_save('target/apoptosis-lego.owl',[graph(lego)]),
        true.

% NOTE: in reactome, multiple levels all linked to the same biological process
check :-
        rdf(_,rdf:type,'obo':'GO_0097190').
foo(0). %'

% http://www.reactome.org/PathwayBrowser/#DIAGRAM=168898&PATH=168256,168249
test(tlr) :-
        debug(test),
        debug(sbtext),
        debug(bp2lego),
        debug(reactome),
        cvt(9047,lego),
        check,
        debug(test,'rendering...',[]),
        write_model,
        rdf_save('target/tlr9-lego.owl',[graph(lego)]),
        true.

:- end_tests(cvt).
