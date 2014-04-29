/* -*- Mode: Prolog -*- */

:- ensure_loaded(prolog/sysbio/bp2lego).
:- use_module(prolog/sysbio/biopax_util).
:- use_module(prolog/sysbio/lego_ns).
:- use_module(prolog/sysbio/ro).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdfs)).

% to generate:
% http://www.reactome.org/ReactomeRESTfulAPI/RESTfulWS/biopaxExporter/Level3/109607

% ========================================
% test
% ========================================

displayOrStandardName(X,N) :-
        displayName(X,N),!.
displayOrStandardName(X,N) :-
        standardName(X,N),!.


pathway_graph_name(G,Name) :-
        rdf_graph(G),
        displayOrStandardName(G,Name).


save_graphs :-
        forall(pathway_graph_name(G,N),
               (   save_graph(G,N))).

save_graph(N) :-
        displayName(G,N),
        !,
        save_graph(G,N).
save_graph(G) :-
        displayName(G,N),
        !,
        save_graph(G,N).


save_graph(G,N) :-
        Dir='target/graphs/',
        (   exists_directory(Dir)
        ->  true
        ;   make_directory(Dir)),
        safe(N,N2),
        concat_atom([Dir,/,N2,'.owl'],Path),
        debug(mago,'saving to ~w',[Path]),
        rdf_save(Path,[graph(G)]).

load_bp(File) :-
        set_prolog_flag(verbose,normal),
        debug(mago),
        debug(rdft),
        debug(rdf(load)),
        ensure_loaded(library(semweb/rdf_cache)),
        rdf_set_cache_options(enabled(true)),
        rdf_set_cache_options(create_local_directory(true)),
        rdf_load(File).

load_hsap :-
        load_bp('t/data/Homo-sapiens.owl').

load_but :-
        load_bp('t/data/metacyc-pwy-6886-bp3.owl'),
        rdf_load('ontologies/go-equivs.owl').

load_apop :-
        load_bp('t/data/ExtrinsicApoptosis.ttl'),
        rdf_load('ontologies/go-equivs.owl').

t_but :-
        load_but,
        msave_all.


msave_all :-
        materialize_views,
        extract_pathways,
        save_graphs.

t :-
        load_apop,
        materialize_views,
        rdf_save('target/apop.owl',[graph(lego)]),
        extract_pathways,
        save_graphs,
        %save_graph('Acetylcholine Neurotransmitter Release Cycle'),
        prolog.

t1 :-
        load_hsap,
        materialize_views,
        rdf_save('target/hs-mago.owl',[graph(lego)]),
        extract_pathways,
        save_graphs,
        %save_graph('Acetylcholine Neurotransmitter Release Cycle'),
        prolog.

t2 :-
        load_hsap,
        prolog.

t3 :-
        load_hsap,
        materialize_views,
        %rdf_save('target/hs-mago.owl',[graph(lego)]),
        extract_pathways,
        %save_graphs,
        %save_graph('Metabolism of Angiotensinogen to Angiotensins'),
        save_graph('Metabolism of proteins'),
        prolog.


        
safe(N,Safe) :-
        atom_chars(N,Cs),
        lsafe(Cs,Cs2),
        atom_chars(Safe,Cs2).

lsafe([],[]).
lsafe([H|L],[H2|L2]) :-
        csafe(H,H2),
        lsafe(L,L2).

csafe(' ','_') :- !.
csafe('-','_') :- !.
csafe('_','_') :- !.
csafe(C,C) :-
        C @>= 'a',
        C @=< 'z',
        !.
csafe(C,C) :-
        C @>= 'A',
        C @=< 'Z',
        !.
csafe(C,C) :-
        C @>= '0',
        C @=< '9',
        !.
csafe(_,'_').
