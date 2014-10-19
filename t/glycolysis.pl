/* -*- Mode: Prolog -*- */

:- begin_tests(cvt).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).

:- ensure_loaded('prolog/sysbio/bp2lego').
:- ensure_loaded('prolog/sysbio/lego_ns').
:- ensure_loaded('prolog/sysbio/ro').
:- ensure_loaded('prolog/sysbio/render/sbtext').
:- ensure_loaded('test_util').

test(convert) :-
        debug(test),
        debug(sbtext),
        debug(bp2lego),
        cvt('Glycolysis2.owl',lego),
        %forall(rdf(S,P,O,lego),
        %       w(S,P,O)),
        check,
        debug(test,'rendering...',[]),
        write_model,
        rdf_save('target/glycolysis-lego.owl',[graph(lego)]),
        true.

lit2atom(literal(type(_,A)),A).
lit2atom(literal(A),A) :- atom(A).

occurs_in_some(X,Label) :-
        rdfs_individual_of(X,BN),
        rdf(BN,owl:onProperty,occurs_in:''),
        rdf(BN,owl:someValuesFrom,Y),
        rdf(Y,rdfs:label,LabelLit),
        lit2atom(LabelLit,Label).

owltype(some(P,Y),X) :-
        rdfs_individual_of(X,BN),
        rdf(BN,owl:onProperty,P),
        rdf(BN,owl:someValuesFrom,Y).

ltype(Label,X) :-
        rdf(Type,rdfs:label,LabelLit),
        lit2atom(LabelLit,Label),
        rdfs_individual_of(X,Type).

check :-
        ltype('glucose-6-phosphate isomerase activity',R1),
        part_of(R1,P1),
        ltype('glycolytic process',P1),
        rdf_reachable(R1,directly_provides_input_for:'',R2),
        ltype('pyruvate kinase activity',R2),
        occurs_in_some(R2,cytosol).

:- end_tests(cvt).

