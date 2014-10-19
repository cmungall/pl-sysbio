/* -*- Mode: Prolog -*- */

/**
  * Ontology tools
  *
  * May be moved to different repo/lib
  *
  */

:- module(onty,
          [
           edge/3,
           edge/4,
           node/1,
           node/2,
           node_prop/3,
           node_prop/4,
           translate_egraph/0,
           translate_egraph/1,
           translate_egraph/2,
           materialize_entailments/2
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(lego_ns).

:- rdf_meta edge(r,r,r,?).
:- rdf_meta edge(r,r,r).
:- rdf_meta node(r,?).
:- rdf_meta node(r).
:- rdf_meta node_prop(r,?,?,?).
:- rdf_meta node_prop(r,?,?).

%% edge(?S,?P,?O) is semidet
%% edge(?S,?P,?O,?G) is semidet
%
% Wrapper for rdf/4 and rdf/3, exclude
% bnodes and literals
%
edge(S,P,O) :-
        edge(S,P,O,_).
edge(S,P,O,G) :-
        rdf(S,P,O,G),
        rdf_is_resource(O),
        \+ rdf_is_bnode(S),
        \+ rdf_is_bnode(O).

%% node(?S) is semidet
%% node(?S,?G) is semidet
%
% True is S is a node (in G if specified)
%
% Excludes blank nodes
%
node(N) :-
        setof(N,P^O^rdf(N,P,O),Ns),
        member(N,Ns),
        \+ rdf_is_bnode(N).
node(N,G) :-
        setof(N,P^O^rdf(N,P,O,G),Ns),
        member(N,Ns),
        \+ rdf_is_bnode(N).

%% node_prop(?S,?P,?V,?G) is semidet
%% node_prop(?S,?P,?V) is semidet
%
% true is V is a literal and there is an
% edge of type P from S to V
%
node_prop(S,P,V) :-
        node_prop(S,P,V,_).
node_prop(S,P,V,G) :-
        rdf(S,P,literal(V),G),
        \+ rdf_is_bnode(S).
        

%% translate_egraph(+SG,+TG) is det
%
% An OWL ontology is encoded in RDF such that
% existentials generate bNodes
%
% This goal will transform all
% ==
% SubClassOf(S SomeValuesFrom(P O))
%   =>
% rdf(S,P,O)
% ==
%
% Note that this is not a valid translation,
% incorrect entailments may be generated when OWL
% reasoning, particular inverses and symmetry
%
% TODO: option to generate new predicates
%
translate_egraph :-
        translate_egraph(_,_).
translate_egraph(TG) :-
        translate_egraph(_,TG).
translate_egraph(SG,TG) :-
        forall((subclass_of_svf(S,P,O,SG),target_graph(SG,TG)),
               rdf_assert(S,P,O,TG)).

%% subclass_of_svf(S,P,O,G)
%
% S SubClassOf P some O (asserted in G)
subclass_of_svf(S,P,O,G) :-
        rdf(S,rdfs:subClassOf,X,G),
        rdf(X,owl:onProperty,P,G),
        rdf(X,owl:someValuesFrom,O,G).

%% target_graph(+SG,?TG) is det
%
% sets the target graph - this will be the
% same as SG unless TG already ground
target_graph(_,TG) :-
        \+ var(TG).
target_graph(SG,TG) :-
        % put in the same graph
        var(TG),
        !,
        SG=TG.

%% materialize_entailments(+G, +Rule) is det
%
% apply entailment/4 iteratively for Rule until
% saturated
materialize_entailments(G,Rule) :-
        repeat,
          rdf_graph_property(G,triples(NumTriples)),
          debug(rdft,'SATURATING. num_triples(~w) = ~w',[G,NumTriples]),
          entailment(S,P,O,Rule),
          rdf_assert(S,P,O,G),
          rdf_graph_property(G,triples(NumTriples_2)),
          (   NumTriples_2 == NumTriples
          ->  !
          ;   fail
          ).

% S sub* X, X P O ==> S P O
entailment(S,P,O,propagate_over_subclass) :-
        rdfs_subclass_of(S,X),
        rdf(X,P,Y),
        objectProperty(P),
        rdfs_subclass_of(Y,O).



        
