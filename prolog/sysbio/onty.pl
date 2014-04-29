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
           node_prop/4
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
        rdf(S,P,literal(Value),G),
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

subclass_of_svf(S,P,O,G) :-
        rdf(S,rdfs:subClassOf,X,G),
        rdf(X,owl:onProperty,P,G),
        rdf(X,owl:someValuesFrom,O,G).

target_graph(_,TG) :-
        \+ var(TG).
target_graph(SG,TG) :-
        % put in the same graph
        var(TG),
        !,
        SG=TG.

