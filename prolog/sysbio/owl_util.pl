/* -*- Mode: Prolog -*- */

:- module(owl_util,
          [node_expression/3]).

:- use_module(library(semweb/rdf_db)).

%% node_expression(+N,?X,+G)
%
% generate the Most Specific Class Expression that
% subsumes N
%
node_expression(N,X,G) :-
        setof(Arg,
              edge_expression(N,Arg,G),
              Args),
        args_expression(Args,X).

edge_expression(N,X,G) :-
        rdf(N,rdf:type,X,G).
edge_expression(N,some(P,Y),G) :-
        rdf(N,P,X,G),
        rdf(P,rdf:type,owl:'ObjectProperty',G),
        node_expression(X,Y,G).

args_expression([A],A) :- !.
args_expression(L,and(L)) :- !,
        L=[_|_].
