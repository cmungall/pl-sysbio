/* -*- Mode: Prolog -*- */

:- module(view_engine,
       [
        %(   <==)/2,
        %op(1050,xfy,<==),
        view/2,
        vq/1,
        materialize_views/0,
        materialize_views/1,
        materialize_views/2
        ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(biopax_util).
:- use_module(lego_ns).
:- use_module(ro).
:- use_module(owl_util).


% ========================================
% VIEW ENGINE
% ========================================
% this may move to its own codebase

%:- multifile((<==)/2).


:- rdf_meta view(t,t).
%:- rdf_meta '<=='(t,t).
:- module_transparent
        view/2,
        materialize_view/0,
        materialize_view/1,
        materialize_view/2.


:- multifile view/2.
%view( H,B ) :- (H <== B).

%% saturate_views(_Graph) is det
%
% materialize all views until materialize_views/1 generates no new triples.
% it is possible for this to never terminate - user should be careful not
% to introduce infinite models with <==/2.
saturate_views(G) :-
        repeat,
          rdf_graph_property(G,triples(NumTriples)),
          debug(rdft,'SATURATING. num_triples(~w) = ~w',[G,NumTriples]),
          materialize_views(G),
          rdf_graph_property(G,triples(NumTriples_2)),
          (   NumTriples_2 == NumTriples
          ->  !
          ;   fail
          ).

%% materialize_views is det
%
% As materialize_views/1, into default graph
materialize_views :-
        rdf_create_graph(lego),
        rdf_retractall(_,_,_,lego),
        materialize_views(lego).
        
%% materialize_views(+Graph,+IsFresh:boolean) is det
%% materialize_views(+Graph) is det
%
% asserts all views (which should have been declared with <==/2) into Graph
%
% if IsFresh is true then clear G prior to materialization
materialize_views(G) :-
        materialize_views(G,false).

materialize_views(G,true) :-
        rdf_create_graph(G),
        rdf_retractall(_,_,_,G),
        materialize_views(G,false).
materialize_views(G,false) :-
        rdf_graph_property(G,triples(NumTriples)),
        debug(rdft,'num_triples(~w) = ~w',[G,NumTriples]),
        forall( view(H,B),
               materialize_view(H,B,G)).

%% materialize_view(+Head:term ,+Body:term, +Graph) is det
%
% asserts Head terms for all Body
%
% Head and Body form a rule, i.e. Head :- Body
materialize_view(H,B,G) :-
        debug(bp2lego, 'view ~w ==> ~w',[B,H]),
        forall(B,
               assert_clist(H, G)).
%materialize_view(_,_,_). % always succeed


%% assert_clist( +TripleSetTerm, +Graph ) is det
%
% A TripleSetTerm is either
%  * a conjunction T1,T2 where both T1 and T2 are TripleSetTerms
%  * a term of the form P(S,O), which corresponds to a triple <S P O>
assert_clist( (A,B), G ) :-
        !,
        assert_clist(A, G),
        assert_clist(B, G).

assert_clist( rdf(Subj, Pred, Obj), G ) :-
        assert_triple(Subj, Pred, Obj, G).

assert_clist( Triple, G ) :-
        Triple =.. [Pred, Subj, Obj],
        assert_triple(Subj, Pred, Obj, G).


%% assert_triple(+Subj, +Pred, +Obj, +Graph) is det
% 
% asserts a triple into Graph
% 
% as rdf_assert/4, with mapping of predicate from short forms
assert_triple(Subj, Pred, Obj, G) :-
        map_predicate(Pred, Pred_2),
        !,
        assert_triple(Subj, Pred_2, Obj, G).
assert_triple(Subj, Pred, Obj, G) :-
        %debug(bp2lego,'ASSERT: ~w ~w ~w ~w',[Subj, Pred, Obj, G]),
        rdf_assert(Subj, Pred, Obj, G).

% slight overloading of namespace registration...
map_predicate(P,P2) :- rdf_current_prefix(P,P2).

% views are designed to be materialized, but
% we allow querying of unmaterialized views
vq(Head) :-
        view( Outer_Head , Body),
        is_subterm_of(Head, Outer_Head),
        Body.

is_subterm_of(T, T) :- !.

is_subterm_of(Sub, (A,_)) :-
        is_subterm_of(Sub, A),
        !.
is_subterm_of(Sub, (_,B)) :-
        is_subterm_of(Sub, B),
        !.
