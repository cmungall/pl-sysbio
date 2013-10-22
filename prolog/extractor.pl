/* -*- Mode: Prolog -*- */

/**
  * Extracts from BioPax3 into a GO instance graph
  *
  *
  */

:- module(mago,
          [
           vq/1,
           t/0,
           t2/0,
           load_glyc/0
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(biopax_util).

:- rdf_register_ns(obo, 'http://purl.obolibrary.org/obo/').
:- rdf_register_ns('GO', 'http://purl.obolibrary.org/obo/GO_').
:- rdf_register_ns('UniProt', 'http://purl.obolibrary.org/obo/UniProtKB_').  % change to PR?
:- rdf_register_ns(mago, 'http://purl.obolibrary.org/obo/GO/mago/').  % TODO
:- rdf_register_ns(activates, 'http://purl.obolibrary.org/obo/activates').
:- rdf_register_ns(directly_activates, 'http://purl.obolibrary.org/obo/directly_activates').
:- rdf_register_ns(inhibits, 'http://purl.obolibrary.org/obo/inhibits').
:- rdf_register_ns(enabled_by, 'http://purl.obolibrary.org/obo/enabled_by').
:- rdf_register_ns(type, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type').
:- rdf_register_ns(has_input, 'http://purl.obolibrary.org/obo/RO_0002233').
:- rdf_register_ns(has_output, 'http://purl.obolibrary.org/obo/RO_0002234').
:- rdf_register_ns(occurs_in, 'http://purl.obolibrary.org/obo/BFO_0000066').
:- rdf_register_ns(has_part, 'http://purl.obolibrary.org/obo/BFO_0000051').
:- rdf_register_ns(part_of, 'http://purl.obolibrary.org/obo/BFO_0000050').
foo('0').

% ========================================
% VIEW ENGINE
% ========================================
% this may move to its own codebase

:- op(1050,xfy,<==).

:- rdf_meta view(t).
:- rdf_meta '<=='(t,t).

view( H,B ) :- (H <== B).

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
        materialize_views(lego).
        
%% materialize_views(+Graph) is det
%
% asserts all views (which should have been declared with <==/2) into Graph
materialize_views(G) :-
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
        debug(mago, 'view ~w ==> ~w',[B,H]),
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
        %debug(mago,'ASSERT: ~w ~w ~w ~w',[Subj, Pred, Obj, G]),
        rdf_assert(Subj, Pred, Obj, G).

% slight overloading of namespace registration...
map_predicate(P,P2) :- rdf_current_prefix(P,P2).

% views are designed to be materialized, but
% we allow querying of unmaterialized views
vq(Head) :-
        view( Outer_Head :- Body),
        is_subterm_of(Head, Outer_Head),
        Body.

is_subterm_of(T, T) :- !.

is_subterm_of(Sub, (A,_)) :-
        is_subterm_of(Sub, A),
        !.
is_subterm_of(Sub, (_,B)) :-
        is_subterm_of(Sub, B),
        !.


% ========================================
% MODEL
% ========================================

% ontology shortcuts and core axioms
:- op(300, xfy, part_of).
:- op(300, xfy, occurs_in).
:- op(300, xfy, enabled_by).
:- op(300, xfy, has_input).
:- op(300, xfy, has_output).

:- op(300, xfy, type).  % rdf:type

:- rdf_meta default_triple(r,r,r).
:- rdf_meta default_predicate_property(r,t).
default_triple(has_part:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(occurs_in:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(has_input:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(has_output:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(activates:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(inhibits:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(rdf:type, rdfs:subPropertyOf, mago:pathway_has).
default_triple(mago:pathway_has, rdf:type, owl:'TransitiveProperty').
default_triple(mago:pathway_has, rdf:type, owl:'ReflexiveProperty').
default_triple(has_part:'',owl:inverseOf, part_of:'').

default_predicate_property(has_part:'',inverse_of(part_of:'')).
default_predicate_property(has_part:'',transitive(true)).
default_predicate_property(mago:pathway_has,transitive(true)).

% ----------------------------------------
% 
%  Transformation rules
%

% ========================================
% part_of
% ========================================

P part_of W <== W pathwayComponent P.

% ========================================
% activity type
% ========================================

Event type EventType <==
        CI controlled Event, % e.g. catalysis112 controlled reaction297
        CI xref Xref,
        xref_to_uri(Xref, EventType,'GO').

% todo - defaults to GO:0003674 or GO:0008150

% ========================================
% occurs_in
% ========================================

% in biopax, entities have locations, not events, so we use the location of the controller
(   Event occurs_in Loc,
     Loc type LocType) <==
             CI controlled Event,
             CI controller C, 
             C cellularLocation Loc,
             Loc xref Xref,
             xref_to_uri(Xref, LocType).

% ========================================
% enabled_by
% ========================================

Event enabled_by M,
     M type MType <==
             CI controlled Event,    % e.g. Catalysis1651 controlled Reaction3695
             CI controller M,        % e.g. Catalysis1651 controller Protein5061
             M entityReference Ref,
             Ref xref Xref,
             xref_to_uri(Xref, MType).


% ========================================
% I/O
% ========================================

Event has_input M,
     M type MType <==
             Event left M,
             M entityReference Ref,
             Ref xref Xref,
             xref_to_uri(Xref, MType).

Event has_output M,
     M type MType <==
             Event right M,
             M entityReference Ref,
             Ref xref Xref,
             xref_to_uri(Xref, MType).

% TODO: cardinality

             

% ========================================
% generic
% ========================================

xref_to_uri(X,URI) :-
        xref_to_uri(X,URI,_).

xref_to_uri(X,URI,NS) :-
        X id ID,                          % e.g. GO:1234567
        concat_atom([NS,Local],':',ID),   % e.g. GO, CHEBI
        !,
        rdf_global_id_wrap(NS:Local, URI).
xref_to_uri(X,URI,NS) :-
        X db NS,
        X id Local,             % e.g. Q9GZV3
        !,
        rdf_global_id_wrap(NS:Local, URI).

rdf_global_id_wrap(NS:Local, URI) :-
        rdf_current_prefix(NS,_),
        !,
        rdf_global_id(NS:Local, URI).
rdf_global_id_wrap(NS:Local, URI) :-
        concat_atom([NS,Local],'_',ID),
        rdf_global_id(obo:ID, URI).

% ========================================
% graph utils
% ========================================

%% pathway_node(+P, ?N, +G)
%
% true if N is reachable from P over has_part,
% or if P=N, or if 1 or 2 steps from such a node
pathway_node(P,N,G) :-
        event_node(P,N,G).

pathway_node(P,N,G) :-
        rdf_reachable(P, has_part:'', Part),
        event_node(Part,N,G).

event_node(N,N,_).
event_node(E,N,G) :-
        rdf(E,_,N,G).
event_node(E,N,G) :-
        rdf(E,_,X,G),
        rdf(X,_,N,G).


%% extract_subgraph(+SourceNode, +SourceGraph, +TargetGraph)
%
% extract closure of SourceNode from SourceGraph into TargetGraph
%
% here closure is defined by pathway_node/3
extract_subgraph(SourceNode,SourceGraph,TargetGraph) :-
        findall(TargetNode, pathway_node(SourceNode, TargetNode, lego), Nodes),
        length(Nodes,NumNodes),
        debug(mago,'Extracting: ~w from ~w ==> ~w',[SourceNode, SourceGraph, NumNodes]),
        forall( (member(S,Nodes),
                 rdf(S,P,O,SourceGraph)),
                rdf_assert(S,P,O,TargetGraph)),
        forall(rdf(S,rdf:type,O,TargetGraph),
               rdf_assert(O,rdf:type,owl:'Class',TargetGraph)).


%% extract_pathways
%
% For each pathway P that satisfies pathway/1, create
% a named graph (also called P) and extract all pathway
% members into this graph
extract_pathways :-
        forall(default_triple(S,R,T),
               rdf_assert(S,R,T,lego)),
        forall(default_predicate_property(Pred,Prop),
               rdf_set_predicate(Pred,Prop)),
        forall(pathway(P),
               extract_pathway(P)).

extract_pathway(P) :-
        rdf_create_graph(P),
        extract_subgraph(P,lego,P).
        

% ========================================
% test
% ========================================


pathway_graph_name(G,Name) :-
        rdf_graph(G),
        displayName(G,Name).


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
        Dir='t/out',
        (   exists_directory(Dir)
        ->  true
        ;   make_directory(Dir)),
        safe(N,N2),
        concat_atom([Dir,/,N2,'.owl'],Path),
        debug(mago,'saving to ~w',[Path]),
        rdf_save(Path,[graph(G)]).


load_glyc :-
        set_prolog_flag(verbose,normal),
        debug(mago),
        debug(rdft),
        debug(rdf(load)),
        ensure_loaded(library(semweb/rdf_cache)),
        rdf_set_cache_options(enabled(true)),
        rdf_set_cache_options(create_local_directory(true)),
        rdf_load('t/data/Homo-sapiens.owl').


t :-
        load_glyc,
        materialize_views,
        rdf_save('t/data/hs-mago.owl',[graph(lego)]),
        extract_pathways,
        %save_graphs,
        save_graph('Acetylcholine Neurotransmitter Release Cycle'),
        prolog.
t2 :-
        load_glyc,
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
