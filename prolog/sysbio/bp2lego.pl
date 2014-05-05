/* -*- Mode: Prolog -*- */

/**
  * Extracts from BioPax3 into a GO instance graph
  *
  * Note that most of this module is a generic RDF
  * view serialization engine; may be separated out into
  * a different project in the future.
  *
  * This only works on BioPax3
  * To convert from l2 to l3:
  *
  * java -Xmx1g -jar paxtools.jar toLevel3 input_l2.owl output_l3.owl
  *
  * It works best on Reactome flavors (because the use GO xrefs heavily)
  *
  */

:- module(bp2lego,
          [
           convert_biopax_to_lego/2,
           materialize_views/0,
           materialize_views/1,
           materialize_views/2,
           extract_pathways/0,
           vq/1,
           anonymize_non_processes/1
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(biopax_util).
:- use_module(lego_ns).

objectProperty(part_of).
objectProperty(has_part).
objectProperty(occurs_in).
objectProperty(has_input).
objectProperty(has_output).
objectProperty(directly_provides_input_for).
objectProperty(directly_inhibits).

% ========================================
% VIEW ENGINE
% ========================================
% this may move to its own codebase

:- op(1050,xfy,<==).

:- rdf_meta view(t,t).
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

assert_clist( rdf(Pred, Subj, Obj), G ) :-
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


% ========================================
% MODEL
% ========================================

% ontology shortcuts and core axioms
:- op(300, xfy, part_of).
:- op(300, xfy, occurs_in).
:- op(300, xfy, enabled_by).
:- op(300, xfy, has_input).
:- op(300, xfy, has_output).
:- op(300, xfy, directly_provides_input_for).
:- op(300, xfy, directly_inhibits).

:- op(300, xfy, type).  % rdf:type

%% default_triple(+S,+P,+O) is det
%
% triples to be asserted into every graph
% TODO - put this into a separate ttl file?
:- rdf_meta default_triple(r,r,r).
:- rdf_meta default_predicate_property(r,t).
default_triple(has_part:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(occurs_in:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(has_input:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(has_output:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(directly_activates:'', rdfs:subPropertyOf, mago:pathway_has).
default_triple(directly_inhibits:'', rdfs:subPropertyOf, mago:pathway_has).
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

P part_of W,
  P type (owl:'NamedIndividual'),
  W type (owl:'NamedIndividual')
    <== W pathwayComponent P.

% alternate
P part_of W,
  P type (owl:'NamedIndividual'),
  W type (owl:'NamedIndividual')
   <== W pathwayOrder Step, Step stepProcess P,
       \+ catalysis(P).

%P part_of W <== ppo(P,W).

%ppo(P,W) :- W pathwayComponent P.
%ppo(P,W) :- W pathwayOrder Step, Step stepProcess P.


%P type owl:'NamedIndividual' <== rdf(P, part_of, _) .
%W type owl:'NamedIndividual' <== rdf(_, part_of, W) .
%P type owl:'NamedIndividual' <== ppo(P,_).
%W type owl:'NamedIndividual' <== 

% ========================================
% process type
% ========================================

Process type Type <==
      pathway(Process),
      Process xref Xref,
      xref_to_go(Xref, Type).

% ========================================
% activity type
% ========================================

Event type EventType <==
       conversion(Event),
       event_type_semidet(Event,EventType).

/*
Event type EventType <==
        CI controlled Event, % e.g. catalysis112 controlled reaction297
        CI xref Xref,
        xref_to_go(Xref, EventType). 

Event type EventType <==
        biochemicalReaction(Event),
        Event xref Xref,
        xref_to_go(Xref, EventType). 
*/

event_type_semidet(Event,EventType) :-
        Event xref Xref,
        xref_to_go(Xref, EventType),
        !.
event_type_semidet(Event,EventType) :-
        Event xref Xref,
        ref_best_xref(Xref, EventType),
        !.
event_type_semidet(Event,EventType) :-
        CI controlled Event, % e.g. catalysis112 controlled reaction297
        CI xref Xref,
        xref_to_go(Xref, EventType),
        !.
event_type_semidet(Event,EventType) :-
        CI controlled Event, % e.g. catalysis112 controlled reaction297
        CI xref Xref,
        ref_best_xref(Xref, EventType),
        !.


        


        

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
     M type MType
     <==
             CI controlled Event,    % e.g. Catalysis1651 controlled Reaction3695
             CI controller M, % e.g. Catalysis1651 controller Protein5061
             M entityReference Ref,
             ref_best_xref(Ref,MType).

% E.g cataylis37 / reaction48 in glycolysis
Event enabled_by M,
     M type MType,
     Event type EType
     <==
             CI controlled Event,      % e.g. reaction48
             CI controller M,          % e.g. Protein108
             ref_best_xref(CI,EType),  % e.g. GO:0004613 ! phosphoenolpyruvate carboxykinase (GTP) activity
             M entityReference MRef,
             ref_best_xref(MRef,MType).

% ========================================
% Steps
% ========================================

Event directly_provides_input_for NextEvent
   <==
   Step stepProcess Event,
   Step nextStep NextStep,
   NextStep stepProcess NextEvent,
   biochemicalReaction(Event),
   biochemicalReaction(NextEvent).

Event directly_inhibits NextEvent
   <==
   control(Event),
   controlled(Event,NextEvent),
   controlType(Event,literal(type(_,'INHIBITION'))).

   

% ========================================
% I/O
% ========================================

Event has_input M,
     M type MType <==
             Event left M,
             M entityReference Ref,
             ref_best_xref(Ref,MType).

Event has_output M,
     M type MType <==
             Event right M,
             M entityReference Ref,
             ref_best_xref(Ref,MType).

% TODO: cardinality

% ========================================
% metadata
% ========================================

rdf(S,rdfs:comment,O) <==
   S displayName O.

             

% ========================================
% generic
% ========================================

xref_to_go(Xref,IRI) :-
        xref_to_uri_inf(Xref, IRI,'GO'),
        !.

std_ns('GO').
std_ns('UniProt').
std_ns('ChEBI').

nonstd_ns('PubMed').
nonstd_ns('Pubmed').

ref_best_xref(Ref,URI) :-
        Ref xref X,
        xref_to_uri(X,URI,NS),
        std_ns(NS),
        !.
ref_best_xref(Ref,URI) :-
        Ref xref X,
        xref_to_uri(X,URI,NS),
        \+ nonstd_ns(NS).


xref_to_uri(X,URI) :-
        xref_to_uri(X,URI,_).

% ignore db field if id contains sufficient info to construct URI
xref_to_uri(X,URI,NS) :-
        X id ID,                          % e.g. GO:1234567
        concat_atom([NS,Local],':',ID),   % e.g. GO, CHEBI
        !,
        rdf_global_id_wrap(NS:Local, URI).

% map (db,id) pair to URI
xref_to_uri(X,URI,NS) :-
        X db NS,
        X id Local,             % e.g. Q9GZV3
        concat_atom(NS_Toks,' ',NS),
        concat_atom(NS_Toks,'',NS_safe),
        !,
        rdf_global_id_wrap(NS_safe:Local, URI).

xref_to_uri_inf(X,URI,NS) :-
        xref_to_uri(X,URI_1),
        rdf(URI_1,owl:equivalentClass,URI),
        rdf_global_id_wrap(NS:_,URI).
xref_to_uri_inf(X,URI,NS) :-
        xref_to_uri(X,URI_1),
        rdf(URI,owl:equivalentClass,URI_1),
        rdf_global_id_wrap(NS:_,URI).
xref_to_uri_inf(X,URI,NS) :-
        xref_to_uri(X,URI,NS).

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
        debug(bp2lego,'Extracting: ~w from ~w ==> ~w',[SourceNode, SourceGraph, NumNodes]),
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
        forall(objectProperty(OPN),
               (   rdf_global_id(OPN:'',OP),
                   rdf_assert(OP,rdf:type,owl:'ObjectProperty',P))),
        extract_subgraph(P,lego,P).
        
% ========================================
% post-processing
% ========================================

anonymize_non_processes(G):-
        forall(rdf(S,P,O,G),
               anonymize_non_process(S,P,O,G)).

anonymize_non_process(S,P,O,G) :-
        %is_process(S),
        %\+ is_process(O),
        anrel(P),
        !,
        anonymize(S,P,O,G).
anonymize_non_process(_,_,_,_).

anonymize(S,P,O,G) :-
        rdf(O,rdf:type,OT,G),
        debug(bp2lego,'Anonymizing: ~w ~w ~w',[S,P,O]),
        !,
        rdf_retractall(S,P,O,G),
        rdf_bnode(X),
        rdf_assert(S,rdf:type,X,G),
        rdf_assert(X,rdf:type,owl:'Restriction',G),
        rdf_assert(X,owl:onProperty,P,G),
        rdf_assert(X,owl:someValuesFrom,OT,G).
anonymize(_,_,_,_).

:- rdf_meta anrel(r).
anrel(occurs_in:'').
anrel(enabled_by:'').

% ========================================
% top-level
% ========================================

convert_biopax_to_lego(Graph,IsFresh) :-
        materialize_views(Graph,IsFresh),
        anonymize_non_processes(Graph).


/*
is_process(X) :-
        pathway(X).
is_process(X) :-
        rdf_reachable(P, has_part:'', X),
        pathway(P).
*/
        
        
        
        