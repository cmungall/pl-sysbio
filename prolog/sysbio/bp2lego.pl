/* -*- Mode: Prolog -*- */

/**
  * Converts BioPax3 triples into a GO/LEGO instance graph
  *
  *
  * NOTE: This only works on BioPax3
  * To convert from l2 to l3:
  *
  * java -Xmx1g -jar paxtools.jar toLevel3 input_l2.owl output_l3.owl
  *
  * It works best on Reactome flavors (because Reactome uses GO xrefs heavily)
  *
  */

:- module(bp2lego,
          [
           convert_biopax_to_lego/1,
           convert_biopax_to_lego/2,
           extract_pathways/0,
           anonymize_non_processes/1
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(biopax_util).
:- use_module(lego_ns).
:- use_module(ro).
:- use_module(view_engine).
:- use_module(owl_util).

objectProperty(part_of).
objectProperty(has_part).
objectProperty(occurs_in).
objectProperty(has_input).
objectProperty(has_output).
objectProperty(directly_provides_input_for).
objectProperty(directly_inhibits).

:- op(1050,xfy,<==).
:- rdf_meta '<=='(t,t).

:- multifile view_engine:view/2.
view_engine:view( H,B ) :- (H <== B).
:- module_transparent('<=='/2).
:- module_transparent(view_engine:view/2).



% ========================================
% MODEL
% ========================================

% ontology shortcuts and core axioms
:- op(300, xfy, part_of).
:- op(300, xfy, occurs_in).
:- op(300, xfy, enabled_by).
:- op(300, xfy, regulated_by).
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
    <== W pathwayComponent P,
    (   conversion(P)
    ;   pathway(P)).


% alternate
/*
P part_of W,
  P type (owl:'NamedIndividual'),
  W type (owl:'NamedIndividual')
   <== W pathwayOrder Step, Step stepProcess P,
       \+ catalysis(P).
*/

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
      bp2lego:xref_to_go(Xref, Type).

Process type Type <==
      pathway(Process),
      rdf_current_prefix(biological_process,Type).

% continuants
X type Type <==
      physicalEntity(X),
      rdf_current_prefix(continuant,Type).


% ========================================
% activity type
% ========================================

Event type EventType <==
       conversion(Event),
       bp2lego:event_type_semidet(Event,EventType),
       sub_atom(EventType,_,_,_,'/GO_').


/*
Event type EventType <==
        CI controlled Event, % e.g. catalysis112 controlled reaction297
        CI xref Xref,
        bp2lego:xref_to_go(Xref, EventType). 

Event type EventType <==
        biochemicalReaction(Event),
        Event xref Xref,
        bp2lego:xref_to_go(Xref, EventType). 
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
Event occurs_in Loc,
   Loc type LocType
   <==
             CI controlled Event,
             CI controller C,
             catalysis(CI),
             C cellularLocation Loc,
             Loc xref Xref,
             bp2lego:xref_to_uri(Xref, LocType).

% ========================================
% enabled_by
% ========================================

Event enabled_by M,
    M type MType
     <==
             CI controlled Event,    % e.g. Catalysis1651 controlled Reaction3695
             CI controller M, % e.g. Catalysis1651 controller Protein5061
             catalysis(CI),
             bp2lego:molecule_type(M,MType).

% E.g cataylis37 / reaction48 in glycolysis
Event enabled_by M,
     M type MType,
     Event type EType
     <==
             CI controlled Event,      % e.g. reaction48
             CI controller M,          % e.g. Protein108
             catalysis(CI),
             bp2lego:ref_best_xref(CI,EType),  % e.g. GO:0004613 ! phosphoenolpyruvate carboxykinase (GTP) activity
             M entityReference MRef,
             bp2lego:ref_best_xref(MRef,MType).


Event regulated_by M,
     M type MType
     <==
             CI controlled Event,    % e.g. Catalysis1651 controlled Reaction3695
             CI controller M, % e.g. Catalysis1651 controller Protein5061
             \+ catalysis(CI),
             bp2lego:molecule_type(M,MType).

% E.g cataylis37 / reaction48 in glycolysis
Event regulated_by M,
     M type MType,
     Event type EType
     <==
             CI controlled Event,      % e.g. reaction48
             CI controller M,          % e.g. Protein108
             \+ catalysis(CI),
             bp2lego:ref_best_xref(CI,EType),  % e.g. GO:0004613 ! phosphoenolpyruvate carboxykinase (GTP) activity
             sub_atom(EType,_,_,_,'/GO_'),
             M entityReference MRef,
             bp2lego:ref_best_xref(MRef,MType).

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
             bp2lego:molecule_type(M,MType).

Event has_output M,
     M type MType <==
             Event right M,
             bp2lego:molecule_type(M,MType).

% TODO: cardinality

% ========================================
% metadata
% ========================================

rdf(S,rdfs:label,literal(O)) <==
   S displayName O.

% ========================================
% complexes and molecules
% ========================================

W has_part P <==
   W component P.
W has_part P <==
   W memberPhysicalEntity E,
   E component P.

:- rdf_meta molecule_type(r,r).
molecule_type(M,MType) :-
        M entityReference Ref,
        ref_best_xref(Ref,MType).
molecule_type(M,macromolecular_complex:'') :-
        complex(M).

M type MT <==
        complex(M),
        rdf_current_prefix(macromolecular_complex,MT).

M type MT <==
        smallMolecule(M),
        rdf_current_prefix(molecular_entity,MT).

M type MT <==
        protein(M),
        %rdf_current_prefix(protein,MT).
        M entityReference Ref,
        bp2lego:ref_best_xref(Ref,MT).


% ========================================
% generic
% ========================================

xref_to_go(Xref,IRI) :-
        xref_to_uri_inf(Xref, IRI,'GO'),
        !.

std_ns('GO').
std_ns('UniProt').
std_ns('UniProtIsoform').
std_ns('ChEBI').
std_ns('CHEBI').

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
% UTIL
% ========================================
% code below this may be moved to distinct module

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

assert_object_properties(G) :-
        forall(objectProperty(OPN),
               (   rdf_global_id(OPN:'',OP),
                   rdf_assert(OP,rdf:type,owl:'ObjectProperty',G))).

%% anonymize_non_processes(+G)
%
% anonymyze all non-process individuals in a graph
%
% first generate a mapping between triples and
% replacement expressions, then replace
%
anonymize_non_processes(G):-
        V = triple_expression(_S,_P,_O,G,_X),
        setof(V,V,Vs),
        forall(member(V,Vs),
               triple_replace(V)).


%% triple_expression(?S,?P,?O,?G,?Expr) is nondet
%
% if S-P-O is a triple where P is a relation to be anonymized,
% assuming S and O are individuals,
% generate Expr equivalent to or subsuming O,
%  i.e.some(P,X) -- where X is the most specific class expression subsuming O
%
triple_expression(S,P,O,G,some(P,X)) :-
        rdf(S,P,O,G),
        anrel(P),
        debug(bp2lego,' checking ~w --> ~w',[P,O]),
        node_expression(O,X,G).  % MSC


%% triple_replace(+Expr)
%
% where Expr combines an S-P-O triple together with a graph G
% and a class expression X, yielded from triple_expression/5
%
triple_replace(triple_expression(S,P,O,G,X)) :-
        rdf_retractall(S,P,O,G),
        debug(bp2lego,' Making complex: ~w',[X]),
        assert_complex_type(X,BN,G),
        % TODO - simplify expression
        debug(bp2lego,'   BN: ~w',[BN]),
        rdf_assert(S,rdf:type,BN,G),
        transfer_label(O,X,G).

% transfer the label from an individual to its MSC expression;
% note this may not be strictly valid if there are multiple
% individuals with the same expression
transfer_label(O,some(_,Y),G) :-
        atom(Y),
        \+ sub_atom(Y,_,_,_,'GO_'),
        forall(rdf(O,rdfs:label,Label,G),
               rdf_assert(Y,rdfs:label,Label,G)),
        !.
transfer_label(_,_,_).

%% assert_complex_type(+Expr,?Node,+Graph)
%
% maps an EL-type class expression to a set of
% triples which are asserted.
assert_complex_type(and(L),BN,G) :-
        !,
        rdf_bnode(BN),
        rdf_assert(BN,rdf:type,owl:'Class'),
        debug(bp2lego,'   AND: ~w',[L]),
        findall(EBN,
                (   member(E,L),
                    assert_complex_type(E,EBN,G)),
                LX),
        rdfs_assert_list(LX,LBN,G),
        rdf_assert(BN,owl:intersectionOf,LBN,G).

assert_complex_type(some(P,Y),BN,G) :-
        !,
        debug(bp2lego,'   SOME: ~w ~w',[P,Y]),
        rdf_bnode(BN),
        rdf_assert(BN,rdf:type,owl:'Restriction',G),
        rdf_assert(BN,owl:onProperty,P,G),
        assert_complex_type(Y,YBN,G),
        rdf_assert(BN,owl:someValuesFrom,YBN,G).
assert_complex_type(X,X,_).


/*
anonymize_non_processes(G):-
        forall(rdf(S,P,O,G),
               anonymize_non_process(S,P,O,G)).
*/

anonymize_non_process(S,P,O,G) :-
        %is_process(S),
        %\+ is_process(O),
        anrel(P),
        !,
        anonymize(S,P,O,G).
anonymize_non_process(_,_,_,_).

% TODO - complexes - translate to intersections
anonymize(S,P,O,G) :-
        rdf(O,rdf:type,OT,G),  
        debug(bp2lego,'Anonymizing: ~w ~w ~w, where O type ~w',[S,P,O,OT]),
        !,
        rdf_retractall(S,P,O,G),
        %rdf_retractall(O,rdf:type,OT,G), % TODO - defer this
        %rdf_retractall(O,_,_,G), % too extreme?
        rdf_bnode(X),
        rdf_assert(S,rdf:type,X,G),
        rdf_assert(X,rdf:type,owl:'Restriction',G),
        rdf_assert(X,owl:onProperty,P,G),
        rdf_assert(X,owl:someValuesFrom,OT,G),
        debug(bp2lego,'  DONE anon: ~w ~w ~w, where O type ~w',[S,P,O,OT]).
anonymize(_,_,_,_).

:- rdf_meta anrel(r).
anrel(occurs_in:'').
anrel(enabled_by:'').
anrel(regulated_by:'').
anrel(has_output:'').
anrel(has_input:'').

add_ontology_header(G) :-
        atom_concat('http://purl.obolibrary.org/obo/',G,Ont),
        rdf_assert(Ont,rdf:type,owl:'Ontology',G),
        rdf_assert(Ont,owl:imports,'http://purl.obolibrary.org/obo/go.owl',G),
        rdf_assert(Ont,owl:imports,'http://purl.obolibrary.org/obo/ro.owl',G).
        

% ========================================
% top-level
% ========================================

convert_biopax_to_lego(Graph) :-
        convert_biopax_to_lego(Graph,[]).
convert_biopax_to_lego(Graph,Opts) :-
        option(fresh(IsFresh),Opts,true),
        materialize_views(Graph,IsFresh),
        assert_object_properties(Graph),
        (   option(abox(true),Opts)
        ->  true
        ;   anonymize_non_processes(Graph)),
        add_ontology_header(Graph).


/*
is_process(X) :-
        pathway(X).
is_process(X) :-
        rdf_reachable(P, has_part:'', X),
        pathway(P).
*/
        
        
        
        
