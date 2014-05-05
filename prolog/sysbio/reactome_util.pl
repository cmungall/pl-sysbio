/* -*- Mode: Prolog -*- */

/**
  * Reactome utilities
  *
  */

:- module(reactome_util,
          [
           remote_load_reactome/1,
           remote_load_reactome/2,
           remote_load_reactome/3
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(lego_ns).
:- use_module(bp2lego).

%% remote_load_reactome(+ReactomeID,+IntoGraph,+Opts:list) is det
%
% loads a reactome pathway into an RDF graph
%
% Options:
%  
%   * fresh(+Bool)
%     if true, clears all RDF graphs prior to loading
%  
%   * lego(+Graph)
%     if true, converts BioPax to LEGO model, putting derived triples into Graph
%  
%   * cleanup(+Bool)
%     if true, clears IntoGraph after loading
%  
remote_load_reactome(ReactomeID) :-
        remote_load_reactome(ReactomeID, []).
remote_load_reactome(ReactomeID,Opts) :-
        atom_concat('reactome_',ReactomeID,IntoGraph),
        remote_load_reactome(ReactomeID, IntoGraph,Opts).

remote_load_reactome(ReactomeID,IntoGraph,Opts) :-
        %(   option(fresh(true),Opts)
        %->  rdf_retractall(_,_,_)
        %;   true),
        atom_concat(
                    'http://www.reactome.org/ReactomeRESTfulAPI/RESTfulWS/biopaxExporter/Level3/',
                    ReactomeID,
                    URL),
        debug(reactome,'Loading from ~w',[URL]),
        ensure_loaded(library(semweb/rdf_http_plugin)),
        rdf_load(URL,[graph(IntoGraph), format(xml)]),
        debug(reactome,'Loading into ~w',[IntoGraph]),
        (   option(lego(LegoGraph),Opts)
        ->  convert_biopax_to_lego(LegoGraph,true),
            forall(rdf(S,P,O,LegoGraph),
                   debug(reactome,'[~w ~w ~w IN ~w]',[S,P,O,LegoGraph]))
        ;   true),
        (   option(cleanup(true),Opts)
        ->  rdf_retractall(_,_,_,IntoGraph)
        ;   true).


