/* -*- Mode: Prolog -*- */

/**
  * Reactome utilities
  *
  */

:- module(reactome_util,
          [
           remote_load_reactome/1,
           remote_load_reactome/2,
           remote_load_reactome/3,
           remote_reactome_main
           ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(lego_ns).
:- use_module(bp2lego).

%% remote_load_reactome(+ReactomeID,+IntoGraph,+Opts:list) is det
%
% loads a reactome pathway into an RDF graph
%
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
%  The ID provided typically corresponds to the DIAGRAM ID in reactome
%  
% Example:
%
%  * http://www.reactome.org/PathwayBrowser/#DIAGRAM=109607&PATH=109581 (Extrinsinc Pathway for Apoptosis)
%  * http://www.reactome.org/ReactomeRESTfulAPI/RESTfulWS/biopaxExporter/Level3/109607
%
%  * http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&ID=70171 Pathway: Glycolysis (Homo sapiens) redirects to:
%  * http://www.reactome.org/PathwayBrowser/#DB=gk_current&FOCUS_SPECIES_ID=48887&FOCUS_PATHWAY_ID=71387&ID=70171
%  * http://www.reactome.org/ReactomeRESTfulAPI/RESTfulWS/biopaxExporter/Level3/70171
%
%
%
%
remote_load_reactome(ReactomeID) :-
        remote_load_reactome(ReactomeID, []).
remote_load_reactome(ReactomeID,Opts) :-
        atom_concat('reactome_',ReactomeID,IntoGraph),
        remote_load_reactome(ReactomeID, IntoGraph,Opts).

% TODO: plant - http://plantreactome.gramene.org/ReactomeRESTfulAPI/RESTfulWS/biopaxExporter/Level3/2894885
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
        ->  convert_biopax_to_lego(LegoGraph,Opts)
        ;   true),
        (   option(cleanup(true),Opts)
        ->  rdf_retractall(_,_,_,IntoGraph)
        ;   true).


remote_reactome_main :-
        current_prolog_flag(argv, Argv),
        OptsSpec=
        [
         [opt(query),
          type(atom),
          shortflags([q]),
          help(['foo'])
          ],
         [opt(output),
          type(atom),
          shortflags([o]),
          help(['foo'])
          ]
        ],
        opt_arguments(OptsSpec, Opts, PositionalArgs),
        forall(member(Q,PositionalArgs),
               query(Q,Opts)),
        halt(0).


query(ID,Opts) :-
        remote_load_reactome(ID,[fresh(true),lego(lego),cleanup(true)]),
        (   member(output(Path),Opts)
        ->  rdf_save(Path,[graph(lego)])
        ;   tmp_file(lego,Path),
            rdf_save(Path,[graph(lego)]),
            read_file_to_string(Path,Str,[]),
            write(Str)),
        rdf_retractall(_,_,_,lego).
