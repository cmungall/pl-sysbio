/* -*- Mode: Prolog -*- */

/**
  * Server
  */

:- module(sbserver,
          [
           server/0
          ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).

:- ensure_loaded('prolog/sysbio/bp2lego').

server :-
        server(9001).

server(Port) :-
        debug(sbserver),
        debug(bp2lego),
        http_server(http_dispatch, [port(Port)]).

% Example reactome/lego/109607
:- http_handler(root(.), say_hi, []).
:- http_handler(root('reactome/lego/'), convert_reactome, [prefix]).


convert_reactome(Request) :-
        format('Content-type: application/rdf+xml~n~n'),
        debug(sbserver,'Checking ~w',[Request]),
        member(path_info(ID),Request),
        cvt(ID),
        concat_atom(['target/',ID,'.owl'],Path),
        rdf_save(Path,[graph(lego)]),
        rdf_retractall(_,_,_,lego),
        read_file_to_string(Path,Str,[]),
        write(Str).


cvt(ReactomeID) :-
        atom_concat(
                    'http://www.reactome.org/ReactomeRESTfulAPI/RESTfulWS/biopaxExporter/Level3/',
                    ReactomeID,
                    Path),
        atom_concat(r,ReactomeID,SourceGraph),
        debug(sbserver,'Loading from ~w',[Path]),
        ensure_loaded(library(semweb/rdf_http_plugin)),
        rdf_load(Path,[graph(SourceGraph), format(xml)]),
        debug(sbserver,'Loading into ~w',[ReactomeID]),
        %rdf_retractall(_,_,_,SourceGraph),
        materialize_views(lego,true),
        forall(rdf(S,P,O,G),
               debug(dbserver,'[~w ~w ~w IN ~w]',[S,P,O,G])).




        


        