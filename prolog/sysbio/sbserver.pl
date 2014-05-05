/* -*- Mode: Prolog -*- */

/**
  * Systems Biology Server
  *
  * Currently this has a single function - to provide a service
  * which given a reactome pathway ID will return a conversion of the
  * biopax for that pathway into Lego OWL
  *
  * To initiate the server:
  * ==
  * ./bin/sbserver
  * ==
  *
  * Then look on port 9001
  *
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
:- ensure_loaded('prolog/sysbio/reactome_util').

server :-
        server(9001).

server(Port) :-
        debug(sbserver),
        debug(bp2lego),
        debug(reactome),
        http_server(http_dispatch, [port(Port)]).

% Example reactome/lego/109607
:- http_handler(root(.), top_level, []).
:- http_handler(root('reactome/lego/'), convert_reactome, [prefix]).

convert_reactome(Request) :-
        format('Content-type: application/rdf+xml~n~n'),
        debug(sbserver,'Checking ~w',[Request]),
        member(path_info(ID),Request),
        debug(sbserver,'ID ~w',[ID]),
        remote_load_reactome(ID,[fresh(true),lego(lego),cleanup(true)]),
        concat_atom(['target/',ID,'.owl'],Path),
        rdf_save(Path,[graph(lego)]),
        rdf_retractall(_,_,_,lego),
        read_file_to_string(Path,Str,[]),
        write(Str).

top_level(_) :-
        format('Content-type: text/plain~n~n'),
        format('Nothing to see here, move along...~n').

        




        


        