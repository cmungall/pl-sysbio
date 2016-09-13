/* -*- Mode: Prolog -*- */

:- module(ro,
          [
           svf/3,
           
           op(400, xfy, (::)),
           (::)/2,
           op(300, xfy, part_of),
           part_of/2,
           op(300, xfy, has_part),
           has_part/2,
           op(300, xfy, occurs_in),
           occurs_in/2,
           op(300, xfy, enabled_by),
           enabled_by/2,
           op(300, xfy, regulated_by),
           regulated_by/2,
           op(300, xfy, has_input),
           has_input/2,
           op(300, xfy, has_output),
           has_output/2,
           op(300, xfy, directly_provides_input_for),
           directly_provides_input_for/2,
           op(300, xfy, causally_upstream_of),
           causally_upstream_of/2,
           
           op(300, xfy, type),
           type/2
           ]).
:- ensure_loaded(lego_ns).
:- use_module(library(semweb/rdf_db)).

:- op(400, xfy, (::)).
:- op(300, xfy, part_of).
:- op(300, xfy, has_part).
:- op(300, xfy, occurs_in).
:- op(300, xfy, enabled_by).
:- op(300, xfy, regulated_by).
:- op(300, xfy, has_input).
:- op(300, xfy, has_output).
:- op(300, xfy, directly_provides_input_for).
:- op(300, xfy, causally_upstream_of).

:- op(300, xfy, type).  % rdf:type

X has_part Y :- rdf(X,has_part:'',Y).
X part_of Y :- rdf(X,part_of:'',Y).
X occurs_in Y :- rdf(X,occurs_in:'',Y).
X enabled_by Y :- rdf(X,enabled_by:'',Y).
X regulated_by Y :- rdf(X,regulated_by:'',Y).
X has_input Y :- rdf(X,has_input:'',Y).
X has_output Y :- rdf(X,has_output:'',Y).
X directly_provides_input_for Y :- rdf(X,directly_provides_input_for:'',Y).
X causally_upstream_of Y :- rdf_has(X,causally_upstream_of:'',Y).

X part_of Y :: G :- rdf(X,part_of:'',Y,G).

X type Y :- rdf_has(X,rdf:type,Y).


:- rdf_meta svf(r,r,r).
svf(X,P,Y) :- rdf(X,rdf:type,N),rdf(N,owl:onProperty,P),rdf(N,owl:someValuesFrom,Y).
