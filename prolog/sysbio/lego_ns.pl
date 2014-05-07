/* -*- Mode: Prolog -*- */

:- module(lego_ns,[]).
:- use_module(library(semweb/rdf_db)).

% Any unificationXref with a db field that maps
:- rdf_register_ns(obo, 'http://purl.obolibrary.org/obo/').
:- rdf_register_ns('GO', 'http://purl.obolibrary.org/obo/GO_').
:- rdf_register_ns('Rhea', 'http://purl.obolibrary.org/obo/RHEA_').
:- rdf_register_ns('MetaCyc', 'http://purl.obolibrary.org/obo/MetaCyc_').
%:- rdf_register_ns('UniProt', 'http://purl.obolibrary.org/obo/UniProtKB_').  % change to PR?
:- rdf_register_ns('UniProt', 'http://purl.obolibrary.org/obo/PR_').  % change to PR?
:- rdf_register_ns('ChEBI', 'http://purl.obolibrary.org/obo/CHEBI_').  % 
:- rdf_register_ns('PubMed', 'http://www.ncbi.nlm.nih.gov/pubmed/').  % 
:- rdf_register_ns('PubChemCompound', 'http://purl.obolibrary.org/obo/pubchem_').  % 
:- rdf_register_ns('KeggGlycan', 'http://purl.obolibrary.org/obo/kegg/glycan_').  % 
:- rdf_register_ns(mago, 'http://purl.obolibrary.org/obo/GO/mago/').  % TODO
:- rdf_register_ns(type, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type').
:- rdf_register_ns(namedIndividual, 'http://www.w3.org/2002/07/owl#NamedIndividual').
:- rdf_register_ns(directly_provides_input_for, 'http://purl.obolibrary.org/obo/RO_0002413').
:- rdf_register_ns(directly_activates, 'http://purl.obolibrary.org/obo/RO_0002406').
:- rdf_register_ns(directly_inhibits, 'http://purl.obolibrary.org/obo/RO_0002408').
:- rdf_register_ns(enabled_by, 'http://purl.obolibrary.org/obo/RO_0002333').
:- rdf_register_ns(regulated_by, 'http://purl.obolibrary.org/obo/RO_0002334').
:- rdf_register_ns(has_input, 'http://purl.obolibrary.org/obo/RO_0002233').
:- rdf_register_ns(has_output, 'http://purl.obolibrary.org/obo/RO_0002234').
:- rdf_register_ns(occurs_in, 'http://purl.obolibrary.org/obo/BFO_0000066').
:- rdf_register_ns(has_part, 'http://purl.obolibrary.org/obo/BFO_0000051').
:- rdf_register_ns(part_of, 'http://purl.obolibrary.org/obo/BFO_0000050').
foo('0').
:- rdf_register_ns(macromolecular_complex, 'http://purl.obolibrary.org/obo/GO_0032991').
:- rdf_register_ns(biological_process, 'http://purl.obolibrary.org/obo/GO_0008150').
