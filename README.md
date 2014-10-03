## Synopsis

systems biology logic programming library

## Details

Currently this is only good at one thing: taking Reactome-flavored
BioPAX3 and translating to the "LEGO/Noctua" OWL model, an event-based
model in which each event is an OWL individual instantiating a GO MF
or BF class, connected to other entities such as gene products and
cellular locations using relations from the Relations Ontology (RO).

this can be done on the command line or by running a server.

The procedure is essentially graph compaction. A reactome RDF graph
uses multiple nodes just to describe a single reaction/event. This can
be compacted to a single node if we use expressive relationships from
RO and GO classes that describe the semantics of the reaction.

## Running the server

From the root directory, type

    ./bin/sbserver

Then point your browser at URLs such as:

 * http://localhost:9001/reactome/lego/109581
  
Where 109581 is a reactome pathway ID

The Reactome API is quried

## Other databases

For non-reactome database it is necessary to manually download the
BioPAX RDF dump, and then convert this on the command line. See
examples for details.

BioPAX-3 must be used - use paxtools to migrate level 1 or level 2.

The extent to which the procedure works depends on how well described
the BioPAX is using standard ontologies like CHEBI and GO. One
challenge is that every DB handled Xrefs differently, so some
additional processing may be required. For now our focus is Reactome.
