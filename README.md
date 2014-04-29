# pl-sysbio

systems biology prolog library

currently this is only good at one thing: taking Reactome-flavored BioPAX3 and translating to the "LEGO" OWL model,
an event-based model in which each event is an OWL individual instantiating a GO MF or BF class, connected to other entities such
as gene products and cellular locations using relations from the Relations Ontology (RO).

this can be done on the command line or by running a server.

## sbserver

From the root directory, type

    ./bin/sbserver

Then point your browser at URLs such as:

 * http://localhost:9001/reactome/lego/109581
  
Where 109581 is a reactome pathway ID
