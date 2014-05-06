OBO=http://purl.obolibrary.org/obo

all: tests

TESTS = apoptosis

tests: $(patsubst %,test-%,$(TESTS))

test-%:
	swipl -g "[t/$*],run_tests -> halt(0); halt(1)"

ontologies/go-orig.obo:
	wget $(OBO)/go.obo -O $@

ontologies/go.owl: ontologies/go-orig.obo
	owltools $< --extract-mingraph --set-ontology-id $(OBO)/go.owl -o $@

ontologies/go.ttl: ontologies/go.owl
	owltools $< -o -f ttl $@

ontologies/ro.owl: 
	owltools $(OBO)/ro.owl --merge-imports-closure --extract-mingraph --set-ontology-id $(OBO)/ro.owl -o $@
