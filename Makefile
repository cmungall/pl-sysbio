OBO=http://purl.obolibrary.org/obo/

all: tests

TESTS = apoptosis

tests: $(patsubst %,test-%,$(TESTS))

test-%:
	swipl -g "[t/$*],run_tests -> halt(0); halt(1)"

ontologies/go-orig.obo:
	wget $(OBO)/go.obo -O $@

ontologies/go.owl: ontologies/go-orig.obo
	owltools $< --extract-mingraph -o $@

ontologies/go.ttl: ontologies/go.owl
	owltools $< -o -f ttl $@
