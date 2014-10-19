OBO=http://purl.obolibrary.org/obo

all: tests

TESTS = apoptosis glycolysis

tests: $(patsubst %,test-%,$(TESTS))

test-%:
	swipl -g "[t/$*],run_tests -> halt(0); halt(1)"

ontologies/go-orig.obo:
	wget $(OBO)/go.obo -O $@

ontologies/go.owl: ontologies/go-orig.obo
	owltools $< --extract-mingraph --set-ontology-id $(OBO)/go.owl -o $@

ontologies/go.ttl: ontologies/go.owl
	owltools $< -o -f ttl $@

ontologies/chebi-orig.obo:
	wget $(OBO)/chebi.obo -O $@

ontologies/chebi.owl: ontologies/chebi-orig.obo
	owltools $< --extract-mingraph --set-ontology-id $(OBO)/chebi.owl -o $@

ontologies/ro.owl: 
	owltools $(OBO)/ro.owl --merge-imports-closure --remove-annotation-assertions -r -l --remove-axiom-annotations --set-ontology-id $(OBO)/ro.owl -o $@

ontologies/extmerged.owl:
	owltools --use-catalog $(OBO)/go.owl $(OBO)/ro.owl $(OBO)/chebi.owl --merge-support-ontologies --set-ontology-id $(OBO)/go.owl -o $@

ontologies/import-module.owl: ontologies/extmerged.owl
	owltools --use-catalog target/hs-lego-hsap.owl $< --extract-module -s $(OBO)/go.owl -c --remove-annotation-assertions -r -l --remove-axiom-annotations --set-ontology-id $(OBO)/go2.owl -o $@

# Add imports directives
%-ig.owl: %.owl
	owltools $< --add-imports-declarations $(OBO)/go.owl $(OBO)/ro.owl $(OBO)/chebi.owl --set-ontology-id http://geneontology.org/models/hsap -o $@

# Alternate strategy: merge in minimal subset of external ontologies
%-merged.owl: %.owl ontologies/import-module.owl
	owltools $^ --merge-support-ontologies -o $@

