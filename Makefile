OBO=http://purl.obolibrary.org/obo

all: tests
test: test-apoptosis test-glycolysis

TESTS = apoptosis glycolysis reactome_remote

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
%-merged.owl: ontologies/import-module.owl %.owl 
	owltools $^ --merge-support-ontologies --set-ontology-id $(OBO)/go/$@ -o $@

all_tgts: target/Homo-sapiens-lego.owl

target/%-lego.owl: t/data/%.owl
	./bin/biopax2lego $< -o $@

target/%-importmod.owl: target/%.owl ontologies/extmerged.owl
	owltools --use-catalog $^ --add-imports-from-supports --extract-module -s $(OBO)/go.owl -c --remove-annotation-assertions -r -l --remove-axiom-annotations --set-ontology-id $(OBO)/lego/imports.owl -o $@

target/%-pack.owl: target/%.owl target/%-importmod.owl
	owltools --use-catalog $< --remove-imports-declarations target/$*-importmod.owl --merge-support-ontologies -o --prefix obo $(OBO)/ --prefix r http://www.reactome.org/biopax/ $@


##	owltools --use-catalog $^ --remove-imports-declarations --merge-support-ontologies -o -f ttl --prefix obo $(OBO) --prefix r http://www.reactome.org/biopax/ $@

prolog/sysbio/go.pl:
	../rdfs2pl/bin/rdfs2pl go $(HOME)/repos/go/ontology/subsets/goslim_generic.owl > $@
