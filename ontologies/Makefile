OBO = http://purl.obolibrary.org/obo

go.owl:
	owltools $(OBO)/go.owl -o $@

go-equivs.owl: go.owl
	owltools $< --translate-xrefs-to-equivs --filter-axioms -t EquivalentClasses -o $@


