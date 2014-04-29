:-module(biopax_util, ['rna'/1, 'protein'/1, 'dna'/1, 'physicalEntity'/1, 'rnaReference'/1, 'publicationXref'/1, 'xref'/1, 'conversion'/1, 'interaction'/1, 'geneticInteraction'/1, 'catalysis'/1, 'control'/1, 'complex'/1, 'rnaRegionReference'/1, 'entityReference'/1, 'smallMoleculeReference'/1, 'cellularLocationVocabulary'/1, 'controlledVocabulary'/1, 'biochemicalPathwayStep'/1, 'pathwayStep'/1, 'molecularInteraction'/1, 'dnaRegionReference'/1, 'relationshipTypeVocabulary'/1, 'fragmentFeature'/1, 'entityFeature'/1, 'bioSource'/1, 'utilityClass'/1, 'complexAssembly'/1, 'gene'/1, 'templateReaction'/1, 'degradation'/1, 'sequenceModificationVocabulary'/1, 'dnaRegion'/1, 'provenance'/1, 'unificationXref'/1, 'experimentalFormVocabulary'/1, 'tissueVocabulary'/1, 'modulation'/1, 'evidence'/1, 'cellVocabulary'/1, 'bindingFeature'/1, 'deltaG'/1, 'sequenceLocation'/1, 'smallMolecule'/1, 'transport'/1, 'stoichiometry'/1, 'score'/1, 'sequenceSite'/1, 'evidenceCodeVocabulary'/1, 'pathway'/1, 'entity'/1, 'chemicalStructure'/1, 'proteinReference'/1, 'kPrime'/1, 'biochemicalReaction'/1, 'dnaReference'/1, 'covalentBindingFeature'/1, 'modificationFeature'/1, 'rnaRegion'/1, 'experimentalForm'/1, 'transportWithBiochemicalReaction'/1, 'entityReferenceTypeVocabulary'/1, 'phenotypeVocabulary'/1, 'relationshipXref'/1, 'sequenceRegionVocabulary'/1, 'templateReactionRegulation'/1, 'interactionVocabulary'/1, 'sequenceInterval'/1, participant/2, controller/2, stepProcess/2, interactionType/2, xref/2, confidence/2, evidenceCode/2, experimentalForm/2, notFeature/2, sequenceFeature/2, experimentalFormDescription/2, componentStoichiometry/2, featureLocation/2, dataSource/2, pathwayComponent/2, left/2, pathwayOrder/2, product/2, kEQ/2, relationshipType/2, right/2, deltaG/2, interactionScore/2, experimentalFormEntity/2, experimentalFeature/2, nextStep/2, featureLocationType/2, participantStoichiometry/2, evidence/2, cofactor/2, memberFeature/2, memberPhysicalEntity/2, subRegion/2, memberEntityReference/2, template/2, regionType/2, tissue/2, entityReferenceType/2, sequenceIntervalBegin/2, phenotype/2, cellularLocation/2, scoreSource/2, absoluteRegion/2, structure/2, physicalEntity/2, controlled/2, organism/2, cellType/2, taxonXref/2, modificationType/2, stepConversion/2, entityReference/2, sequenceIntervalEnd/2, bindsTo/2, entityFeature/2, component/2, url/2, deltaS/2, eCNumber/2, availability/2, term/2, biopax_comment/2, deltaH/2, author/2, biopax_name/2, source/2, conversionDirection/2, value/2, patoData/2, structureFormat/2, id/2, dbVersion/2, catalysisDirection/2, stoichiometricCoefficient/2, stepDirection/2, structureData/2, pMg/2, deltaGPrime0/2, templateDirection/2, standardName/2, temperature/2, controlType/2, intraMolecular/2, molecularWeight/2, displayName/2, ph/2, positionStatus/2, sequence/2, kPrime/2, chemicalFormula/2, year/2, idVersion/2, sequencePosition/2, spontaneous/2, title/2, db/2, ionicStrength/2, op(300, xfy, participant), op(300, xfy, controller), op(300, xfy, stepProcess), op(300, xfy, interactionType), op(300, xfy, xref), op(300, xfy, confidence), op(300, xfy, evidenceCode), op(300, xfy, experimentalForm), op(300, xfy, notFeature), op(300, xfy, sequenceFeature), op(300, xfy, experimentalFormDescription), op(300, xfy, componentStoichiometry), op(300, xfy, featureLocation), op(300, xfy, dataSource), op(300, xfy, pathwayComponent), op(300, xfy, left), op(300, xfy, pathwayOrder), op(300, xfy, product), op(300, xfy, kEQ), op(300, xfy, relationshipType), op(300, xfy, right), op(300, xfy, deltaG), op(300, xfy, interactionScore), op(300, xfy, experimentalFormEntity), op(300, xfy, experimentalFeature), op(300, xfy, nextStep), op(300, xfy, featureLocationType), op(300, xfy, participantStoichiometry), op(300, xfy, evidence), op(300, xfy, cofactor), op(300, xfy, memberFeature), op(300, xfy, memberPhysicalEntity), op(300, xfy, subRegion), op(300, xfy, memberEntityReference), op(300, xfy, template), op(300, xfy, regionType), op(300, xfy, tissue), op(300, xfy, entityReferenceType), op(300, xfy, sequenceIntervalBegin), op(300, xfy, phenotype), op(300, xfy, cellularLocation), op(300, xfy, scoreSource), op(300, xfy, absoluteRegion), op(300, xfy, structure), op(300, xfy, physicalEntity), op(300, xfy, controlled), op(300, xfy, organism), op(300, xfy, cellType), op(300, xfy, taxonXref), op(300, xfy, modificationType), op(300, xfy, stepConversion), op(300, xfy, entityReference), op(300, xfy, sequenceIntervalEnd), op(300, xfy, bindsTo), op(300, xfy, entityFeature), op(300, xfy, component), op(300, xfy, url), op(300, xfy, deltaS), op(300, xfy, eCNumber), op(300, xfy, availability), op(300, xfy, term), op(300, xfy, biopax_comment), op(300, xfy, deltaH), op(300, xfy, author), op(300, xfy, biopax_name), op(300, xfy, source), op(300, xfy, conversionDirection), op(300, xfy, value), op(300, xfy, patoData), op(300, xfy, structureFormat), op(300, xfy, id), op(300, xfy, dbVersion), op(300, xfy, catalysisDirection), op(300, xfy, stoichiometricCoefficient), op(300, xfy, stepDirection), op(300, xfy, structureData), op(300, xfy, pMg), op(300, xfy, deltaGPrime0), op(300, xfy, templateDirection), op(300, xfy, standardName), op(300, xfy, temperature), op(300, xfy, controlType), op(300, xfy, intraMolecular), op(300, xfy, molecularWeight), op(300, xfy, displayName), op(300, xfy, ph), op(300, xfy, positionStatus), op(300, xfy, sequence), op(300, xfy, kPrime), op(300, xfy, chemicalFormula), op(300, xfy, year), op(300, xfy, idVersion), op(300, xfy, sequencePosition), op(300, xfy, spontaneous), op(300, xfy, title), op(300, xfy, db), op(300, xfy, ionicStrength)]).

:-use_module(library(semweb/rdf_db)).
:-use_module(library(semweb/rdfs)).

:- initialization(rdf_load('ontologies/biopax-level3.owl'),
		  after_load).

biopax_util:rna(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Rna').
biopax_util:protein(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Protein').
biopax_util:dna(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Dna').
biopax_util:physicalEntity(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PhysicalEntity').
biopax_util:rnaReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RnaReference').
biopax_util:publicationXref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PublicationXref').
biopax_util:xref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Xref').
biopax_util:conversion(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Conversion').
biopax_util:interaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Interaction').
biopax_util:geneticInteraction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#GeneticInteraction').
biopax_util:catalysis(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Catalysis').
biopax_util:control(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Control').
biopax_util:complex(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Complex').
biopax_util:rnaRegionReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RnaRegionReference').
biopax_util:entityReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EntityReference').
biopax_util:smallMoleculeReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SmallMoleculeReference').
biopax_util:cellularLocationVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#CellularLocationVocabulary').
biopax_util:controlledVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ControlledVocabulary').
biopax_util:biochemicalPathwayStep(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalPathwayStep').
biopax_util:pathwayStep(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PathwayStep').
biopax_util:molecularInteraction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#MolecularInteraction').
biopax_util:dnaRegionReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DnaRegionReference').
biopax_util:relationshipTypeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RelationshipTypeVocabulary').
biopax_util:fragmentFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#FragmentFeature').
biopax_util:entityFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EntityFeature').
biopax_util:bioSource(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BioSource').
biopax_util:utilityClass(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#UtilityClass').
biopax_util:complexAssembly(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ComplexAssembly').
biopax_util:gene(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Gene').
biopax_util:templateReaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TemplateReaction').
biopax_util:degradation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Degradation').
biopax_util:sequenceModificationVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceModificationVocabulary').
biopax_util:dnaRegion(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DnaRegion').
biopax_util:provenance(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Provenance').
biopax_util:unificationXref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#UnificationXref').
biopax_util:experimentalFormVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ExperimentalFormVocabulary').
biopax_util:tissueVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TissueVocabulary').
biopax_util:modulation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Modulation').
biopax_util:evidence(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Evidence').
biopax_util:cellVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#CellVocabulary').
biopax_util:bindingFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BindingFeature').
biopax_util:deltaG(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DeltaG').
biopax_util:sequenceLocation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceLocation').
biopax_util:smallMolecule(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SmallMolecule').
biopax_util:transport(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Transport').
biopax_util:stoichiometry(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Stoichiometry').
biopax_util:score(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Score').
biopax_util:sequenceSite(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceSite').
biopax_util:evidenceCodeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EvidenceCodeVocabulary').
biopax_util:pathway(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Pathway').
biopax_util:entity(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#Entity').
biopax_util:chemicalStructure(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ChemicalStructure').
biopax_util:proteinReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ProteinReference').
biopax_util:kPrime(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#KPrime').
biopax_util:biochemicalReaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction').
biopax_util:dnaReference(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#DnaReference').
biopax_util:covalentBindingFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#CovalentBindingFeature').
biopax_util:modificationFeature(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ModificationFeature').
biopax_util:rnaRegion(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RnaRegion').
biopax_util:experimentalForm(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#ExperimentalForm').
biopax_util:transportWithBiochemicalReaction(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TransportWithBiochemicalReaction').
biopax_util:entityReferenceTypeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#EntityReferenceTypeVocabulary').
biopax_util:phenotypeVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#PhenotypeVocabulary').
biopax_util:relationshipXref(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#RelationshipXref').
biopax_util:sequenceRegionVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceRegionVocabulary').
biopax_util:templateReactionRegulation(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#TemplateReactionRegulation').
biopax_util:interactionVocabulary(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#InteractionVocabulary').
biopax_util:sequenceInterval(A):-rdfs_individual_of(A, 'http://www.biopax.org/release/biopax-level3.owl#SequenceInterval').
biopax_util:participant(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#participant', B).
biopax_util:controller(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#controller', B).
biopax_util:stepProcess(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stepProcess', B).
biopax_util:interactionType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#interactionType', B).
biopax_util:xref(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#xref', B).
biopax_util:confidence(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#confidence', B).
biopax_util:evidenceCode(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#evidenceCode', B).
biopax_util:experimentalForm(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalForm', B).
biopax_util:notFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#notFeature', B).
biopax_util:sequenceFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#feature', B). % renamed
biopax_util:experimentalFormDescription(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalFormDescription', B).
biopax_util:componentStoichiometry(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#componentStoichiometry', B).
biopax_util:featureLocation(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#featureLocation', B).
biopax_util:dataSource(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#dataSource', B).
biopax_util:pathwayComponent(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#pathwayComponent', B).
biopax_util:left(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#left', B).
biopax_util:pathwayOrder(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#pathwayOrder', B).
biopax_util:product(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#product', B).
biopax_util:kEQ(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#kEQ', B).
biopax_util:relationshipType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#relationshipType', B).
biopax_util:right(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#right', B).
biopax_util:deltaG(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaG', B).
biopax_util:interactionScore(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#interactionScore', B).
biopax_util:experimentalFormEntity(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalFormEntity', B).
biopax_util:experimentalFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#experimentalFeature', B).
biopax_util:nextStep(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#nextStep', B).
biopax_util:featureLocationType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#featureLocationType', B).
biopax_util:participantStoichiometry(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#participantStoichiometry', B).
biopax_util:evidence(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#evidence', B).
biopax_util:cofactor(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#cofactor', B).
biopax_util:memberFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#memberFeature', B).
biopax_util:memberPhysicalEntity(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#memberPhysicalEntity', B).
biopax_util:subRegion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#subRegion', B).
biopax_util:memberEntityReference(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#memberEntityReference', B).
biopax_util:template(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#template', B).
biopax_util:regionType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#regionType', B).
biopax_util:tissue(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#tissue', B).
biopax_util:entityReferenceType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#entityReferenceType', B).
biopax_util:sequenceIntervalBegin(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequenceIntervalBegin', B).
biopax_util:phenotype(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#phenotype', B).
biopax_util:cellularLocation(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#cellularLocation', B).
biopax_util:scoreSource(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#scoreSource', B).
biopax_util:absoluteRegion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#absoluteRegion', B).
biopax_util:structure(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#structure', B).
biopax_util:physicalEntity(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#physicalEntity', B).
biopax_util:controlled(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#controlled', B).
biopax_util:organism(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#organism', B).
biopax_util:cellType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#cellType', B).
biopax_util:taxonXref(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#taxonXref', B).
biopax_util:modificationType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#modificationType', B).
biopax_util:stepConversion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stepConversion', B).
biopax_util:entityReference(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#entityReference', B).
biopax_util:sequenceIntervalEnd(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequenceIntervalEnd', B).
biopax_util:bindsTo(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#bindsTo', B).
biopax_util:entityFeature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#entityFeature', B).
biopax_util:component(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#component', B).
biopax_util:url(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#url', X), rdf_literal_value(X, B).
biopax_util:deltaS(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaS', X), rdf_literal_value(X, B).
biopax_util:eCNumber(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#eCNumber', X), rdf_literal_value(X, B).
biopax_util:availability(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#availability', X), rdf_literal_value(X, B).
biopax_util:term(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#term', X), rdf_literal_value(X, B).
biopax_util:biopax_comment(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#comment', Y), rdf_literal_value(Y, B).
biopax_util:deltaH(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaH', X), rdf_literal_value(X, B).
biopax_util:author(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#author', X), rdf_literal_value(X, B).
biopax_util:biopax_name(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#name', Y), rdf_literal_value(Y, B).
biopax_util:source(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#source', X), rdf_literal_value(X, B).
biopax_util:conversionDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#conversionDirection', X), rdf_literal_value(X, B).
biopax_util:value(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#value', X), rdf_literal_value(X, B).
biopax_util:patoData(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#patoData', X), rdf_literal_value(X, B).
biopax_util:structureFormat(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#structureFormat', X), rdf_literal_value(X, B).
biopax_util:id(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#id', X), rdf_literal_value(X, B).
biopax_util:dbVersion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#dbVersion', X), rdf_literal_value(X, B).
biopax_util:catalysisDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#catalysisDirection', X), rdf_literal_value(X, B).
biopax_util:stoichiometricCoefficient(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stoichiometricCoefficient', X), rdf_literal_value(X, B).
biopax_util:stepDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#stepDirection', X), rdf_literal_value(X, B).
biopax_util:structureData(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#structureData', X), rdf_literal_value(X, B).
biopax_util:pMg(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#pMg', X), rdf_literal_value(X, B).
biopax_util:deltaGPrime0(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#deltaGPrime0', X), rdf_literal_value(X, B).
biopax_util:templateDirection(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#templateDirection', X), rdf_literal_value(X, B).
biopax_util:standardName(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#standardName', X), rdf_literal_value(X, B).
biopax_util:temperature(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#temperature', X), rdf_literal_value(X, B).
biopax_util:controlType(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#controlType', X), rdf_literal_value(X, B).
biopax_util:intraMolecular(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#intraMolecular', X), rdf_literal_value(X, B).
biopax_util:molecularWeight(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#molecularWeight', X), rdf_literal_value(X, B).
biopax_util:displayName(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#displayName', X), rdf_literal_value(X, B).
biopax_util:ph(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#ph', X), rdf_literal_value(X, B).
biopax_util:positionStatus(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#positionStatus', X), rdf_literal_value(X, B).
biopax_util:sequence(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequence', X), rdf_literal_value(X, B).
biopax_util:kPrime(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#kPrime', X), rdf_literal_value(X, B).
biopax_util:chemicalFormula(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#chemicalFormula', X), rdf_literal_value(X, B).
biopax_util:year(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#year', X), rdf_literal_value(X, B).
biopax_util:idVersion(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#idVersion', X), rdf_literal_value(X, B).
biopax_util:sequencePosition(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#sequencePosition', X), rdf_literal_value(X, B).
biopax_util:spontaneous(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#spontaneous', X), rdf_literal_value(X, B).
biopax_util:title(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#title', X), rdf_literal_value(X, B).
biopax_util:db(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#db', X), rdf_literal_value(X, B).
biopax_util:ionicStrength(A, B):-rdf_has(A, 'http://www.biopax.org/release/biopax-level3.owl#ionicStrength', X), rdf_literal_value(X, B).
