# An existence of an Organism (sensu http://rs.tdwg.org/dwc/terms/Organism) at a particular place at a particular time (http://rs.tdwg.org/dwc/terms/Occurrence)
setRefClass(
  Class = "DwCOccurrence",
  fields = list(
    # An identifier for the Occurrence (as opposed to a particular digital record of the occurrence). In the absence of a persistent global unique identifier, construct one from a combination of identifiers in the record that will most closely make the occurrenceID globally unique (http://rs.tdwg.org/dwc/terms/occurrenceID)
    # Recommended best practice is to use a persistent, globally unique identifier
    occurrenceID = "character",
    # An identifier (preferably unique) for the record within the data set or collection (http://rs.tdwg.org/dwc/terms/catalogNumber)
    catalogNumber = "character",
    # An identifier given to the Occurrence at the time it was recorded. Often serves as a link between field notes and an Occurrence record, such as a specimen collector's number (http://rs.tdwg.org/dwc/terms/recordNumber)
    recordNumber = "character",
    # A list (concatenated and separated) of names of people, groups, or organizations responsible for recording the original Occurrence. The primary collector or observer, especially one who applies a personal identifier (recordNumber), should be listed first (http://rs.tdwg.org/dwc/terms/recordedBy)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    recordedBy = "character",
    # The number of individuals represented present at the time of the Occurrence (http://rs.tdwg.org/dwc/terms/individualCount)
    individualCount = "character",
    # A number or enumeration value for the quantity of organisms (http://rs.tdwg.org/dwc/terms/organismQuantity)
    # A dwc:organismQuantity must have a corresponding dwc:organismQuantityType
    organismQuantity = "character",
    # The type of quantification system used for the quantity of organisms (http://rs.tdwg.org/dwc/terms/organismQuantityType)
    # A dwc:organismQuantityType must have a corresponding dwc:organismQuantity
    organismQuantityType = "character",
    # The sex of the biological individual(s) represented in the Occurrence (http://rs.tdwg.org/dwc/terms/sex)
    # Recommended best practice is to use a controlled vocabulary
    sex = "character",
    # The age class or life stage of the biological individual(s) at the time the Occurrence was recorded (http://rs.tdwg.org/dwc/terms/lifeStage)
    # Recommended best practice is to use a controlled vocabulary
    lifeStage = "character",
    # The reproductive condition of the biological individual(s) represented in the Occurrence (http://rs.tdwg.org/dwc/terms/reproductiveCondition)
    # Recommended best practice is to use a controlled vocabulary
    reproductiveCondition = "character",
    # The behavior shown by the subject at the time the Occurrence was recorded (http://rs.tdwg.org/dwc/terms/behavior)
    behavior = "character",
    # The process by which the biological individual(s) represented in the Occurrence became established at the location (http://rs.tdwg.org/dwc/terms/establishmentMeans)
    # Recommended best practice is to use a controlled vocabulary
    establishmentMeans = "character",
    # A statement about the presence or absence of a Taxon at a Location (http://rs.tdwg.org/dwc/terms/occurrenceStatus)
    # Recommended best practice is to use a controlled vocabulary
    occurrenceStatus = "character",
    # A list (concatenated and separated) of preparations and preservation methods for a specimen (http://rs.tdwg.org/dwc/terms/preparations)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    preparations = "character",
    # The current state of a specimen with respect to the collection identified in collectionCode or collectionID (http://rs.tdwg.org/dwc/terms/disposition)
    # Recommended best practice is to use a controlled vocabulary
    disposition = "character",
    # A list (concatenated and separated) of identifiers (publication, global unique identifier, URI) of media associated with the Occurrence (http://rs.tdwg.org/dwc/terms/associatedMedia)
    associatedMedia = "character",
    # A list (concatenated and separated) of identifiers (publication, bibliographic reference, global unique identifier, URI) of literature associated with the Occurrence (http://rs.tdwg.org/dwc/terms/associatedReferences)
    associatedReferences = "character",
    # A list (concatenated and separated) of identifiers (publication, global unique identifier, URI) of genetic sequence information associated with the Occurrence (http://rs.tdwg.org/dwc/terms/associatedSequences)
    associatedSequences = "character",
    # A list (concatenated and separated) of identifiers or names of taxa and their associations with the Occurrence (	http://rs.tdwg.org/dwc/terms/associatedTaxa)
    associatedTaxa = "character",
    # A list (concatenated and separated) of previous or alternate fully qualified catalog numbers or other human-used identifiers for the same Occurrence, whether in the current or any other data set or collection (http://rs.tdwg.org/dwc/terms/otherCatalogNumbers)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    otherCatalogNumbers = "character",
    # Comments or notes about the Occurrence (http://rs.tdwg.org/dwc/terms/occurrenceRemarks)
    occurrenceRemarks = "character"
  ),
  contains = "DwCComponent"
)