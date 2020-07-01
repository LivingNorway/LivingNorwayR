# A particular organism or defined group of organisms considered to be taxonomically homogeneous (http://rs.tdwg.org/dwc/terms/Organism)
# Instances of the dwc:Organism class are intended to facilitate linking one or more dwc:Identification instances to one or more dwc:Occurrence instances. Therefore, things that are typically assigned scientific names (such as viruses, hybrids, and lichens) and aggregates whose occurrences are typically recorded (such as packs, clones, and colonies) are included in the scope of this class
setRefClass(
  Class = "DwCOrganism",
  fields = list(
    # An identifier for the Organism instance (as opposed to a particular digital record of the Organism). May be a globally unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/organismID)
    organismID = "character",
    # A textual name or label assigned to an Organism instance (http://rs.tdwg.org/dwc/terms/organismName)
    organismName = "character",
    # A description of the kind of Organism instance. Can be used to indicate whether the Organism instance represents a discrete organism or if it represents a particular type of aggregation (http://rs.tdwg.org/dwc/terms/organismScope)
    # Recommended best practice is to use a controlled vocabulary. This term is not intended to be used to specify a type of taxon. To describe the kind of dwc:Organism using a URI object in RDF, use rdf:type (http://www.w3.org/1999/02/22-rdf-syntax-ns#type) instead
    organismScope = "character",
    # A list (concatenated and separated) of identifiers of other Occurrence records and their associations to this Occurrence (http://rs.tdwg.org/dwc/terms/associatedOccurrences)
    associatedOcccurrences = "character",
    # A list (concatenated and separated) of identifiers of other Organisms and their associations to this Organism (http://rs.tdwg.org/dwc/terms/associatedOrganisms)
    associatedOrganisms = "character",
    # A list (concatenated and separated) of previous assignments of names to the Organism (http://rs.tdwg.org/dwc/terms/previousIdentifications)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    previousIdentifications = "character",
    # Comments or notes about the Organism instance (http://rs.tdwg.org/dwc/terms/organismRemarks)
    organismRemarks = "character"
  ),
  contains = "DwCComponent"
)