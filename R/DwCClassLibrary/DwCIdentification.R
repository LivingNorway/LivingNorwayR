# A taxonomic determination (e.g., the assignment to a taxon) (http://rs.tdwg.org/dwc/terms/Identification)
setRefClass(
  Class = "DwCIdentification",
  fields = list(
    # An identifier for the Identification (the body of information associated with the assignment of a scientific name). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/identificationID)
    identification = "character",
    # A brief phrase or a standard term ("cf.", "aff.") to express the determiner's doubts about the Identification (http://rs.tdwg.org/dwc/terms/identificationQualifier)

  ),
  contains = "DwCComponent"
)
