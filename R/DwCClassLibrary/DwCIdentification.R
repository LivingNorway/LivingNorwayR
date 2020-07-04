# A taxonomic determination (e.g., the assignment to a taxon) (http://rs.tdwg.org/dwc/terms/Identification)
setRefClass(
  Class = "DwCIdentification",
  fields = list(
    # An identifier for the Identification (the body of information associated with the assignment of a scientific name). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/identificationID)
    identificationID = "character",
    # A brief phrase or a standard term ("cf.", "aff.") to express the determiner's doubts about the Identification (http://rs.tdwg.org/dwc/terms/identificationQualifier)
    identificationQualifier = "character",
    # A list (concatenated and separated) of nomenclatural types (type status, typified scientific name, publication) applied to the subject (http://rs.tdwg.org/dwc/terms/typeStatus)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    typeStatus = "character",
    # A list (concatenated and separated) of names of people, groups, or organizations who assigned the Taxon to the subject (http://rs.tdwg.org/dwc/terms/identifiedBy)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    identifiedBy = "character",
    # The date on which the subject was identified as representing the Taxon (http://rs.tdwg.org/dwc/terms/dateIdentified)
    # Recommended best practice is to use a date that conforms to ISO 8601:2004(E)
    dateIdentified = "character",
    # A list (concatenated and separated) of references (publication, global unique identifier, URI) used in the Identification (http://rs.tdwg.org/dwc/terms/identificationReferences)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    identificationReferences = "character",
    # A categorical indicator of the extent to which the taxonomic identification has been verified to be correct (http://rs.tdwg.org/dwc/terms/identificationVerificationStatus)
    # Recommended best practice is to use a controlled vocabulary such as that used in HISPID and ABCD
    identificationVerificationStatus = "character",
    # Comments or notes about the Identification (http://rs.tdwg.org/dwc/terms/identificationRemarks)
    identificationRemarks = "character"
  ),
  contains = "DwCComponent"
)
