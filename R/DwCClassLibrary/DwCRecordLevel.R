setRefClass(
  Class = "DwCRecordLevel",
  fields = list(
    # The nature or genre of the resource (http://purl.org/dc/terms/type)
    # Must be populated with a value from the DCMI type vocabulary (http://dublincore.org/documents/2010/10/11/dcmi-type-vocabulary/)
    type = "character",
    # The most recent date-time on which the resource was changed (http://purl.org/dc/terms/modified)
    # Recommended best practice is to use a date that conforms to ISO 8601:2004(E)
    modified = "character",
    # A language of the resource (http://purl.org/dc/terms/language)
    # Recommended best practice is to use RFC 5646 as a controlled vocabulary
    language = "character",
    # A legal document giving official permission to do something with the resource (http://purl.org/dc/terms/license)
    license = "character",
    # A person or organization owning or managing rights over the resource (http://purl.org/dc/terms/rightsHolder)
    rightsHolder = "character",
    #	Information about who can access the resource or an indication of its security status. Access Rights may include information regarding access or restrictions based on privacy, security, or other policies (http://purl.org/dc/terms/accessRights)
    accessRights = "character",
    # A bibliographic reference for the resource as a statement indicating how this record should be cited (attributed) when used (http://purl.org/dc/terms/bibliographicCitation)
    # Recommended practice is to include sufficient bibliographic detail to identify the resource as unambiguously as possible
    bibliographicCitation = "character",
    # A related resource that is referenced, cited, or otherwise pointed to by the described resource (http://purl.org/dc/terms/references)
    references = "character",
    # An identifier for the institution having custody of the object(s) or information referred to in the record (http://rs.tdwg.org/dwc/terms/institutionID)
    # For physical specimens, the recommended best practice is to use an identifier from a collections registry such as the Global Registry of Biodiversity Repositories (http://grbio.org/).
    institutionID = "character",
    # An identifier for the collection or dataset from which the record was derived (http://rs.tdwg.org/dwc/terms/collectionID)
    # For physical specimens, the recommended best practice is to use an identifier from a collections registry such as the Global Registry of Biodiversity Repositories (http://grbio.org/)
    collectionID = "character",
    # An identifier for the set of data. May be a global unique identifier or an identifier specific to a collection or institution (http://rs.tdwg.org/dwc/terms/datasetID)
    datasetID = "character",
    # The name (or acronym) in use by the institution having custody of the object(s) or information referred to in the record (http://rs.tdwg.org/dwc/terms/institutionCode)
    institutionCode = "character",
    # The name, acronym, coden, or initialism identifying the collection or data set from which the record was derived (http://rs.tdwg.org/dwc/terms/collectionCode)
    collectionCode = "character",
    # The name identifying the data set from which the record was derived (http://rs.tdwg.org/dwc/terms/datasetName)
    datasetName = "character",
    # The name (or acronym) in use by the institution having ownership of the object(s) or information referred to in the record (http://rs.tdwg.org/dwc/terms/ownerInstitutionCode)
    ownerInstitutionCode = "character",
    # The specific nature of the data record (http://rs.tdwg.org/dwc/terms/basisOfRecord)
    # Recommended best practice is to use the standard label of one of the Darwin Core classes
    basisOfRecord = "character",
    # Additional information that exists, but that has not been shared in the given record (http://rs.tdwg.org/dwc/terms/informationWithheld)
    informationWithheld = "character",
    # Actions taken to make the shared data less specific or complete than in its original form. Suggests that alternative data of higher quality may be available on request (http://rs.tdwg.org/dwc/terms/dataGeneralizations)
    dataGeneralizations = "character",
    # A list of additional measurements, facts, characteristics, or assertions about the record. Meant to provide a mechanism for structured content (http://rs.tdwg.org/dwc/terms/dynamicProperties)
    # Recommended best practice is to use a key:value encoding schema for a data interchange format such as JSON
    dynamicProperties = "character"
  ),
  contains = "DwCComponent"
)