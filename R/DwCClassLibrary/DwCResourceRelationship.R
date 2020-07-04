# A relationship of one rdfs:Resource (http://www.w3.org/2000/01/rdf-schema#Resource) to another (http://rs.tdwg.org/dwc/terms/ResourceRelationship)
# Resources can be thought of as identifiable records or instances of classes and may include, but need not be limited to dwc:Occurrence, dwc:Organism, dwc:MaterialSample, dwc:Event, dwc:Location, dwc:GeologicalContext, dwc:Identification, or dwc:Taxon
setRefClass(
  Class = "DwCResourceRelationship",
  fields = list(
    # An identifier for an instance of relationship between one resource (the subject) and another (relatedResource, the object) (http://rs.tdwg.org/dwc/terms/resourceRelationshipID)
    resourceRelationshipOD = "character",
    # An identifier for the resource that is the subject of the relationship (http://rs.tdwg.org/dwc/terms/resourceID)
    resourceID = "character",
    # An identifier for a related resource (the object, rather than the subject of the relationship) (http://rs.tdwg.org/dwc/terms/relatedResourceID)
    relatedResourceID = "character",
    # The relationship of the resource identified by relatedResourceID to the subject (optionally identified by the resourceID) (http://rs.tdwg.org/dwc/terms/relationshipOfResource)
    # Recommended best practice is to use a controlled vocabulary
    relationshipOfResource = "character",
    # The source (person, organization, publication, reference) establishing the relationship between the two resources (http://rs.tdwg.org/dwc/terms/relationshipAccordingTo)
    relationshipAccordingTo = "character",
    # The date-time on which the relationship between the two resources was established (http://rs.tdwg.org/dwc/terms/relationshipEstablishedDate)
    # Recommended best practice is to use a date that conforms to ISO 8601:2004(E)
    relationshipEstablishedDate = "character",
    # Comments or notes about the relationship between the two resources (http://rs.tdwg.org/dwc/terms/relationshipRemarks)
    relationshipRemarks = "character"
  ),
  contains = "DwCComponent"
)
