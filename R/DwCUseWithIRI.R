setRefClass(
  Class = "DwCUseWithIRI",
  fields = list(
    # Use to link a dcterms:Location instance subject to the lowest level standardized hierarchically-described resource (http://rs.tdwg.org/dwc/iri/inDescribedPlace)
    # Recommended best practice is to use an IRI from a controlled registry. A "convenience property" that replaces Darwin Core literal-value terms related to locations. See Section 2.7.5 of the Darwin Core RDF Guide for details
    inDescribedPlace = "character",
    # A person, group, or organization who assigned the Taxon to the subject (http://rs.tdwg.org/dwc/iri/identifiedBy)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    identifiedBy = "character",
    # A person, group, or organization responsible for recording the original Occurrence (http://rs.tdwg.org/dwc/iri/recordedBy)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    recordedBy = "character",
    # Use to link a dwc:Identification instance subject to a taxonomic entity such as a taxon, taxon concept, or taxon name use (http://rs.tdwg.org/dwc/iri/toTaxon)
    # A "convenience property" that replaces Darwin Core literal-value terms related to taxonomic entities. See Section 2.7.4 of the Darwin Core RDF Guide for details
    toTaxon = "character",
    # Use to link any subject resource that is part of a collection to the collection containing the resource (http://rs.tdwg.org/dwc/iri/inCollection)
    # Recommended best practice is to use an IRI from a controlled registry. A "convenience property" that replaces literal-value terms related to collections and institutions. See Section 2.7.3 of the Darwin Core RDF Guide for details
    inCollection = "character",
    # A person, group, or organization who determined the georeference (spatial representation) for the Location (http://rs.tdwg.org/dwc/iri/georeferencedBy)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    georeferencedBy = "character",
    # A description of the behavior shown by the subject at the time the Occurrence was recorded (http://rs.tdwg.org/dwc/iri/behavior)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    behavior = "character",
    # Actions taken to make the shared data less specific or complete than in its original form. Suggests that alternative data of higher quality may be available on request (http://rs.tdwg.org/dwc/iri/dataGeneralizations)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    dataGeneralizations = "character",
    # The current state of a specimen with respect to the collection identified in collectionCode or collectionID (http://rs.tdwg.org/dwc/iri/disposition)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    disposition = "character",
    # Use to link a dwc:GeologicalContext instance to chronostratigraphic time periods at the lowest possible level in a standardized hierarchy. Use this property to point to the earliest possible geological time period from which the cataloged item was collected (http://rs.tdwg.org/dwc/iri/earliestGeochronologicalEra)
    # Recommended best practice is to use an IRI from a controlled vocabulary. A "convenience property" that replaces Darwin Core literal-value terms related to geological context. See Section 2.7.6 of the Darwin Core RDF Guide for details
    earliestGeochronologicalEra = "character",
    # The process by which the biological individual(s) represented in the Occurrence became established at the location (http://rs.tdwg.org/dwc/iri/establishmentMeans)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    establishmentMeans = "character",
    # One of a) an indicator of the existence of, b) a reference to (publication, URI), or c) the text of notes taken in the field about the Event (http://rs.tdwg.org/dwc/iri/fieldNotes)
    # The subject is a dwc:Event instance and the object is a (possibly IRI-identified) resource that is the field notes
    fieldNotes = "character",
    # An identifier given to the event in the field. Often serves as a link between field notes and the Event (http://rs.tdwg.org/dwc/iri/fieldNumber)
    # The subject is a (possibly IRI-identified) resource that is the field notes and the object is a dwc:Event instance
    fieldNumber = "character",
    # A Well-Known Text (WKT) representation of the Spatial Reference System (SRS) for the footprintWKT of the Location. Do not use this term to describe the SRS of the decimalLatitude and decimalLongitude, even if it is the same as for the footprintWKT - use the geodeticDatum instead (http://rs.tdwg.org/dwc/iri/footprintSRS)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    footprintSRS = "character",
    # A Well-Known Text (WKT) representation of the shape (footprint, geometry) that defines the Location. A Location may have both a point-radius representation (see decimalLatitude) and a footprint representation, and they may differ from each other (http://rs.tdwg.org/dwc/iri/footprintWKT)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    footprintWKT = "character",
    # Use to link a dwc:GeologicalContext instance to an IRI-identified lithostratigraphic unit at the lowest possible level in a hierarchy (http://rs.tdwg.org/dwc/iri/fromLithostratigraphicUnit)
    # Recommended best practice is to use an IRI from a controlled vocabulary. A "convenience property" that replaces Darwin Core literal-value terms related to geological context. See Section 2.7.7 of the Darwin Core RDF Guide for details
    fromLithostratigraphicUnit = "character",
    # The ellipsoid, geodetic datum, or spatial reference system (SRS) upon which the geographic coordinates given in decimalLatitude and decimalLongitude as based (http://rs.tdwg.org/dwc/iri/geodeticDatum)'
    # Recommended best practice is to use an IRI for the EPSG code of the SRS, if known. Otherwise use an IRI or controlled vocabulary for the name or code of the geodetic datum, if known. Otherwise use an IRI or controlled vocabulary for the name or code of the ellipsoid, if known. If none of these is known, use the value unknown
    geodeticDatum = "character",
    # A description or reference to the methods used to determine the spatial footprint, coordinates, and uncertainties (http://rs.tdwg.org/dwc/iri/georeferenceProtocol)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    georeferenceProtocol = "character",
    # A map, gazetteer, or other resource used to georeference the Location (http://rs.tdwg.org/dwc/iri/georeferenceSources)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    georeferenceSources = "character",
    # A categorical description of the extent to which the georeference has been verified to represent the best possible spatial description (http://rs.tdwg.org/dwc/iri/georeferenceVerificationStatus)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    georeferenceVerificationStatus = "character",
    # A category or description of the habitat in which the Event occurred (http://rs.tdwg.org/dwc/iri/habitat)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    habitat = "character",
    # A controlled value to express the determiner's doubts about the Identification (http://rs.tdwg.org/dwc/iri/identificationQualifier)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    identificationQualifier = "character",
    # A categorical indicator of the extent to which the taxonomic identification has been verified to be correct (http://rs.tdwg.org/dwc/iri/identificationVerificationStatus)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects. Recommended best practice is to use a controlled vocabulary such as that used in HISPID and ABCD
    identificationVerificationStatus = "character",
    # Use to link a subject dataset record to the dataset which contains it (http://rs.tdwg.org/dwc/iri/inDataset)
    # A string literal name of the dataset can be provided using the term dwc:datasetName. See the Darwin Core RDF Guide for details
    inDataset = "character",
    # Additional information that exists, but that has not been shared in the given record (http://rs.tdwg.org/dwc/iri/informationWithheld)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    informationWithheld = "character",
    # Use to link a dwc:GeologicalContext instance to chronostratigraphic time periods at the lowest possible level in a standardized hierarchy. Use this property to point to the latest possible geological time period from which the cataloged item was collected (http://rs.tdwg.org/dwc/iri/latestGeochronologicalEra)
    # Recommended best practice is to use an IRI from a controlled vocabulary. A "convenience property" that replaces Darwin Core literal-value terms related to geological context. See Section 2.7.6 of the Darwin Core RDF Guide for details
    latestGeochronologicalEra = "character",
    # The age class or life stage of the biological individual(s) at the time the Occurrence was recorded (http://rs.tdwg.org/dwc/iri/lifeStage)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    lifeStage = "character",
    # Information about the source of this Location information. Could be a publication (gazetteer), institution, or team of individuals (http://rs.tdwg.org/dwc/iri/locationAccordingTo)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    locationAccordingTo = "character",
    # A person, group, or organization who determined the value of the MeasurementOrFact (http://rs.tdwg.org/dwc/iri/measurementDeterminedBy)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    measurementDeterminedBy = "character",
    # The method or protocol used to determine the measurement, fact, characteristic, or assertion (http://rs.tdwg.org/dwc/iri/measurementMethod)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    measurementMethod = "character",
    # The nature of the measurement, fact, characteristic, or assertion (http://rs.tdwg.org/dwc/iri/measurementType)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    measurementType = "character",
    # The units associated with the measurementValue (http://rs.tdwg.org/dwc/iri/measurementUnit)
    # Recommended best practice is to use the International System of Units (SI)
    measurementUnit = "character",
    # A statement about the presence or absence of a Taxon at a Location (http://rs.tdwg.org/dwc/iri/occurrenceStatus)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    occurrenceStatus = "character",
    # The type of quantification system used for the quantity of organisms (http://rs.tdwg.org/dwc/iri/organismQuantityType)
    # A dwc;organismQuantityType must have a corresponding dwc:organismQuantity
    organismQuantityType = "character",
    # A preparation or preservation method for a specimen (http://rs.tdwg.org/dwc/iri/preparations)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    preparations = "character",
    # An identifier given to the Occurrence at the time it was recorded. Often serves as a link between field notes and an Occurrence record, such as a specimen collector's number (http://rs.tdwg.org/dwc/iri/recordNumber)
    # The subject is a dwc:Occurrence and the object is a (possibly IRI-identified) resource that is the field notes
    recordNumber = "character",
    # The reproductive condition of the biological individual(s) represented in the Occurrence (http://rs.tdwg.org/dwc/iri/reproductiveCondition)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    reproductiveCondition = "character",
    # The unit of measurement of the size (time duration, length, area, or volume) of a sample in a sampling event (http://rs.tdwg.org/dwc/iri/sampleSizeUnit)
    # A sampleSizeUnit must have a corresponding sampleSizeValue. Recommended best practice is to use a controlled vocabulary such as the Ontology of Units of Measure http://www.wurvoc.org/vocabularies/om-1.8/ of SI units, derived units, or other non-SI units accepted for use within the SI
    sampleSizeUnit = "character",
    # The method or protocol used during an Event (http://rs.tdwg.org/dwc/iri/samplingProtocol)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    samplingProtocol = "character",
    # The sex of the biological individual(s) represented in the Occurrence (http://rs.tdwg.org/dwc/iri/sex)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    sex = "character",
    # A nomenclatural type (type status, typified scientific name, publication) applied to the subject (http://rs.tdwg.org/dwc/iri/typeStatus)
    # Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    typeStatus = "character",
    # The spatial coordinate system for the verbatimLatitude and verbatimLongitude or the verbatimCoordinates of the Location (http://rs.tdwg.org/dwc/iri/verbatimCoordinateSystem)
    # Recommended best practice is to use a controlled vocabulary. Terms in the dwciri namespace are intended to be used in RDF with non-literal objects
    verbatimCoordinateSystem = "character",
    # The ellipsoid, geodetic datum, or spatial reference system (SRS) upon which coordinates given in verbatimLatitude and verbatimLongitude, or verbatimCoordinates are based (http://rs.tdwg.org/dwc/iri/verbatimSRS)
    # Recommended best practice is to use an IRI for the EPSG code of the SRS, if known. Otherwise use an IRI or controlled vocabulary for the name or code of the geodetic datum, if known. Otherwise use an IRI or controlled vocabulary for the name or code of the ellipsoid, if known. If none of these is known, use the value unknown.
    verbatimSRS = "character"
  ),
  contains = "DwCComponent"
)
