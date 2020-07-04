setRefClass(
  Class = "DwCMeasurementOrFact",
  fields = list(
    # An identifier for the MeasurementOrFact (information pertaining to measurements, facts, characteristics, or assertions). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/measurementID)
    measurementID = "character",
    # The nature of the measurement, fact, characteristic, or assertion (http://rs.tdwg.org/dwc/terms/measurementType)
    # Recommended best practice is to use a controlled vocabulary
    measurementType = "character",
    # The value of the measurement, fact, characteristic, or assertion (http://rs.tdwg.org/dwc/terms/measurementValue)
    measurementValue = "character",
    # The description of the potential error associated with the measurementValue (http://rs.tdwg.org/dwc/terms/measurementAccuracy)
    measurementAccuracy = "character",
    # The units associated with the measurementValue (http://rs.tdwg.org/dwc/terms/measurementUnit)
    # Recommended best practice is to use the International System of Units (SI)
    measurementUnit = "character",
    # A list (concatenated and separated) of names of people, groups, or organizations who determined the value of the MeasurementOrFact
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    measurementDeterminedBy = "character",
    # The date on which the MeasurementOrFact was made (http://rs.tdwg.org/dwc/terms/measurementDeterminedDate)
    # Recommended best practice is to use a date that conforms to ISO 8601:2004(E)
    measurementDeterminedDate = "character",
    # A description of or reference to (publication, URI) the method or protocol used to determine the measurement, fact, characteristic, or assertion (http://rs.tdwg.org/dwc/terms/measurementMethod)
    measurementMethod = "character",
    # Comments or notes accompanying the MeasurementOrFact (http://rs.tdwg.org/dwc/terms/measurementRemarks)
    measurementRemarks = "character"
  ),
  contains = "DwCComponent"
)
