# An action that occurs at some location during some time (http://rs.tdwg.org/dwc/terms/Event)
setRefClass(
  Class = "DwCEvent",
  fields = list(
    # An identifier for the set of information associated with an Event (something that occurs at a place and time). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/eventID)
    eventID = "character",
    # An identifier for the broader Event that groups this and potentially other Events (http://rs.tdwg.org/dwc/terms/parentEventID)
    # Use a globally unique identifier for a dwc:Event or an identifier for a dwc:Event that is specific to the data set
    parentEventID = "character",
    # An identifier given to the event in the field. Often serves as a link between field notes and the Event (http://rs.tdwg.org/dwc/terms/fieldNumber)
    fieldNumber = "character",
    # The date-time or interval during which an Event occurred. For occurrences, this is the date-time when the event was recorded. Not suitable for a time in a geological context (http://rs.tdwg.org/dwc/terms/eventDate)
    # Recommended best practice is to use a date that conforms to ISO 8601:2004(E)
    eventDate = "character",
    # The time or interval during which an Event occurred (http://rs.tdwg.org/dwc/terms/eventTime)
    # Recommended best practice is to use a time that conforms to ISO 8601:2004(E)
    eventTime = "character",
    # The earliest ordinal day of the year on which the Event occurred (1 for January 1, 365 for December 31, except in a leap year, in which case it is 366) (http://rs.tdwg.org/dwc/terms/startDayOfYear)
    startDayOfYear = "character",
    # The latest ordinal day of the year on which the Event occurred (1 for January 1, 365 for December 31, except in a leap year, in which case it is 366) (	http://rs.tdwg.org/dwc/terms/endDayOfYear)
    endDayOfYear = "character",
    # The four-digit year in which the Event occurred, according to the Common Era Calendar (http://rs.tdwg.org/dwc/terms/year)
    year = "character",
    # The ordinal month in which the Event occurred (http://rs.tdwg.org/dwc/terms/month)
    month = "character",
    # The integer day of the month on which the Event occurred (http://rs.tdwg.org/dwc/terms/day)
    day = "character",
    # The verbatim original representation of the date and time information for an Event (http://rs.tdwg.org/dwc/terms/verbatimEventDate)
    verbatimEventDate = "character",
    # A category or description of the habitat in which the Event occurred (http://rs.tdwg.org/dwc/terms/habitat)
    habitat = "character",
    # The name of, reference to, or description of the method or protocol used during an Event (http://rs.tdwg.org/dwc/terms/samplingProtocol)
    samplingProtocol = "character",
    # A numeric value for a measurement of the size (time duration, length, area, or volume) of a sample in a sampling event (http://rs.tdwg.org/dwc/terms/sampleSizeValue)
    # A sampleSizeValue must have a corresponding sampleSizeUnit
    sampleSizeValue = "character",
    # The unit of measurement of the size (time duration, length, area, or volume) of a sample in a sampling event (http://rs.tdwg.org/dwc/terms/sampleSizeUnit)
    # A sampleSizeUnit must have a corresponding sampleSizeValue, e.g., 5 for sampleSizeValue with metre for sampleSizeUnit
    sampleSizeUnit = "character",
    # The amount of effort expended during an Event (http://rs.tdwg.org/dwc/terms/samplingEffort)
    samplingEffort = "character",
    # One of a) an indicator of the existence of, b) a reference to (publication, URI), or c) the text of notes taken in the field about the Event (http://rs.tdwg.org/dwc/terms/fieldNotes)
    fieldNotes = "character",
    # Comments or notes about the Event (http://rs.tdwg.org/dwc/terms/eventRemarks)
    eventRemarks = "character"
  ),
  contains = "DwCComponent"
)