# Geological information, such as stratigraphy, that qualifies a region or place (http://rs.tdwg.org/dwc/terms/GeologicalContext)
setRefClass(
  Class = "DwCGeologicalContext",
  fields = list(
    # An identifier for the set of information associated with a GeologicalContext (the location within a geological context, such as stratigraphy). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/geologicalContextID)
    geologicalContextID = "character",
    # The full name of the earliest possible geochronologic eon or lowest chrono-stratigraphic eonothem or the informal name ("Precambrian") attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/earliestEonOrLowestEonothem)
    earliestEonOrLowestEonothem = "character",
    # The full name of the latest possible geochronologic eon or highest chrono-stratigraphic eonothem or the informal name ("Precambrian") attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/latestEonOrHighestEonothem)
    latestEonOrHighestEonothem = "character",
    # The full name of the earliest possible geochronologic era or lowest chronostratigraphic erathem attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/earliestEraOrLowestErathem)
    earliestEraOrLowestErathem = "character",
    # The full name of the latest possible geochronologic era or highest chronostratigraphic erathem attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/latestEraOrHighestErathem)
    latestEraOrHighestErathem = "character",
    # The full name of the earliest possible geochronologic period or lowest chronostratigraphic system attributable to the stratigraphic horizon from which the cataloged item was collected (	http://rs.tdwg.org/dwc/terms/earliestPeriodOrLowestSystem)
    earliestPeriodOrLowestSystem = "character",
    # The full name of the latest possible geochronologic period or highest chronostratigraphic system attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/latestPeriodOrHighestSystem)
    latestPeriodOrHighestSystem = "character",
    # The full name of the earliest possible geochronologic epoch or lowest chronostratigraphic series attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/earliestEpochOrLowestSeries)
    earliestEpochOrLowestSeries = "character",
    # The full name of the latest possible geochronologic epoch or highest chronostratigraphic series attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/latestEpochOrHighestSeries)
    latestEpochOrHighestSeries = "character",
    # The full name of the earliest possible geochronologic age or lowest chronostratigraphic stage attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/earliestAgeOrLowestStage)
    earliestAgeOrLowestStage = "character",
    # The full name of the latest possible geochronologic age or highest chronostratigraphic stage attributable to the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/latestAgeOrHighestStage)
    lowestAgeOrHighestStage = "character",
    # The full name of the lowest possible geological biostratigraphic zone of the stratigraphic horizon from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/lowestBiostratigraphicZone)
    lowestBiostratigraphicZone = "character",
    # The full name of the highest possible geological biostratigraphic zone of the stratigraphic horizon from which the cataloged item was collected (	http://rs.tdwg.org/dwc/terms/highestBiostratigraphicZone)
    highestBiostratigraphicZone = "character",
    # The combination of all litho-stratigraphic names for the rock from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/lithostratigraphicTerms)
    lithostratigraphicTerms = "character",
    # The full name of the lithostratigraphic group from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/group)
    group = "character",
    # The full name of the lithostratigraphic formation from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/formation)
    formation = "character",
    # The full name of the lithostratigraphic member from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/member)
    member = "character",
    # The full name of the lithostratigraphic bed from which the cataloged item was collected (http://rs.tdwg.org/dwc/terms/bed)
    bed = "character"
  ),
  contains = "DwCComponents"
)