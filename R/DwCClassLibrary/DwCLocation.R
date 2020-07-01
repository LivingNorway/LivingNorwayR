# A spatial region or named place (http://purl.org/dc/terms/Location)
setRefClass(
  Class = "DwCLocation",
  fields = list(
    # An identifier for the set of location information (data associated with dcterms:Location). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/locationID)
    locationID = "character",
    # An identifier for the geographic region within which the Location occurred (http://rs.tdwg.org/dwc/terms/higherGeographyID)
    # Recommended best practice is to use a persistent identifier from a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    higherGeographyID = "character",
    # A list (concatenated and separated) of geographic names less specific than the information captured in the locality term (http://rs.tdwg.org/dwc/terms/higherGeography)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | ), with terms in order from least specific to most specific
    higherGeography = "character",
    # The name of the continent in which the Location occurs (http://rs.tdwg.org/dwc/terms/continent)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    continent = "character",
    # The name of the water body in which the Location occurs (http://rs.tdwg.org/dwc/terms/waterBody)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    waterBody = "character",
    # The name of the island group in which the Location occurs (http://rs.tdwg.org/dwc/terms/islandGroup)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    islandGroup = "character",
    # The name of the island on or near which the Location occurs (http://rs.tdwg.org/dwc/terms/island)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    island = "character",
    # The name of the country or major administrative unit in which the Location occurs (http://rs.tdwg.org/dwc/terms/country)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    country = "character",
    # The standard code for the country in which the Location occurs (http://rs.tdwg.org/dwc/terms/countryCode)
    # Recommended best practice is to use an ISO 3166-1-alpha-2 country code
    countryCode = "character",
    # The name of the next smaller administrative region than country (state, province, canton, department, region, etc.) in which the Location occurs (http://rs.tdwg.org/dwc/terms/stateProvince)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    stateProvince = "character",
    # The full, unabbreviated name of the next smaller administrative region than stateProvince (county, shire, department, etc.) in which the Location occurs (http://rs.tdwg.org/dwc/terms/county)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    county = "character",
    # The full, unabbreviated name of the next smaller administrative region than county (city, municipality, etc.) in which the Location occurs. Do not use this term for a nearby named place that does not contain the actual location (http://rs.tdwg.org/dwc/terms/municipality)
    # Recommended best practice is to use a controlled vocabulary such as the Getty Thesaurus of Geographic Names
    municipality = "character",
    # The specific description of the place. Less specific geographic information can be provided in other geographic terms (higherGeography, continent, country, stateProvince, county, municipality, waterBody, island, islandGroup). This term may contain information modified from the original to correct perceived errors or standardize the description (http://rs.tdwg.org/dwc/terms/locality)
    locality = "character",
    # The original textual description of the place (http://rs.tdwg.org/dwc/terms/verbatimLocality)
    verbatimLocality = "character",
    # The lower limit of the range of elevation (altitude, usually above sea level), in meters (http://rs.tdwg.org/dwc/terms/minimumElevationInMeters)
    minimumElevationInMeters = "character",
    # The upper limit of the range of elevation (altitude, usually above sea level), in meters (http://rs.tdwg.org/dwc/terms/maximumElevationInMeters)
    maximumElevationInMeters = "character",
    # The original description of the elevation (altitude, usually above sea level) of the Location (http://rs.tdwg.org/dwc/terms/verbatimElevation)
    verbatimElevation = "character",
    # The lesser depth of a range of depth below the local surface, in meters (http://rs.tdwg.org/dwc/terms/minimumDepthInMeters)
    minimumDepthInMeters = "character",
    # The greater depth of a range of depth below the local surface, in meters (http://rs.tdwg.org/dwc/terms/maximumDepthInMeters)
    maximumDepthInMeters = "character",
    # The original description of the depth below the local surface (http://rs.tdwg.org/dwc/terms/verbatimDepth)
    verbatimDepth = "character",
    # The lesser distance in a range of distance from a reference surface in the vertical direction, in meters. Use positive values for locations above the surface, negative values for locations below. If depth measures are given, the reference surface is the location given by the depth, otherwise the reference surface is the location given by the elevation (http://rs.tdwg.org/dwc/terms/minimumDistanceAboveSurfaceInMeters)
    minimumDistanceAboveSurfaceInMeters = "character",
    # The greater distance in a range of distance from a reference surface in the vertical direction, in meters. Use positive values for locations above the surface, negative values for locations below. If depth measures are given, the reference surface is the location given by the depth, otherwise the reference surface is the location given by the elevation (http://rs.tdwg.org/dwc/terms/maximumDistanceAboveSurfaceInMeters)
    maximumDistanceAboveSurfaceInMeters = "character",
    # Information about the source of this Location information. Could be a publication (gazetteer), institution, or team of individuals (http://rs.tdwg.org/dwc/terms/locationAccordingTo)
    locationAccordingTo = "character",
    # Comments or notes about the Location (http://rs.tdwg.org/dwc/terms/locationRemarks)
    locationRemarks = "character",
    # The geographic latitude (in decimal degrees, using the spatial reference system given in geodeticDatum) of the geographic center of a Location. Positive values are north of the Equator, negative values are south of it. Legal values lie between -90 and 90, inclusive (http://rs.tdwg.org/dwc/terms/decimalLatitude)
    decimalLatitude = "character",
    # The geographic longitude (in decimal degrees, using the spatial reference system given in geodeticDatum) of the geographic center of a Location. Positive values are east of the Greenwich Meridian, negative values are west of it. Legal values lie between -180 and 180, inclusive (http://rs.tdwg.org/dwc/terms/decimalLongitude)
    decimalLongitude = "character",
    # The ellipsoid, geodetic datum, or spatial reference system (SRS) upon which the geographic coordinates given in decimalLatitude and decimalLongitude as based (http://rs.tdwg.org/dwc/terms/geodeticDatum)
    # Recommended best practice is to use the EPSG code of the SRS, if known. Otherwise use a controlled vocabulary for the name or code of the geodetic datum, if known. Otherwise use a controlled vocabulary for the name or code of the ellipsoid, if known. If none of these is known, use the value unknown
    geodeticDatum = "character",
    # The horizontal distance (in meters) from the given decimalLatitude and decimalLongitude describing the smallest circle containing the whole of the Location. Leave the value empty if the uncertainty is unknown, cannot be estimated, or is not applicable (because there are no coordinates). Zero is not a valid value for this term (http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters)
    coordinateUncertaintyInMeters = "character",
    # A decimal representation of the precision of the coordinates given in the decimalLatitude and decimalLongitude (http://rs.tdwg.org/dwc/terms/coordinatePrecision)
    coordinatePrecision = "character",
    # The ratio of the area of the point-radius (decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters) to the area of the true (original, or most specific) spatial representation of the Location. Legal values are 0, greater than or equal to 1, or undefined. A value of 1 is an exact match or 100% overlap. A value of 0 should be used if the given point-radius does not completely contain the original representation. The pointRadiusSpatialFit is undefined (and should be left blank) if the original representation is a point without uncertainty and the given georeference is not that same point (without uncertainty). If both the original and the given georeference are the same point, the pointRadiusSpatialFit is 1 (http://rs.tdwg.org/dwc/terms/pointRadiusSpatialFit)
    # Detailed explanations with graphical examples can be found in the Guide to Best Practices for Georeferencing, Chapman and Wieczorek, eds. 2006
    pointRadiusSpatialFit = "character",
    # The verbatim original spatial coordinates of the Location. The coordinate ellipsoid, geodeticDatum, or full Spatial Reference System (SRS) for these coordinates should be stored in verbatimSRS and the coordinate system should be stored in verbatimCoordinateSystem (	http://rs.tdwg.org/dwc/terms/verbatimCoordinates)
    verbatimCoordinates = "character",
    # The verbatim original latitude of the Location. The coordinate ellipsoid, geodeticDatum, or full Spatial Reference System (SRS) for these coordinates should be stored in verbatimSRS and the coordinate system should be stored in verbatimCoordinateSystem (http://rs.tdwg.org/dwc/terms/verbatimLatitude)
    verbatimLatitude = "character",
    # The verbatim original longitude of the Location. The coordinate ellipsoid, geodeticDatum, or full Spatial Reference System (SRS) for these coordinates should be stored in verbatimSRS and the coordinate system should be stored in verbatimCoordinateSystem (http://rs.tdwg.org/dwc/terms/verbatimLongitude)
    verbatimLongitude = "character",
    # The spatial coordinate system for the verbatimLatitude and verbatimLongitude or the verbatimCoordinates of the Location (http://rs.tdwg.org/dwc/terms/verbatimCoordinateSystem)
    # Recommended best practice is to use a controlled vocabulary
    verbatimCoordinateSystem = "character",
    # The ellipsoid, geodetic datum, or spatial reference system (SRS) upon which coordinates given in verbatimLatitude and verbatimLongitude, or verbatimCoordinates are based (http://rs.tdwg.org/dwc/terms/verbatimSRS)
    # Recommended best practice is to use the EPSG code of the SRS, if known. Otherwise use a controlled vocabulary for the name or code of the geodetic datum, if known. Otherwise use a controlled vocabulary for the name or code of the ellipsoid, if known. If none of these is known, use the value unknown.
    verbatimSRS = "character",
    # A Well-Known Text (WKT) representation of the shape (footprint, geometry) that defines the Location. A Location may have both a point-radius representation (see decimalLatitude) and a footprint representation, and they may differ from each other (http://rs.tdwg.org/dwc/terms/footprintWKT)
    footprintWKT = "character",
    # A Well-Known Text (WKT) representation of the Spatial Reference System (SRS) for the footprintWKT of the Location. Do not use this term to describe the SRS of the decimalLatitude and decimalLongitude, even if it is the same as for the footprintWKT - use the geodeticDatum instead (http://rs.tdwg.org/dwc/terms/footprintSRS)
    footprintSRS = "character",
    # The ratio of the area of the footprint (footprintWKT) to the area of the true (original, or most specific) spatial representation of the Location. Legal values are 0, greater than or equal to 1, or undefined. A value of 1 is an exact match or 100% overlap. A value of 0 should be used if the given footprint does not completely contain the original representation. The footprintSpatialFit is undefined (and should be left blank) if the original representation is a point and the given georeference is not that same point. If both the original and the given georeference are the same point, the footprintSpatialFit is 1 (http://rs.tdwg.org/dwc/terms/footprintSpatialFit)
    # Detailed explanations with graphical examples can be found in the Guide to Best Practices for Georeferencing, Chapman and Wieczorek, eds. 2006
    footprintSpatialFit = "character",
    # A list (concatenated and separated) of names of people, groups, or organizations who determined the georeference (spatial representation) for the Location (http://rs.tdwg.org/dwc/terms/georeferencedBy)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    georeferencedBy = "character",
    # The date on which the Location was georeferenced (http://rs.tdwg.org/dwc/terms/georeferencedDate)
    # Recommended best practice is to use a date that conforms to ISO 8601:2004(E)
    georeferencedDate = "character",
    # A description or reference to the methods used to determine the spatial footprint, coordinates, and uncertainties (http://rs.tdwg.org/dwc/terms/georeferenceProtocol)
    georeferenceProtocol = "character",
    # A list (concatenated and separated) of maps, gazetteers, or other resources used to georeference the Location, described specifically enough to allow anyone in the future to use the same resources (http://rs.tdwg.org/dwc/terms/georeferenceSources)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | )
    georeferenceSources = "character",
    # A categorical description of the extent to which the georeference has been verified to represent the best possible spatial description (http://rs.tdwg.org/dwc/terms/georeferenceVerificationStatus)
    # Recommended best practice is to use a controlled vocabulary
    georeferenceVerificationStatus = "character",
    # Notes or comments about the spatial description determination, explaining assumptions made in addition or opposition to the those formalized in the method referred to in georeferenceProtocol (http://rs.tdwg.org/dwc/terms/georeferenceRemarks)
    georeferenceRemarks = "character"
  ),
  contains = "DwCComponent"
)