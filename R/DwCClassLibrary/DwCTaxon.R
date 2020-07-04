# A group of organisms (sensu http://purl.obolibrary.org/obo/OBI_0100026) considered by taxonomists to form a homogeneous unit (http://rs.tdwg.org/dwc/terms/Taxon)
setRefClass(
  Class = "DwCTaxon",
  fields = list(
    # An identifier for the set of taxon information (data associated with the Taxon class). May be a global unique identifier or an identifier specific to the data set (http://rs.tdwg.org/dwc/terms/taxonID)
    taxonID = "character",
    # An identifier for the nomenclatural (not taxonomic) details of a scientific name (http://rs.tdwg.org/dwc/terms/scientificNameID)
    scientificNameID = "character",
    # An identifier for the name usage (documented meaning of the name according to a source) of the currently valid (zoological) or accepted (botanical) taxon (http://rs.tdwg.org/dwc/terms/acceptedNameUsageID)
    acceptedNameUsageID = "character",
    # An identifier for the name usage (documented meaning of the name according to a source) of the direct, most proximate higher-rank parent taxon (in a classification) of the most specific element of the scientificName (http://rs.tdwg.org/dwc/terms/parentNameUsageID)
    parentNameUsageID = "character",
    # An identifier for the name usage (documented meaning of the name according to a source) in which the terminal element of the scientificName was originally established under the rules of the associated nomenclaturalCode (http://rs.tdwg.org/dwc/terms/originalNameUsageID)
    originalNameUsageID = "character",
    # An identifier for the source in which the specific taxon concept circumscription is defined or implied. See nameAccordingTo (http://rs.tdwg.org/dwc/terms/nameAccordingToID)
    nameAccordingToID = "character",
    # An identifier for the publication in which the scientificName was originally established under the rules of the associated nomenclaturalCode (http://rs.tdwg.org/dwc/terms/namePublishedInID)
    namePublishedInID = "character",
    # An identifier for the taxonomic concept to which the record refers - not for the nomenclatural details of a taxon (http://rs.tdwg.org/dwc/terms/taxonConceptID)
    taxonConceptID = "character",
    # The full scientific name, with authorship and date information if known. When forming part of an Identification, this should be the name in lowest level taxonomic rank that can be determined. This term should not contain identification qualifications, which should instead be supplied in the IdentificationQualifier term (http://rs.tdwg.org/dwc/terms/scientificName)
    scientificName = "character",
    # The full name, with authorship and date information if known, of the currently valid (zoological) or accepted (botanical) taxon (http://rs.tdwg.org/dwc/terms/acceptedNameUsage)
    acceptedNameUsage = "character",
    # The full name, with authorship and date information if known, of the direct, most proximate higher-rank parent taxon (in a classification) of the most specific element of the scientificName (http://rs.tdwg.org/dwc/terms/parentNameUsage)
    parentNameUsage = "character",
    # The taxon name, with authorship and date information if known, as it originally appeared when first established under the rules of the associated nomenclaturalCode. The basionym (botany) or basonym (bacteriology) of the scientificName or the senior/earlier homonym for replaced names (http://rs.tdwg.org/dwc/terms/originalNameUsage)
    originalNameUsage = "character",
    # The reference to the source in which the specific taxon concept circumscription is defined or implied - traditionally signified by the Latin "sensu" or "sec." (from secundum, meaning "according to"). For taxa that result from identifications, a reference to the keys, monographs, experts and other sources should be given (http://rs.tdwg.org/dwc/terms/nameAccordingTo)
    nameAccordingTo = "character",
    # A reference for the publication in which the scientificName was originally established under the rules of the associated nomenclaturalCode (http://rs.tdwg.org/dwc/terms/namePublishedIn)
    namePublishedIn = "character",
    # The four-digit year in which the scientificName was published (http://rs.tdwg.org/dwc/terms/namePublishedInYear)
    namePublishedInYear = "character",
    # A list (concatenated and separated) of taxa names terminating at the rank immediately superior to the taxon referenced in the taxon record (http://rs.tdwg.org/dwc/terms/higherClassification)
    # Recommended best practice is to separate the values in a list with space vertical bar space ( | ), with terms in order from the highest taxonomic rank to the lowest
    higherClassification = "character",
    # The full scientific name of the kingdom in which the taxon is classified (http://rs.tdwg.org/dwc/terms/kingdom)
    kingdom = "character",
    # The full scientific name of the phylum or division in which the taxon is classified (http://rs.tdwg.org/dwc/terms/phylum)
    phylum = "character",
    # The full scientific name of the class in which the taxon is classified (http://rs.tdwg.org/dwc/terms/class)
    class = "character",
    # The full scientific name of the order in which the taxon is classified (http://rs.tdwg.org/dwc/terms/order)
    order = "character",
    # The full scientific name of the family in which the taxon is classified (http://rs.tdwg.org/dwc/terms/family)
    family = "character",
    # The full scientific name of the genus in which the taxon is classified (http://rs.tdwg.org/dwc/terms/genus)
    genus = "character",
    # The full scientific name of the subgenus in which the taxon is classified. Values should include the genus to avoid homonym confusion (http://rs.tdwg.org/dwc/terms/subgenus)
    subgenus = "character",
    # The name of the first or species epithet of the scientificName (http://rs.tdwg.org/dwc/terms/specificEpithet)
    specificEpithet = "character",
    # The name of the lowest or terminal infraspecific epithet of the scientificName, excluding any rank designation (http://rs.tdwg.org/dwc/terms/infraspecificEpithet)
    infraspecificEpithet = "character",
    # The taxonomic rank of the most specific name in the scientificName (http://rs.tdwg.org/dwc/terms/taxonRank)
    # Recommended best practice is to use a controlled vocabulary
    taxonRank = "character",
    # The taxonomic rank of the most specific name in the scientificName as it appears in the original record (http://rs.tdwg.org/dwc/terms/verbatimTaxonRank)
    verbatimTaxonRank = "character",
    # The authorship information for the scientificName formatted according to the conventions of the applicable nomenclaturalCode (http://rs.tdwg.org/dwc/terms/scientificNameAuthorship)
    scientificNameAuthorship = "character",
    # A common or vernacular name (http://rs.tdwg.org/dwc/terms/vernacularName)
    vernacularName = "character",
    # The nomenclatural code (or codes in the case of an ambiregnal name) under which the scientificName is constructed (http://rs.tdwg.org/dwc/terms/nomenclaturalCode)
    # Recommended best practice is to use a controlled vocabulary
    nomenclaturalCode = "character",
    # The status of the use of the scientificName as a label for a taxon. Requires taxonomic opinion to define the scope of a taxon. Rules of priority then are used to define the taxonomic status of the nomenclature contained in that scope, combined with the experts opinion. It must be linked to a specific taxonomic reference that defines the concept (http://rs.tdwg.org/dwc/terms/taxonomicStatus)
    # Recommended best practice is to use a controlled vocabulary
    taxonomicStatus = "character",
    # The status related to the original publication of the name and its conformance to the relevant rules of nomenclature. It is based essentially on an algorithm according to the business rules of the code. It requires no taxonomic opinion (http://rs.tdwg.org/dwc/terms/nomenclaturalStatus)
    nomenclaturalStatus = "character",
    # Comments or notes about the taxon or name (http://rs.tdwg.org/dwc/terms/taxonRemarks)
    taxonRemarks = "character"
  ),
  contains = "DwCComponent"
)
