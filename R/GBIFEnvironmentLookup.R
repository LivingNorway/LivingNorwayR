# ------ 1. GBIF ENVIRONMENT LOOKUP FUNCTION ------
#' Lookup up a GBIF class environment from a qualified name
#' @param qualName A \code{character} of qualifed names for the GBIF class to lookup
#' @return A \code{list} of environments representing \code{R6} classes derived from \code{DwCGeneric}
#' corresponding to each element in \code{qualName}.
GBIFClassLookup <- function(qualName) {
	inQualName <- tryCatch(as.character(qualName), error = function(err) {
		stop("error encountered processing the qualified class name:", err)
	})
	outVal <- list()
	if(length(inQualName) > 0) {
		outVal <- lapply(X = inQualName, FUN = function(curQualName) {
			switch(curQualName,
				"http://rs.tdwg.org/dwc/terms/Event" = GBIFEvent,
				"http://rs.tdwg.org/dwc/terms/Occurrence" = GBIFOccurrence,
				"http://rs.tdwg.org/dwc/terms/Taxon" = GBIFTaxon,
				"http://rs.tdwg.org/ac/terms/Multimedia" = GBIFMultimedia,
				"http://rs.tdwg.org/dwc/terms/Identification" = GBIFIdentification,
				"http://rs.tdwg.org/dwc/terms/MeasurementOrFact" = GBIFMeasurementOrFact,
				"http://rs.tdwg.org/dwc/terms/ResourceRelationship" = GBIFResourceRelationship,
				"http://rs.gbif.org/terms/1.0/Description" = GBIFDescription,
				"http://rs.gbif.org/terms/1.0/Distribution" = GBIFDistribution,
				"http://rs.gbif.org/terms/1.0/Identifier" = GBIFIdentifier,
				"http://rs.gbif.org/terms/1.0/Image" = GBIFImage,
				"http://rs.gbif.org/terms/1.0/Multimedia" = GBIFMultimedia,
				"http://rs.gbif.org/terms/1.0/Reference" = GBIFReference,
				"http://rs.gbif.org/terms/1.0/Releve" = GBIFReleve,
				"http://rs.gbif.org/terms/1.0/SpeciesProfile" = GBIFSpeciesProfile,
				"http://rs.gbif.org/terms/1.0/TypesAndSpecimen" = GBIFTypesAndSpecimen,
				"http://rs.gbif.org/terms/1.0/VernacularName" = GBIFVernacularName,
				"http://purl.org/germplasm/germplasmTerm#/GermplasmAccession" = GBIFGermplasmAccession,
				"http://purl.org/germplasm/germplasmTerm#/MeasurementScore" = GBIFMeasurementScore,
				"http://purl.org/germplasm/germplasmTerm#/MeasurementTrait" = GBIFMeasurementTrait,
				"http://purl.org/germplasm/germplasmTerm#/MeasurementTrial" = GBIFMeasurementTrial,
				"http://data.ggbn.org/schemas/ggbn/terms/Amplification" = GBIFAmplification,
				"http://data.ggbn.org/schemas/ggbn/terms/Cloning" = GBIFCloning,
				"http://data.ggbn.org/schemas/ggbn/terms/GelImage" = GBIFGelImage,
				"http://data.ggbn.org/schemas/ggbn/terms/Loan" = GBIFLoan,
				"http://data.ggbn.org/schemas/ggbn/terms/MaterialSample" = GBIFMaterialSample,
				"http://data.ggbn.org/schemas/ggbn/terms/Permit" = GBIFPermit,
				"http://data.ggbn.org/schemas/ggbn/terms/Preparation" = GBIFPreparation,
				"http://data.ggbn.org/schemas/ggbn/terms/Preservation" = GBIFPreservation,
				"http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample" = GBIFGermplasmSample,
				"http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact" = GBIFExtendedMeasurementOrFact,
				"http://rs.tdwg.org/chrono/terms/ChronometricAge" = GBIFChronometricAge,
				"http://zooarchnet.org/dwc/terms/ChronometricDate" = GBIFChronometricDate,
				NULL
			)
		})
	}
	outVal
}
