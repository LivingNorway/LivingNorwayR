# ------ 1. EXTENDEDMEASUREMENTORFACT TERM INFORMATION ------
#' Return the information of the term associated with the GBIF ExtendedMeasurementOrFact class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFExtendedMeasurementOrFactTerm <- function() {
	outValue <- NULL
	if("http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"]]$termInfo
	} else if("http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"]]$termInfo
	}
	outValue
}

# ------ 2. EXTENDEDMEASUREMENTORFACT MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF ExtendedMeasurementOrFact class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF ExtendedMeasurementOrFact class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFExtendedMeasurementOrFactMembers <- function() {
	outValue <- NULL
	if("http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"]]$compositeTerms
	} else if("http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF EXTENDEDMEASUREMENTORFACT CLASS ------
#' R6 class representing a data structure for a GBIF ExtendedMeasurementOrFact augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFExtendedMeasurementOrFact} class allows for the specification of data tables that comply with the ExtendedMeasurementOrFact
#' \url{http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact}{class specification} of GBIF.
GBIFExtendedMeasurementOrFact <- R6Class("GBIFExtendedMeasurementOrFact",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF ExtendedMeasurementOrFact class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF ExtendedMeasurementOrFact class ======
		#' @description
		#' Create a new GBIFExtendedMeasurementOrFact object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFExtendedMeasurementOrFactMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFExtendedMeasurementOrFact
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFExtendedMeasurementOrFactMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFExtendedMeasurementOrFact} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFExtendedMeasurementOrFactMembers]{getGBIFExtendedMeasurementOrFactMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFExtendedMeasurementOrFactTerm(),
				associatedTerms = getGBIFExtendedMeasurementOrFactMembers(),
				objectData = objectData,
				idColumnInfo = idColumnInfo,
				nameAutoMap = nameAutoMap,
				defDateFormat = defDateFormat,
				...
			)
			invisible(self)
		}
	)
)

# ------ 4. INITIALISATION FUNCTION ------
#' Initialize a new GBIF ExtendedMeasurementOrFact object
#' @param objectData A \code{data.frame} containing the data to import into the object
t#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFExtendedMeasurementOrFactMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFExtendedMeasurementOrFact
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFExtendedMeasurementOrFactMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFExtendedMeasurementOrFact} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFExtendedMeasurementOrFactMembers]{getGBIFExtendedMeasurementOrFactMembers}}
initializeGBIFExtendedMeasurementOrFact <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFExtendedMeasurementOrFact$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

