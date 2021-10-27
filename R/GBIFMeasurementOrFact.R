# ------ 1. MEASUREMENTORFACT TERM INFORMATION ------
#' Return the information of the term associated with the GBIF MeasurementOrFact class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMeasurementOrFactTerm <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/MeasurementOrFact" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]]$termInfo
	} else if("http://rs.tdwg.org/dwc/terms/MeasurementOrFact" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]]$termInfo
	}
	outValue
}

# ------ 2. MEASUREMENTORFACT MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF MeasurementOrFact class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF MeasurementOrFact class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMeasurementOrFactMembers <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/MeasurementOrFact" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]]$compositeTerms
	} else if("http://rs.tdwg.org/dwc/terms/MeasurementOrFact" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF MEASUREMENTORFACT CLASS ------
#' R6 class representing a data structure for a GBIF MeasurementOrFact augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFMeasurementOrFact} class allows for the specification of data tables that comply with the MeasurementOrFact
#' \url{http://rs.tdwg.org/dwc/terms/MeasurementOrFact}{class specification} of GBIF.
GBIFMeasurementOrFact <- R6Class("GBIFMeasurementOrFact",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF MeasurementOrFact class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF MeasurementOrFact class ======
		#' @description
		#' Create a new GBIFMeasurementOrFact object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFMeasurementOrFactMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMeasurementOrFact
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMeasurementOrFactMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFMeasurementOrFact} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMeasurementOrFactMembers]{getGBIFMeasurementOrFactMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFMeasurementOrFactTerm(),
				associatedTerms = getGBIFMeasurementOrFactMembers(),
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
#' Initialize a new GBIF MeasurementOrFact object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFMeasurementOrFactMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMeasurementOrFact
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMeasurementOrFactMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFMeasurementOrFact} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMeasurementOrFactMembers]{getGBIFMeasurementOrFactMembers}}
#' @export
initializeGBIFMeasurementOrFact <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFMeasurementOrFact$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

