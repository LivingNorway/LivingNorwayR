# ------ 1. MEASUREMENTSCORE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF MeasurementScore class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMeasurementScoreTerm <- function() {
	outValue <- NULL
	if("http://purl.org/germplasm/germplasmTerm#/MeasurementScore" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementScore"]]$termInfo
	} else if("http://purl.org/germplasm/germplasmTerm#/MeasurementScore" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementScore"]]$termInfo
	}
	outValue
}

# ------ 2. MEASUREMENTSCORE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF MeasurementScore class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF MeasurementScore class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMeasurementScoreMembers <- function() {
	outValue <- NULL
	if("http://purl.org/germplasm/germplasmTerm#/MeasurementScore" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementScore"]]$compositeTerms
	} else if("http://purl.org/germplasm/germplasmTerm#/MeasurementScore" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementScore"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF MEASUREMENTSCORE CLASS ------
#' R6 class representing a data structure for a GBIF MeasurementScore augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFMeasurementScore} class allows for the specification of data tables that comply with the MeasurementScore
#' \url{http://purl.org/germplasm/germplasmTerm#/MeasurementScore}{class specification} of GBIF.
GBIFMeasurementScore <- R6Class("GBIFMeasurementScore",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF MeasurementScore class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF MeasurementScore class ======
		#' @description
		#' Create a new GBIFMeasurementScore object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFMeasurementScoreMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMeasurementScore
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMeasurementScoreMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFMeasurementScore} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMeasurementScoreMembers]{getGBIFMeasurementScoreMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFMeasurementScoreTerm(),
				associatedTerms = getGBIFMeasurementScoreMembers(),
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
#' Initialize a new GBIF MeasurementScore object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFMeasurementScoreMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMeasurementScore
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMeasurementScoreMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFMeasurementScore} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMeasurementScoreMembers]{getGBIFMeasurementScoreMembers}}
initializeGBIFMeasurementScore <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFMeasurementScore$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

