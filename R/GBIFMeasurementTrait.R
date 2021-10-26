# ------ 1. MEASUREMENTTRAIT TERM INFORMATION ------
#' Return the information of the term associated with the GBIF MeasurementTrait class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMeasurementTraitTerm <- function() {
	outValue <- NULL
	if("http://purl.org/germplasm/germplasmTerm#/MeasurementTrait" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementTrait"]]$termInfo
	} else if("http://purl.org/germplasm/germplasmTerm#/MeasurementTrait" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementTrait"]]$termInfo
	}
	outValue
}

# ------ 2. MEASUREMENTTRAIT MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF MeasurementTrait class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF MeasurementTrait class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMeasurementTraitMembers <- function() {
	outValue <- NULL
	if("http://purl.org/germplasm/germplasmTerm#/MeasurementTrait" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementTrait"]]$compositeTerms
	} else if("http://purl.org/germplasm/germplasmTerm#/MeasurementTrait" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://purl.org/germplasm/germplasmTerm#/MeasurementTrait"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF MEASUREMENTTRAIT CLASS ------
#' R6 class representing a data structure for a GBIF MeasurementTrait augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFMeasurementTrait} class allows for the specification of data tables that comply with the MeasurementTrait
#' \url{http://purl.org/germplasm/germplasmTerm#/MeasurementTrait}{class specification} of GBIF.
GBIFMeasurementTrait <- R6Class("GBIFMeasurementTrait",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF MeasurementTrait class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF MeasurementTrait class ======
		#' @description
		#' Create a new GBIFMeasurementTrait object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFMeasurementTraitMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMeasurementTrait
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMeasurementTraitMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFMeasurementTrait} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMeasurementTraitMembers]{getGBIFMeasurementTraitMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFMeasurementTraitTerm(),
				associatedTerms = getGBIFMeasurementTraitMembers(),
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
#' Initialize a new GBIF MeasurementTrait object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFMeasurementTraitMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMeasurementTrait
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMeasurementTraitMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFMeasurementTrait} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMeasurementTraitMembers]{getGBIFMeasurementTraitMembers}}
initializeGBIFMeasurementTrait <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFMeasurementTrait$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

