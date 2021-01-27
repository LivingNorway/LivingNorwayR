# ------ 1. IDENTIFICATION TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Identification class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFIdentificationTerm <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/Identification" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Identification"]]$termInfo
	} else if("http://rs.tdwg.org/dwc/terms/Identification" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/Identification"]]$termInfo
	}
	outValue
}

# ------ 2. IDENTIFICATION MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Identification class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Identification class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFIdentificationMembers <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/Identification" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Identification"]]$compositeTerms
	} else if("http://rs.tdwg.org/dwc/terms/Identification" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/Identification"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF IDENTIFICATION CLASS ------
#' R6 class representing a data structure for a GBIF Identification augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFIdentification} class allows for the specification of data tables that comply with the Identification
#' \url{http://rs.tdwg.org/dwc/terms/Identification}{class specification} of GBIF.
GBIFIdentification <- R6Class("GBIFIdentification",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Identification class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Identification class ======
		#' @description
		#' Create a new GBIFIdentification object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFIdentificationMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFIdentification
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFIdentificationMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFIdentification} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFIdentificationMembers]{getGBIFIdentificationMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				getGBIFIdentificationTerm(),
				getGBIFIdentificationMembers(),
				objectData,
				idColumnInfo,
				nameAutoMap,
				defDateFormat,
				...
			)
			invisible(self)
		}
	)
)

# ------ 4. INITIALISATION FUNCTION ------
#' Initialize a new GBIF Identification object
#' @param objectData A \code{data.frame} containing the data to import into the object
t#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFIdentificationMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFIdentification
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFIdentificationMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFIdentification} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFIdentificationMembers]{getGBIFIdentificationMembers}}
initializeGBIFIdentification <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFIdentification$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

