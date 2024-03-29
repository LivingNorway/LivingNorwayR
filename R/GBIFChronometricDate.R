# ------ 1. CHRONOMETRICDATE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF ChronometricDate class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFChronometricDateTerm <- function() {
	outValue <- NULL
	if("http://zooarchnet.org/dwc/terms/ChronometricDate" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://zooarchnet.org/dwc/terms/ChronometricDate"]]$termInfo
	} else if("http://zooarchnet.org/dwc/terms/ChronometricDate" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://zooarchnet.org/dwc/terms/ChronometricDate"]]$termInfo
	}
	outValue
}

# ------ 2. CHRONOMETRICDATE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF ChronometricDate class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF ChronometricDate class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFChronometricDateMembers <- function() {
	outValue <- NULL
	if("http://zooarchnet.org/dwc/terms/ChronometricDate" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://zooarchnet.org/dwc/terms/ChronometricDate"]]$compositeTerms
	} else if("http://zooarchnet.org/dwc/terms/ChronometricDate" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://zooarchnet.org/dwc/terms/ChronometricDate"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF CHRONOMETRICDATE CLASS ------
#' R6 class representing a data structure for a GBIF ChronometricDate augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFChronometricDate} class allows for the specification of data tables that comply with the ChronometricDate
#' \url{http://zooarchnet.org/dwc/terms/ChronometricDate}{class specification} of GBIF.
GBIFChronometricDate <- R6Class("GBIFChronometricDate",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF ChronometricDate class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF ChronometricDate class ======
		#' @description
		#' Create a new GBIFChronometricDate object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFChronometricDateMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFChronometricDate
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFChronometricDateMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFChronometricDate} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFChronometricDateMembers]{getGBIFChronometricDateMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFChronometricDateTerm(),
				associatedTerms = getGBIFChronometricDateMembers(),
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
#' Initialize a new GBIF ChronometricDate object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFChronometricDateMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFChronometricDate
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFChronometricDateMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFChronometricDate} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFChronometricDateMembers]{getGBIFChronometricDateMembers}}
initializeGBIFChronometricDate <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFChronometricDate$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

