# ------ 1. PERMIT TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Permit class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFPermitTerm <- function() {
	outValue <- NULL
	if("http://data.ggbn.org/schemas/ggbn/terms/Permit" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://data.ggbn.org/schemas/ggbn/terms/Permit"]]$termInfo
	} else if("http://data.ggbn.org/schemas/ggbn/terms/Permit" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://data.ggbn.org/schemas/ggbn/terms/Permit"]]$termInfo
	}
	outValue
}

# ------ 2. PERMIT MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Permit class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Permit class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFPermitMembers <- function() {
	outValue <- NULL
	if("http://data.ggbn.org/schemas/ggbn/terms/Permit" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://data.ggbn.org/schemas/ggbn/terms/Permit"]]$compositeTerms
	} else if("http://data.ggbn.org/schemas/ggbn/terms/Permit" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://data.ggbn.org/schemas/ggbn/terms/Permit"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF PERMIT CLASS ------
#' R6 class representing a data structure for a GBIF Permit augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFPermit} class allows for the specification of data tables that comply with the Permit
#' \url{http://data.ggbn.org/schemas/ggbn/terms/Permit}{class specification} of GBIF.
GBIFPermit <- R6Class("GBIFPermit",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Permit class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Permit class ======
		#' @description
		#' Create a new GBIFPermit object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFPermitMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFPermit
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFPermitMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFPermit} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFPermitMembers]{getGBIFPermitMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFPermitTerm(),
				associatedTerms = getGBIFPermitMembers(),
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
#' Initialize a new GBIF Permit object
#' @param objectData A \code{data.frame} containing the data to import into the object
t#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFPermitMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFPermit
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFPermitMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFPermit} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFPermitMembers]{getGBIFPermitMembers}}
initializeGBIFPermit <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFPermit$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

