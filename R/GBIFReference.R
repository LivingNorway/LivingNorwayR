# ------ 1. REFERENCE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Reference class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFReferenceTerm <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/Reference" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/Reference"]]$termInfo
	} else if("http://rs.gbif.org/terms/1.0/Reference" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/Reference"]]$termInfo
	}
	outValue
}

# ------ 2. REFERENCE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Reference class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Reference class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFReferenceMembers <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/Reference" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/Reference"]]$compositeTerms
	} else if("http://rs.gbif.org/terms/1.0/Reference" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/Reference"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF REFERENCE CLASS ------
#' R6 class representing a data structure for a GBIF Reference augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFReference} class allows for the specification of data tables that comply with the Reference
#' \url{http://rs.gbif.org/terms/1.0/Reference}{class specification} of GBIF.
GBIFReference <- R6Class("GBIFReference",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Reference class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Reference class ======
		#' @description
		#' Create a new GBIFReference object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFReferenceMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFReference
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFReferenceMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFReference} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFReferenceMembers]{getGBIFReferenceMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFReferenceTerm(),
				associatedTerms = getGBIFReferenceMembers(),
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
#' Initialize a new GBIF Reference object
#' @param objectData A \code{data.frame} containing the data to import into the object
t#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFReferenceMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFReference
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFReferenceMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFReference} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFReferenceMembers]{getGBIFReferenceMembers}}
initializeGBIFReference <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFReference$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

