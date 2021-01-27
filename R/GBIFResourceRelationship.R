# ------ 1. RESOURCERELATIONSHIP TERM INFORMATION ------
#' Return the information of the term associated with the GBIF ResourceRelationship class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFResourceRelationshipTerm <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/ResourceRelationship" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/ResourceRelationship"]]$termInfo
	} else if("http://rs.tdwg.org/dwc/terms/ResourceRelationship" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/ResourceRelationship"]]$termInfo
	}
	outValue
}

# ------ 2. RESOURCERELATIONSHIP MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF ResourceRelationship class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF ResourceRelationship class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFResourceRelationshipMembers <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/ResourceRelationship" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/ResourceRelationship"]]$compositeTerms
	} else if("http://rs.tdwg.org/dwc/terms/ResourceRelationship" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/ResourceRelationship"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF RESOURCERELATIONSHIP CLASS ------
#' R6 class representing a data structure for a GBIF ResourceRelationship augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFResourceRelationship} class allows for the specification of data tables that comply with the ResourceRelationship
#' \url{http://rs.tdwg.org/dwc/terms/ResourceRelationship}{class specification} of GBIF.
GBIFResourceRelationship <- R6Class("GBIFResourceRelationship",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF ResourceRelationship class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF ResourceRelationship class ======
		#' @description
		#' Create a new GBIFResourceRelationship object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFResourceRelationshipMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFResourceRelationship
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFResourceRelationshipMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFResourceRelationship} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFResourceRelationshipMembers]{getGBIFResourceRelationshipMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				getGBIFResourceRelationshipTerm(),
				getGBIFResourceRelationshipMembers(),
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
#' Initialize a new GBIF ResourceRelationship object
#' @param objectData A \code{data.frame} containing the data to import into the object
t#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFResourceRelationshipMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFResourceRelationship
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFResourceRelationshipMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFResourceRelationship} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFResourceRelationshipMembers]{getGBIFResourceRelationshipMembers}}
initializeGBIFResourceRelationship <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFResourceRelationship$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

