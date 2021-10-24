# ------ 1. RELEVE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Releve class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFReleveTerm <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/Releve" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/Releve"]]$termInfo
	} else if("http://rs.gbif.org/terms/1.0/Releve" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/Releve"]]$termInfo
	}
	outValue
}

# ------ 2. RELEVE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Releve class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Releve class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFReleveMembers <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/Releve" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/Releve"]]$compositeTerms
	} else if("http://rs.gbif.org/terms/1.0/Releve" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/Releve"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF RELEVE CLASS ------
#' R6 class representing a data structure for a GBIF Releve augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFReleve} class allows for the specification of data tables that comply with the Releve
#' \url{http://rs.gbif.org/terms/1.0/Releve}{class specification} of GBIF.
GBIFReleve <- R6Class("GBIFReleve",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Releve class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Releve class ======
		#' @description
		#' Create a new GBIFReleve object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFReleveMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFReleve
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFReleveMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFReleve} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFReleveMembers]{getGBIFReleveMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFReleveTerm(),
				associatedTerms = getGBIFReleveMembers(),
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
#' Initialize a new GBIF Releve object
#' @description Initializes a new GBIF Releve object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFReleveMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFReleve
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFReleveMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFReleve} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFReleveMembers]{getGBIFReleveMembers}}
initializeGBIFReleve <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFReleve$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

