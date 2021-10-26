# ------ 1. DESCRIPTION TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Description class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFDescriptionTerm <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/Description" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/Description"]]$termInfo
	} else if("http://rs.gbif.org/terms/1.0/Description" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/Description"]]$termInfo
	}
	outValue
}

# ------ 2. DESCRIPTION MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Description class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Description class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFDescriptionMembers <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/Description" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/Description"]]$compositeTerms
	} else if("http://rs.gbif.org/terms/1.0/Description" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/Description"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF DESCRIPTION CLASS ------
#' R6 class representing a data structure for a GBIF Description augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFDescription} class allows for the specification of data tables that comply with the Description
#' \url{http://rs.gbif.org/terms/1.0/Description}{class specification} of GBIF.
GBIFDescription <- R6Class("GBIFDescription",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Description class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Description class ======
		#' @description
		#' Create a new GBIFDescription object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFDescriptionMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFDescription
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFDescriptionMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFDescription} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFDescriptionMembers]{getGBIFDescriptionMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFDescriptionTerm(),
				associatedTerms = getGBIFDescriptionMembers(),
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
#' Initialize a new GBIF Description object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFDescriptionMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFDescription
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFDescriptionMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFDescription} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFDescriptionMembers]{getGBIFDescriptionMembers}}
initializeGBIFDescription <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFDescription$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

