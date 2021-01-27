# ------ 1. CLONING TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Cloning class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFCloningTerm <- function() {
	outValue <- NULL
	if("http://data.ggbn.org/schemas/ggbn/terms/Cloning" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://data.ggbn.org/schemas/ggbn/terms/Cloning"]]$termInfo
	} else if("http://data.ggbn.org/schemas/ggbn/terms/Cloning" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://data.ggbn.org/schemas/ggbn/terms/Cloning"]]$termInfo
	}
	outValue
}

# ------ 2. CLONING MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Cloning class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Cloning class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFCloningMembers <- function() {
	outValue <- NULL
	if("http://data.ggbn.org/schemas/ggbn/terms/Cloning" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://data.ggbn.org/schemas/ggbn/terms/Cloning"]]$compositeTerms
	} else if("http://data.ggbn.org/schemas/ggbn/terms/Cloning" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://data.ggbn.org/schemas/ggbn/terms/Cloning"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF CLONING CLASS ------
#' R6 class representing a data structure for a GBIF Cloning augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFCloning} class allows for the specification of data tables that comply with the Cloning
#' \url{http://data.ggbn.org/schemas/ggbn/terms/Cloning}{class specification} of GBIF.
GBIFCloning <- R6Class("GBIFCloning",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Cloning class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Cloning class ======
		#' @description
		#' Create a new GBIFCloning object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFCloningMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFCloning
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFCloningMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFCloning} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFCloningMembers]{getGBIFCloningMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				getGBIFCloningTerm(),
				getGBIFCloningMembers(),
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
#' Initialize a new GBIF Cloning object
#' @param objectData A \code{data.frame} containing the data to import into the object
t#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFCloningMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFCloning
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFCloningMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFCloning} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFCloningMembers]{getGBIFCloningMembers}}
initializeGBIFCloning <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFCloning$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

