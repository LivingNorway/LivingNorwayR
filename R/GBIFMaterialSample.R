# ------ 1. MATERIALSAMPLE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF MaterialSample class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMaterialSampleTerm <- function() {
	outValue <- NULL
	if("http://data.ggbn.org/schemas/ggbn/terms/MaterialSample" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://data.ggbn.org/schemas/ggbn/terms/MaterialSample"]]$termInfo
	} else if("http://data.ggbn.org/schemas/ggbn/terms/MaterialSample" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://data.ggbn.org/schemas/ggbn/terms/MaterialSample"]]$termInfo
	}
	outValue
}

# ------ 2. MATERIALSAMPLE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF MaterialSample class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF MaterialSample class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFMaterialSampleMembers <- function() {
	outValue <- NULL
	if("http://data.ggbn.org/schemas/ggbn/terms/MaterialSample" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://data.ggbn.org/schemas/ggbn/terms/MaterialSample"]]$compositeTerms
	} else if("http://data.ggbn.org/schemas/ggbn/terms/MaterialSample" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://data.ggbn.org/schemas/ggbn/terms/MaterialSample"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF MATERIALSAMPLE CLASS ------
#' R6 class representing a data structure for a GBIF MaterialSample augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFMaterialSample} class allows for the specification of data tables that comply with the MaterialSample
#' \url{http://data.ggbn.org/schemas/ggbn/terms/MaterialSample}{class specification} of GBIF.
GBIFMaterialSample <- R6Class("GBIFMaterialSample",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF MaterialSample class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF MaterialSample class ======
		#' @description
		#' Create a new GBIFMaterialSample object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFMaterialSampleMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMaterialSample
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMaterialSampleMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFMaterialSample} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMaterialSampleMembers]{getGBIFMaterialSampleMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFMaterialSampleTerm(),
				associatedTerms = getGBIFMaterialSampleMembers(),
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
#' Initialize a new GBIF MaterialSample object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFMaterialSampleMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFMaterialSample
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFMaterialSampleMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFMaterialSample} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFMaterialSampleMembers]{getGBIFMaterialSampleMembers}}
initializeGBIFMaterialSample <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFMaterialSample$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

