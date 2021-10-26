# ------ 1. GERMPLASMSAMPLE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF GermplasmSample class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFGermplasmSampleTerm <- function() {
	outValue <- NULL
	if("http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample"]]$termInfo
	} else if("http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample"]]$termInfo
	}
	outValue
}

# ------ 2. GERMPLASMSAMPLE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF GermplasmSample class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF GermplasmSample class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFGermplasmSampleMembers <- function() {
	outValue <- NULL
	if("http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample"]]$compositeTerms
	} else if("http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF GERMPLASMSAMPLE CLASS ------
#' R6 class representing a data structure for a GBIF GermplasmSample augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFGermplasmSample} class allows for the specification of data tables that comply with the GermplasmSample
#' \url{http://rs.nordgen.org/dwc/germplasm/0.1/terms/GermplasmSample}{class specification} of GBIF.
GBIFGermplasmSample <- R6Class("GBIFGermplasmSample",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF GermplasmSample class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF GermplasmSample class ======
		#' @description
		#' Create a new GBIFGermplasmSample object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFGermplasmSampleMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFGermplasmSample
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFGermplasmSampleMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFGermplasmSample} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFGermplasmSampleMembers]{getGBIFGermplasmSampleMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFGermplasmSampleTerm(),
				associatedTerms = getGBIFGermplasmSampleMembers(),
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
#' Initialize a new GBIF GermplasmSample object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFGermplasmSampleMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFGermplasmSample
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFGermplasmSampleMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFGermplasmSample} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFGermplasmSampleMembers]{getGBIFGermplasmSampleMembers}}
initializeGBIFGermplasmSample <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFGermplasmSample$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

