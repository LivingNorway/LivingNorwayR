# ------ 1. OCCURRENCE TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Occurrence class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFOccurrenceTerm <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/Occurrence" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Occurrence"]]$termInfo
	} else if("http://rs.tdwg.org/dwc/terms/Occurrence" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/Occurrence"]]$termInfo
	}
	outValue
}

# ------ 2. OCCURRENCE MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Occurrence class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Occurrence class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
#' @export
getGBIFOccurrenceMembers <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/Occurrence" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Occurrence"]]$compositeTerms
	} else if("http://rs.tdwg.org/dwc/terms/Occurrence" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/Occurrence"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF OCCURRENCE CLASS ------
#' R6 class representing a data structure for a GBIF Occurrence augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFOccurrence} class allows for the specification of data tables that comply with the Occurrence
#' \url{http://rs.tdwg.org/dwc/terms/Occurrence}{class specification} of GBIF.
GBIFOccurrence <- R6Class("GBIFOccurrence",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Occurrence class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Occurrence class ======
		#' @description
		#' Create a new GBIFOccurrence object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFOccurrenceMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFOccurrence
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFOccurrenceMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFOccurrence} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFOccurrenceMembers]{getGBIFOccurrenceMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFOccurrenceTerm(),
				associatedTerms = getGBIFOccurrenceMembers(),
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
#' Initialize a new GBIF Occurrence object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFOccurrenceMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFOccurrence
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFOccurrenceMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFOccurrence} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFOccurrenceMembers]{getGBIFOccurrenceMembers}}
#'@examples
#'\dontrun{
#'## Get a dataset as an Archive and then extract the occurrence file
#'#'## Get the dataset using the key:
#'datasetKey <- "346a9b13-5c96-4793-bcd7-d6614950e726"
#'Archive <-getLNportalData(datasetKey = datasetKey)
#'core<-Archive$getCoreTable()
#'core=core$exportAsDataFrame()
#'GBIFOcc=initializeGBIFOccurrence(core, "id", TRUE)
#'}
#' @export
initializeGBIFOccurrence <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFOccurrence$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

