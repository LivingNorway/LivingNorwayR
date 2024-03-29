# ------ 1. EVENT TERM INFORMATION ------
#' Return the information of the term associated with the GBIF Event class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFEventTerm <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/Event" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Event"]]$termInfo
	} else if("http://rs.tdwg.org/dwc/terms/Event" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/Event"]]$termInfo
	}
	outValue
}

# ------ 2. EVENT MEMBER INFORMATION ------
#' Return a list of properties associated with the GBIF Event class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF Event class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
#' @export
getGBIFEventMembers <- function() {
	outValue <- NULL
	if("http://rs.tdwg.org/dwc/terms/Event" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Event"]]$compositeTerms
	} else if("http://rs.tdwg.org/dwc/terms/Event" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/Event"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF EVENT CLASS ------
#' R6 class representing a data structure for a GBIF Event augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFEvent} class allows for the specification of data tables that comply with the Event
#' \url{http://rs.tdwg.org/dwc/terms/Event}{class specification} of GBIF.
GBIFEvent <- R6Class("GBIFEvent",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF Event class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF Event class ======
		#' @description
		#' Create a new GBIFEvent object
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFEventMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFEvent
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFEventMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFEvent} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFEventMembers]{getGBIFEventMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFEventTerm(),
				associatedTerms = getGBIFEventMembers(),
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
#' Initialize a new GBIF Event object
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFEventMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFEvent
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFEventMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFEvent} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFEventMembers]{getGBIFEventMembers}}
#'@examples
#'\dontrun{
#'## Get a dataset as an Archive and then extract the event core
#'## Get the dataset using the key:
#'datasetKey <- "aea17af8-5578-4b04-b5d3-7adf0c5a1e60"
#'Archive <-getLNportalData(datasetKey = datasetKey)
#'event<-Archive$getCoreTable()
#'## Convert the event back to a dataframe
#'event=event$exportAsDataFrame()
#'## Then use the dataframe to initialise an object of class GBIF event
#'GBIFevent<-initializeGBIFEvent(event, idColumnInfo = "id", nameAutoMap = TRUE)
#'}
#' @export
initializeGBIFEvent <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFEvent$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

