# ------ 1. TYPESANDSPECIMEN TERM INFORMATION ------
#' Types and specimen term information
#' @description Return the information of the term associated with the GBIF TypesAndSpecimen class
#' @return A \code{DwCTerm} object containing the term information
#' @seealso \code{\link[DwCTerm]{DwCTerm}}
getGBIFTypesAndSpecimenTerm <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/TypesAndSpecimen" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/TypesAndSpecimen"]]$termInfo
	} else if("http://rs.gbif.org/terms/1.0/TypesAndSpecimen" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/TypesAndSpecimen"]]$termInfo
	}
	outValue
}

# ------ 2. TYPESANDSPECIMEN MEMBER INFORMATION ------
#' Types and specimen member information
#' @description Return a list of properties associated with the GBIF TypesAndSpecimen class
#' @return A \code{list} of \code{DwCTerm} objects, one for each member of the the
#' GBIF TypesAndSpecimen class
#' @seealso \code{\link[DwCTerm]{DwCTerm}}

getGBIFTypesAndSpecimenMembers <- function() {
	outValue <- NULL
	if("http://rs.gbif.org/terms/1.0/TypesAndSpecimen" %in% names(GBIFCoreClassList)) {
		outValue <- GBIFCoreClassList[["http://rs.gbif.org/terms/1.0/TypesAndSpecimen"]]$compositeTerms
	} else if("http://rs.gbif.org/terms/1.0/TypesAndSpecimen" %in% names(GBIFExtClassList)) {
		outValue <- GBIFExtClassList[["http://rs.gbif.org/terms/1.0/TypesAndSpecimen"]]$compositeTerms
	}
	outValue
}

# ------ 3. GBIF TYPESANDSPECIMEN CLASS ------
#' GBIF Types and specimen R6 Class
#' @description R6 class representing a data structure for a GBIF TypesAndSpecimen augmented data table (based on the Dawrin core)
#'
#' The \code{GBIFTypesAndSpecimen} class allows for the specification of data tables that comply with the TypesAndSpecimen
#' \url{http://rs.gbif.org/terms/1.0/TypesAndSpecimen}{class specification} of GBIF.
#'
GBIFTypesAndSpecimen <- R6Class("GBIFTypesAndSpecimen",
	inherit = DwCGeneric,
	# ====== 3.1. Define private members of the GBIF TypesAndSpecimen class ======
	private = list(
	),
	public = list(
		# ====== 3.2. Define an initialization function for the GBIF TypesAndSpecimen class ======
		#' Initialisation function for types and specimens
		#' @description Create a new GBIFTypesAndSpecimen object
		#' @details Define an initialization function for the GBIF TypesAndSpecimen class
		#' @param objectData A \code{data.frame} containing the data to import into the object
		#' @param idColumnInfo Either a \code{character} scalar containing the column name of
		#' \code{objectData} or an \code{integer} scalar giving the index of the column of
		#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
		#' may be the qualified name of the Darwin core term for which the appropriately mapped column
		#' will be used as the ID variable (the possible Darwin core term names can be found by running
		#' \code{names(getGBIFTypesAndSpecimenMembers())})
		#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
		#' to their respective Darwin core terms based on the column names
		#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
		#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
		#' for this string.
		#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFTypesAndSpecimen
		#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
		#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
		#' term. Mappable terms can be found using: \code{sapply(X = getGBIFTypesAndSpecimenMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
		#' @return A new \code{GBIFTypesAndSpecimen} object
		#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFTypesAndSpecimenMembers]{getGBIFTypesAndSpecimenMembers}}
		initialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
			super$initialize(
				classTermInfo = getGBIFTypesAndSpecimenTerm(),
				associatedTerms = getGBIFTypesAndSpecimenMembers(),
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
#' Initialize a new GBIF TypesAndSpecimen object
#' @description Initializes a new GBIF TypesAndSpecimen object
#' @details Initialisation function for GBIFTypesAndSpecimen
#' @param objectData A \code{data.frame} containing the data to import into the object
#' @param idColumnInfo Either a \code{character} scalar containing the column name of
#' \code{objectData} or an \code{integer} scalar giving the index of the column of
#' \code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
#' may be the qualified name of the Darwin core term for which the appropriately mapped column
#' will be used as the ID variable (the possible Darwin core term names can be found by running
#' \code{names(getGBIFTypesAndSpecimenMembers())})
#' @param nameAutoMap A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
#' to their respective Darwin core terms based on the column names
#' @param defDateFormat A \code{character} scalar providing the default format for strings denoting dates in the
#' data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
#' for this string.
#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIFTypesAndSpecimen
#' class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
#' or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
#' term. Mappable terms can be found using: \code{sapply(X = getGBIFTypesAndSpecimenMembers(), FUN = function(curTerm) { curTerm$getTermName() })}
#' @return A new \code{GBIFTypesAndSpecimen} object
#' @seealso \code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFTypesAndSpecimenMembers]{getGBIFTypesAndSpecimenMembers}}
initializeGBIFTypesAndSpecimen <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = "YYYY-MM-DD", ...) {
	GBIFTypesAndSpecimen$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)
}

