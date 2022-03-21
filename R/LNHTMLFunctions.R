# ------ 1. INTERNAL TAG CREATION FUNCTIONS ------

# ====== 1.1. Generate tag IDs ======
#' Check tag ID
#' Check values given as IDs for EML tag. It the entry is NA then a unique ID is generated
#' @param tagID A \code{character} scalar for with the ID code
#' @return A \code{character} scalar containing the processed ID code
#'
#' @seealso \code{\link[uuid::UUIDgenerate]{UUIDgenerate}}
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @keyword internal
#'
generateTagID <- function(tagID) {
  outArg <- tryCatch(as.character(tagID), error = function(err) {
    stop("error encountered whilst processing the tag ID: ", err)
  })
  if(length(outArg) <= 0) {
    outArg <- NA
  } else if(length(outArg) > 1) {
    warning("tagID has a length greater than 1: only using the first element")
    outArg <- outArg[1]
  }
  if(is.na(outArg) || outArg == "") {
    outArg <- NA
  } else if(grepl("\\s+", outArg, perl = TRUE)) {
    stop("error encountered whilst processing tag ID: whitespace is present")
  }
  if(is.na(outArg)) {
    # If the tag ID is NA then generate a unique tag code from the UUID package
    outArg <- uuid::UUIDgenerate()
  }
  outArg
}

# ====== 1,2, Retrieve the EML schema ======
#' Retrieve an EML schema and convert it into an R list that can be used for
#' creation of metadata functions
#' @param schema A \code{character} scalar with the location of the EML schema to import
#' @return A \code{list} containing the schema elements
#'
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @keyword internal
#'
retrieveEMLSchema <- function(schema) {
  inSchema <- tryCatch(as.character(schema), error = function(err) {
    stop("error encountered whilst processing the EML schema: ", err)
  })
  if(length(inSchema) <= 0) {
    inSchema <- NA
  }
  sapply(X = inSchema, FUN = function(curSchema) {
    # Import the XML
    inXML <- xml2::read_xml(curSchema, encoding = "UTF-8")
  })
}
