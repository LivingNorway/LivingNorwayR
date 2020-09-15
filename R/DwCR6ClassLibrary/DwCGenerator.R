# ------ 1. FUNCTION TO RETRIEVE DATA FRAME OF DARWIN CORE TERMS ------
#' Retrieve terms used by Darwin core
#'
#' @param includeExtensions A \code{logical} scalar that, if \code{TRUE}, instructs the fuction to also
#' include terms used in registered Darwin core extensions. If \code{FALSE}, only the terms specified by
#' the Darwin core standard is included
#' @param includeDeprecated A \code{logical} scalar that, if \code{TRUE}, instructs the function to also
#' include terms in the Dawin Core standard that are deprecated
#'
#' @return A \code{data.frame} containing one row per term. The output has the following columns:
#' \itemize{
#'     \item{termName}{The name of the term}
#'     \item{namespaceName}{The name of the namespace containing the term}
#'     \item{termIRI}{The URL of the term definition}
#'     \item{termVersionIRI}{The URL of the versioned term definition}
#'     \item{dateModified}{The date of the last modification of the term definition}
#'     \item{label}{A long version of the term name}
#'     \item{isReplacedBy}{For depracated terms this entry contains the URL of the definition of the replacing term}
#'     \item{definition}{The written definition of the usage of the term}
#'     \item{notes}{Notes on the use of the term}
#'     \item{type}{The data type of the term}
#'     \item{examples}{An example of data values that could be used for this term}
#'     \item{termInformationLN}{Supplementary information provided by Living Norway to guide usage of the term}
#'     \item{execCommitteeDecision}{A set of URLs containing decisions made about the term made by the relevant executive committee}
#'     \item{miscInformation}{Other information about the term}
#'     \item{termDef}{A label denoting under what standard the term is defined}
#' }
#'
#' @seealso \code{\link[retrieveDwCClassSpecifications]{retrieveDwCClassSpecifications}}
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#'
retrieveDwCTermSpecifications <- function(includeExtensions = TRUE, includeDeprecated = FALSE) {
  # TODO: add in some error handling if a conection to the server can't be made
  # ====== 1.1. Retrieve terms from Darwin core standard =====
  # Download terms defined by the Darwin Core standard
  termNodes <- xml_find_all(read_html("https://dwc.tdwg.org/list/"), "//h2[@id=\"4-vocabulary\"]/following-sibling::table")
  termList <- do.call(rbind, lapply(X = termNodes, FUN = function(curNode) {
    # Initialise an output list for the term entry
    outList <- data.frame(
      # Retrieve the name of the term as defined (and its namespace)
      termName = gsub("^.*\\:([\\w\\-]+)\\s*$", "\\1", xml_text(xml_find_first(curNode, ".//thead")),perl = TRUE),
      namespaceName = gsub("^.*\\s([\\w\\-]+)\\:[\\w\\-]+\\s*$", "\\1", xml_text(xml_find_first(curNode, ".//thead")), perl = TRUE),
      termIRI = "",
      termVersionIRI = "",
      dateModified = "",
      label = "",
      isReplacedBy = "",
      definition = "",
      notes = "",
      type = "",
      examples = "",
      termInformationLN = "",
      execCommitteeDecisions = "",
      miscInformation = "",
      termDef = "DwCStandard",
      stringsAsFactors = FALSE
    )
    # Look through the table of attributes for the term and populate the list
    for(infoNode in xml_find_all(curNode, ".//tbody/tr")) {
      colNodes <- xml_find_all(infoNode, ".//td")
      attrName <- xml_text(colNodes[1])
      attrVal <- xml_text(colNodes[2])
      colName <- switch(attrName,
                        "Term IRI" = "termIRI",
                        "Term version IRI" = "termVersionIRI",
                        "Modified" = "dateModified",
                        "Label" = "label",
                        "Is replaced by" = "isReplacedBy",
                        "Definition" = "definition",
                        "Notes" = "notes",
                        "Type" = "type",
                        "Examples" = "examples",
                        "Executive Committee decision" = "execCommitteeDecisions",
                        "miscInformation")
      outList[, colName] <- gsub("^\\|", "", paste(outList[, colName], attrVal, sep = "|"), perl = TRUE)
    }
    outList[1, ] <- ifelse(outList[1, ] == "", NA, outList[1, ])
    outList
  }))
  # ====== 1.2. Retrieve concepts from TDWG ======
  # Currently this isn't supported because we need to have access to the Biodiversity Information Standards database
  # on biological concepts. There are around 13000 concepts listed there (some of which are supported by GBIF) and
  # to support some of GBIF's registered extensions to Darwin Core we need an automated interface
  if(tryCatch(as.logical(includeExtensions)[1], error = function(err) {
    stop("error encountered during processing of extensions inclusion parameter: ", err)
  })) {
    # TODO: put processing of extension terms here
    warning("extension terms not currently supported")
  }
  rownames(termList) <- paste(termList$namespaceName, termList$termName, sep = ":")
  if(tryCatch(as.logical(includeDeprecated)[1] == FALSE, error = function(err) {
    stop("error encountered during processing of depracation inclusion parameter: ", err)
  })) {
    # If the terms list is to exclude depracted terms then remove them from the output
    termList <- termList[is.na(termList$isReplacedBy) & ifelse(is.na(termList$miscInformation), "", termList$miscInformation) != "This term is deprecated and should no longer be used.", ]
  }
  termList
}

# ------ 2. FUNCTION TO RETRIEVE LIST OF DARWIN CORE CLASSES ------
#' Retrieve classes and their related term used by Darwin core
#'
#' @param includeExtensions A \code{logical} scalar that, if \code{TRUE}, instructs the fuction to also
#' include classes used in registered Darwin core extensions. If \code{FALSE}, only the classes specified by
#' the Darwin core standard is included
#' @param includeDeprecated A \code{logical} scalar that, if \code{TRUE}, instructs the function to also
#' include terms in the Dawin Core standard that are depracated
#'
#' @return A \code{list} containing one element per class. Each element is itself a \code{list} with the
#' following named elements:
#' \itemize{
#'   \item{termName}{The name of the term}
#'   \item{namespaceName}{The name of the namespace containing the term}
#'   \item{termIRI}{The URL of the term definition}
#'   \item{termVersionIRI}{The URL of the versioned term definition}
#'   \item{dateModified}{The date of the last modification of the term definition}
#'   \item{label}{A long version of the term name}
#'   \item{isReplacedBy}{For depracated terms this entry contains the URL of the definition of the replacing term}
#'   \item{definition}{The written definition of the usage of the term}
#'   \item{notes}{Notes on the use of the term}
#'   \item{type}{The data type of the term}
#'   \item{examples}{An example of data values that could be used for this term}
#'   \item{termInformationLN}{Supplementary information provided by Living Norway to guide usage of the term}
#'   \item{execCommitteeDecision}{A set of URLs containing decisions made about the term made by the relevant executive committee}
#'   \item{miscInformation}{Other information about the term}
#'   \item{termDef}{A label denoting under what standard the term is defined}
#'   \item{compositeTerms}{A \code{data.frame} of terms related to the class. The data frame is formatted according to the
#'   format returned by \code{\link[retrieveDwCTermSpecifications]{retrieveDwCTermSpecifications}}}
#' }
#'
#' @seealso \code{\link[retrieveDwCTermSpecifications]{retrieveDwCTermSpecifications}}
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#'
retrieveDwCClassSpecifications <- function(includeExtensions = TRUE, includeDeprecated = FALSE) {
  # TODO: add in some error handling if a connection to the server can't be made
  # ====== 2.1. Retrieve terms from the Darwin core standard ======
  termFrame <- retrieveDwCTermSpecifications(includeExtensions, includeDeprecated)
  # ====== 2.2. Retrieve classes from the Darwin core standard ======
  classFrame <- termFrame[termFrame$type == "Class", ]
  # Download the landuage terms defined by the Darwin Core standard
  langNodes <- xml_find_all(read_html("https://dwc.tdwg.org/list/"), "//p[preceding-sibling::h3[@id=\"31-index-by-term-name\"] and following-sibling::h3[@id=\"32-index-by-label\"]]")
  apply(X = as.matrix(classFrame), FUN = function(curClassInfo, langNodes, termFrame) {
    # Initialise an output list
    outList <- list(
      termName = as.character(curClassInfo[1]),
      namespaceName = as.character(curClassInfo[2]),
      termIRI = as.character(curClassInfo[3]),
      termVersionIRI = as.character(curClassInfo[4]),
      dateModified = as.character(curClassInfo[5]),
      label = as.character(curClassInfo[6]),
      isReplacedBy = as.character(curClassInfo[7]),
      definition = as.character(curClassInfo[8]),
      notes = as.character(curClassInfo[9]),
      type = as.character(curClassInfo[10]),
      examples = as.character(curClassInfo[11]),
      termInformationLN = as.character(curClassInfo[12]),
      execCommitteeDecisions = as.character(curClassInfo[13]),
      miscInformation = as.character(curClassInfo[14]),
      termDef = as.character(curClassInfo[15]),
      compositeTerms = as.data.frame(matrix(as.character(c()), nrow = 0, ncol = 14, dimnames = list(NULL, colnames(termFrame))))
    )
    # Lookup the HTML node containing the specification for the class
    nodeIndex <- which(xml_text(langNodes) == outList$label)
    if(length(nodeIndex) > 0) {
      # Get the terms associated with the class
      termLabels <- strsplit(xml_text(langNodes)[nodeIndex[1] + 1], "\\s*\\|\\s*", perl = TRUE)[[1]]
      outList$compositeTerms = termFrame[termLabels, ]
    }
    outList
  }, MARGIN = 1, langNodes = langNodes, termFrame = termFrame)
}
