# ------ 1. A DWCTERM CLASS TO HOLD INFORMATION ON DARWIN CORE TERMS ------
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.

DwCTerm <- R6::R6Class("DwCTerm",
  # ====== 1.1. Define private members of the terms class ======
  private = list(
    termName = character(),
    namespaceName = character(),
    termIRI = character(),
    termVersionIRI = character(),
    dateModified = character(),
    label = character(),
    isReplacedBy = character(),
    definition = character(),
    notes = character(),
    type = character(),
    examples = character(),
    termInformationLN = character(),
    execCommitteeDecisions = character(),
    miscInformation = character(),
    termDef = character()
  ),
  public = list(
    # ====== 1.2. Initialise a DwCTerm object ======
    #' Initialise a DwCTerm object
    #'
    #' @param termName A \code{character} scalar containing the name of the term
    #' @param namespaceName A \code{character} scalar containing the name of the namespace containing the term
    #' @param termIRI A \code{character} scalar containing the Internationalized Resource Identifier (IRI) of the definition of the term
    #' @param termVersionIRI A \code{character} scalar containing the Internationalized Resource Identifier (IRI) of the definition of the current version of the term
    #' @param dateModified A \code{character} scalar containing date information of the last modification of the term definition
    #' @param label A \code{character} scalar containing a label for the term (used in casual language)
    #' @param isReplacedBy A \code{character} scalar containing the Internationalized Resource Identifier (IRI) of the definition of the term that replaced this term (used
    #' if the term is deprecated)
    #' @param definition A \code{character} scalar containing a brief description of the purpose of the term
    #' @param notes A \code{character} vector containing extra information about the usage of the term
    #' @param type A \code{character} scalar containing the type of term defined
    #' @param examples A \code{character} vector giving a set of example values for the term
    #' @param termInformationLN A \code{characer} vector giving supplementary information for the term provided by the \url{https://livingnorway.no/}{Living Norway} initiative
    #' @param execCommitteeDecisions A \code{character} vector of links to decisions made by executive committees on the usage of the term
    #' @param miscInformation A \code{character} vector providing supplementary information for the term
    #' @param termDef A \code{character} scalar providing information about the location of the body in charge of definition of the term
    #'
    #' @return A new DwCTerm object
    initialize = function(termName, namespaceName = character(), termIRI = character(), termVersionIRI = character(),
      dateModified = character(), label = character(), isReplacedBy = character(), definition = character(), notes = character(),
      type = character(), examples = character(), termInformationLN = character(), execCommitteeDecisions = character(),
      miscInformation = character(), termDef = character()) {
      # Utility function to ensure that character scalar have the correct formatting
      characterScalarTest <- function(paramText, inVar) {
        outVal <- tryCatch(as.character(inVar), error = function(err) {
          stop("error encountered whilst processing the ", paramText, " parameter: ", err)
        })
        if(length(outVal) > 2) {
          warning(paramText, " has length greater than one: only the first element will be used")
          outVal <- outVal[1]
        } else if(length(outVal) > 0) {
          if(is.na(outVal) || outVal == "") {
            outVal <- character()
          }
        }
        outVal
      }
      # Utility function to ensure that character vectors have the correct formatting
      characterVectorTest <- function(paramText, inVar) {
        outVal <- tryCatch(as.character(inVar), error = function(err) {
          stop("error encountered whilst processing the ", paramText, "parameter: ", err)
        })
        # Remove the empty strings from the output value
        if(length(outVal) >= 1) {
          outVal <- outVal[!is.na(outVal) & outVal != ""]
        }
        outVal
      }
      # Sanity check the term name
      private$termName <- characterScalarTest("term name", termName)
      if(length(private$termName) < 1) {
        stop("error encountered whilst processing the term name parameter: invalid value provided")
      }
      # Sanity check the namespace
      private$namespaceName <- characterScalarTest("namespace name", namespaceName)
      # Sanity check the term IRI
      private$termIRI <- characterScalarTest("term IRI", termIRI)
      # Sanity check the term version IRI
      private$termVersionIRI <- characterScalarTest("term version IRI", termVersionIRI)
      # Sanity check the date modified
      private$dateModified <- characterScalarTest("date modified", dateModified)
      # Sanity check the label
      private$label <- characterScalarTest("label", label)
      # Sanity check the is replaced by
      private$isReplacedBy <- characterScalarTest("\"is replaced by\"", isReplacedBy)
      # Sanity check the definition
      private$definition <- characterScalarTest("definition", definition)
      # Sanity check the notes
      private$notes <- characterVectorTest("notes", notes)
      # Sanity check the type
      private$type <- characterScalarTest("type", type)
      # Sanity check the examples
      private$examples <- characterVectorTest("examples", examples)
      # Sanity check the Living Norway term information
      private$termInformationLN <- characterVectorTest("Living Norway term information", termInformationLN)
      # Sanity check the executive dicisions
      private$execCommitteeDecisions <- characterVectorTest("executive committee decisiosn", execCommitteeDecisions)
      # Sanity check the miscellaneous information
      private$miscInformation <- characterVectorTest("miscellaneous information", miscInformation)
      # Sanity check the term definition source
      private$termDef <- characterScalarTest("term definition source", termDef)
      # Return the object invisibly
      invisible(self)
    },
    # ====== 1.3. Retrieve the qualified name of the term ======
    #' Retrieve the qualified name of the term (the term name with the associated namespace)
    #'
    #' @return The qualified name of the term
    #'
    getQualifiedName = function() {
      outVal <- private$termName
      if(length(private$namespaceName) > 0)
      { # Include the namespace informtion if it exists
        outVal <- paste(private$namespaceName, private$termName, sep = ":")
      }
      outVal
    },
    # ====== 1.4. Assess whether the term is deprecated ======
    #' Return the deprecation status of the term
    #'
    #' @return A \code{logical} scalar that is \code{TRUE} if the term is deprecated and
    #' \code{FALSE} otherwise
    #'
    isDeprecated = function() {
      outVal <- FALSE
      if(length(private$isReplacedBy) > 0) {
        outVal <- TRUE
      } else if(length(private$miscInformation) > 0) {
        outVal <- any(grepl("This term is deprecated and should no longer be used", private$miscInformation, fixed = TRUE))
      }
      outVal
    },
    # ====== 1.5. Override print function for the term ======
    #' Print the term information
    #'
    print = function(...) {
      cat(self$getQualifiedName())
      if(length(private$label) > 0) {
        cat(" -", private$label)
      }
      if(self$isDeprecated()) {
        cat(" (DEPRECATED)")
      }
      cat("\n")
      if(length(private$definition) > 0) {
        cat(private$definition, "\n", sep = "")
      }
      cat("\n")
      if(length(private$termDef) > 0) {
        cat("\tDefined in: ", private$termDef, "\n", sep = "")
      }
      if(length(private$termIRI) > 0) {
        cat("\tIRI: ", private$termIRI, "\n", sep = "")
      }
      if(length(private$termVersionIRI) > 0) {
        cat("\tVersion IRI: ", private$termVersionIRI, "\n", sep = "")
      }
      if(length(private$type) > 0) {
        cat("\tType: ", private$type, "\n", sep = "")
      }
      if(length(private$dateModified) > 0) {
        cat("\tDate modified: ", private$dateModified, "\n", sep = "")
      }
      if(length(private$isReplacedBy) > 0) {
        cat("\tIs replaced by: ", private$isReplacedBy, "\n", sep = "")
      }
      if(length(private$notes) > 0) {
        cat("\tNotes:\n", paste("\t\t", private$notes, sep = "", collapse = "\n"), "\n", sep = "")
      }
      if(length(private$execCommitteeDecisions) > 0) {
        cat("\tExecutive committee decisions:\n", paste("\t\t", private$execCommitteeDecisions, sep = "", collapse = "\n"), "\n", sep = "")
      }
      if(length(private$examples) > 0) {
        cat("\tExamples:\n", paste("\t\t", private$examples, sep = "", collapse = "\n"), "\n", sep = "")
      }
      if(length(private$miscInformation) > 0) {
        cat("\tMiscellaneous information:\n", paste("\t\t", private$miscInformation, sep = "", collapse = "\n"), "\n", sep = "")
      }
      if(length(private$termInformationLN) > 0) {
        cat("\tLiving Norway supplementary information:\n", paste("\t\t", private$termInformationLN, sep = "", collapse = "\n"), "\n", sep = "")
      }
      invisible(self)
    },
    # ====== 1.6. Retrieve the term name ======
    #' Retrieve the term name
    #'
    #' @return A \code{character} scalar containing the term name
    #'
    getTermName = function() {
      private$termName
    },
    # ====== 1.7. Retrieve the namespace name ======
    #' Retrieve the namespace name
    #'
    #' @return A \code{character} scalar containing the namespace name
    #'
    getNamespaceName = function() {
      private$namespaceName
    },
    # ====== 1.8. Retrieve the term IRI ======
    #' Retrieve the term IRI
    #'
    #' @return A \code{character} scalar containing the term IRI
    #'
    getTermIRI = function() {
      private$termIRI
    },
    # ====== 1.9. Retrieve the term version IRI ======
    #' Retrieve the term version IRI
    #'
    #' @return A \code{character} scalar containing the term version IRI
    #'
    getTermVersionIRI = function() {
      private$termVersionIRI
    },
    # ====== 1.10. Retrieve the modification date ======
    #' Retrieve the modification date
    #'
    #' @return A \code{character} scalar containing the modification date
    #'
    getDateModified = function() {
      private$dateModified
    },
    # ====== 1.11. Retrieve the term label ======
    #' Retrieve the label
    #'
    #' @return A \code{character} scalar containing the label
    #'
    getLabel = function() {
      private$label
    },
    # ====== 1.12. Retrieve the replacement IRI ======
    #' Retrieve the replacement IRI
    #'
    #' @return A \code{character} scalar containing the IRI of the replacement term
    #'
    getIsReplacedBy = function() {
      private$isReplacedBy
    },
    # ====== 1.13. Retrieve the definition ======
    #' Retrieve the definition
    #'
    #' @return A \code{character} scalar containing the term definition
    #'
    getDefinition = function() {
      private$definition
    },
    # ====== 1.14. Retrieve the term notes ======
    #' Retrieve the term notes
    #'
    #' @return A \code{character} vector containing the term notes
    #'
    getNotes = function() {
      private$notes
    },
    # ====== 1.15. Retrieve the type of the term ======
    #' Retrieve the type of the term
    #'
    #' @return A \code{character} scalar containing the term type
    getType = function() {
      private$type
    },
    # ====== 1.16. Retrieve examples of the use of the term ======
    #' Retrieve examples of the use of the term
    #'
    #' @return A \code{character} vector containing examples of use of the term
    #'
    getExamples = function() {
      self@examples
    },
    # ====== 1.17. Retrieve Living Norway supplementary information ======
    #' Retrieve supplementary information about the term provided by Living Norway
    #'
    #' @return A \code{character} vector containing the Living Norway supplementary information
    #'
    getTermInformationLN = function() {
      self@termInformationLN
    },
    # ====== 1.18. Retrieve executive committee decisions ======
    #' Retrieve executive committee decisions about the use of the term
    #'
    #' @return A \code{character} vector containing the executive committee decisions
    #'
    getExecCommitteeDecisions = function() {
      self@execCommitteeDecisions
    },
    # ====== 1.19. Retrieve miscellaneous information ======
    #' Retrieve miscellaneous information on the usage of the term
    #'
    #' @return A \code{character} vector containing the miscellaneous information
    #'
    getMiscInformation = function() {
      self@miscInformation
    },
    # ====== 1.20. Retrieve the source of the term definition ======
    #' Retrieve the source of the term definition
    #'
    #' @return A \code{character} scalar containing the source of the term definition
    #'
    getTermDef = function() {
      self@termDef
    }
  )
)

# ------ 2. FUNCTION TO RETRIEVE DATA FRAME OF DARWIN CORE TERMS ------
#' Retrieve terms used by Darwin core
#'
#' @param includeExtensions A \code{logical} scalar that, if \code{TRUE}, instructs the fuction to also
#' include terms used in registered Darwin core extensions. If \code{FALSE}, only the terms specified by
#' the Darwin core standard is included
#' @param includeDeprecated A \code{logical} scalar that, if \code{TRUE}, instructs the function to also
#' include terms in the Dawin Core standard that are deprecated
#'
#' @return A \code{list} of \code{DwCTerm} objects representing the terms defined by the Darwin core
#' standard
#'
#' @seealso \code{\link[retrieveDwCClassSpecifications]{retrieveDwCClassSpecifications}}
#' \code{\link[DwCTerm]{DwCTerm}}
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#'
retrieveDwCTermSpecifications <- function(includeExtensions = TRUE, includeDeprecated = FALSE) {
  # TODO: add in some error handling if a conection to the server can't be made
  # ====== 2.1. Retrieve terms from Darwin core standard =====
  # Download terms defined by the Darwin Core standard
  termNodes <- xml_find_all(read_html("https://dwc.tdwg.org/list/"), "//h2[@id=\"4-vocabulary\"]/following-sibling::table")
  termList <- lapply(X = termNodes, FUN = function(curNode) {
    # Initialise an output data.frame for the term entry
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
      termDef = "https://dwc.tdwg.org/",
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
      delimToUse <- "|||"
      if(outList[, colName] == "") {
        outList[, colName] <- attrVal
      } else {
        outList[, colName] <- paste(outList[, colName], attrVal, sep = delimToUse)
      }
    }
    outList[1, ] <- ifelse(outList[1, ] == "", NA, outList[1, ])
    DwCTerm$new(termName = outList$termName, namespaceName = outList$namespaceName, termIRI = outList$termIRI, termVersionIRI = outList$termVersionIRI,
      dateModified = outList$dateModified, label = outList$label, isReplacedBy = outList$isReplacedBy, definition = outList$definition, notes = strsplit(outList$notes, delimToUse, fixed = TRUE)[[1]],
      type = outList$type, examples = strsplit(outList$examples, delimToUse, fixed = TRUE)[[1]], termInformationLN = strsplit(outList$termInformationLN, delimToUse, fixed = TRUE)[[1]],
      execCommitteeDecisions = strsplit(outList$execCommitteeDecisions, delimToUse, fixed = TRUE)[[1]], miscInformation = strsplit(outList$miscInformation, delimToUse, fixed = TRUE)[[1]],
      termDef = outList$termDef)
  })
  # ====== 2.2. Retrieve concepts from TDWG ======
  # Currently this isn't supported because we need to have access to the Biodiversity Information Standards database
  # on biological concepts. There are around 13000 concepts listed there (some of which are supported by GBIF) and
  # to support some of GBIF's registered extensions to Darwin Core we need an automated interface
  if(tryCatch(as.logical(includeExtensions)[1], error = function(err) {
    stop("error encountered during processing of extensions inclusion parameter: ", err)
  })) {
    # TODO: put processing of extension terms here
    warning("extension terms not currently supported")
  }
  # ====== 2.3. Process the outputs ======
  # Use qualified names to index the list
  names(termList) <- sapply(X = termList, FUN = function(curOb) {
    curOb$getQualifiedName()
  })
  if(tryCatch(as.logical(includeDeprecated)[1] == FALSE, error = function(err) {
    stop("error encountered during processing of depracation inclusion parameter: ", err)
  })) {
    # If the terms list is to exclude depracted terms then remove them from the output
    termList <- termList[!sapply(X = termList, FUN = function(curOb) { curOb$isDeprecated() })]
    # termList <- termList[is.na(termList$isReplacedBy) & ifelse(is.na(termList$miscInformation), "", termList$miscInformation) != "This term is deprecated and should no longer be used.", ]
  }
  termList
}

# ------ 3. FUNCTION TO RETRIEVE LIST OF DARWIN CORE CLASSES ------
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
#'   \item{termInfo}{A \code{DwCTerm} object containing the information of the class term}
#'   \item{compositeTerms}{A \code{list} of \code{DwCTerm} objects for each term that is associated with
#'   the class}
#' }
#'
#' @seealso \code{\link[retrieveDwCTermSpecifications]{retrieveDwCTermSpecifications}}
#' \code{\link[DwCTerm]{DwCTerm}}
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}
#'
retrieveDwCClassSpecifications <- function(includeExtensions = TRUE, includeDeprecated = FALSE) {
  # TODO: add in some error handling if a connection to the server can't be made
  # ====== 3.1. Retrieve terms from the Darwin core standard ======
  termList <- retrieveDwCTermSpecifications(includeExtensions, includeDeprecated)
  # ====== 3.2. Retrieve classes from the Darwin core standard ======
  classList <- termList[sapply(X = termList, FUN = function(curOb) { curOb$getType() == "Class" })]
  # Download the landuage terms defined by the Darwin Core standard
  langNodes <- xml_find_all(read_html("https://dwc.tdwg.org/list/"), "//p[preceding-sibling::h3[@id=\"31-index-by-term-name\"] and following-sibling::h3[@id=\"32-index-by-label\"]]")
  setNames(lapply(X = classList, FUN = function(curClassInfo, langNodes, termList) {
    # Initialise an output list
    outList <- list(
      termInfo = curClassInfo,
      compositeTerms = list()
    )
    # Lookup the HTML node containing the specification for the class
    nodeIndex <- which(xml_text(langNodes) == outList$termInfo$getLabel())
    if(length(nodeIndex) > 0) {
      # Get the terms associated with the class
      termLabels <- strsplit(xml_text(langNodes)[nodeIndex[1] + 1], "\\s*\\|\\s*", perl = TRUE)[[1]]
      termLabels <- termLabels[termLabels %in% names(termList)]
      outList$compositeTerms <- termList[termLabels]
    }
    outList
  }, langNodes = langNodes, termList = termList), names(classList))
}
