# ------ 1. A DWCTERM CLASS TO HOLD INFORMATION ON DARWIN CORE TERMS ------
#' DWCTerm object
#' @description R6 class to hold information on DWC terms
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
    termDef = character(),
    vocabularyURI = character()
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
    #' @param vocabularyURI A \code{character} scalar containing the Unified Resource Identifier (URI) for a vocabulary that the possible values for this term are based on
    #'
    #' @return A new DwCTerm object
    initialize = function(termName, namespaceName = character(), termIRI = character(), termVersionIRI = character(),
      dateModified = character(), label = character(), isReplacedBy = character(), definition = character(), notes = character(),
      type = character(), examples = character(), termInformationLN = character(), execCommitteeDecisions = character(),
      miscInformation = character(), termDef = character(), vocabularyURI = character()) {
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
      if(length(private$namespaceName) > 0) {
        if(!grepl("/$", private$namespaceName, perl = TRUE)) {
          private$namespaceName <- paste(private$namespaceName, "/", sep = "")
        }
      }
      # Sanity check the term IRI
      private$termIRI <- characterScalarTest("term IRI", termIRI)
      if(length(private$termIRI) < 0) {
        private$termIRI <- self$getQualifiedName()
      }
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
      # Sanity check the vocabulary term
      private$vocabularyURI <- characterScalarTest("vocabulary URI", vocabularyURI)
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
        outVal <- paste(private$namespaceName, private$termName, sep = "")
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
      deprTextSearch <- function(inText) {
        any(
          grepl("This term is deprecated and should no longer be used", inText, fixed = TRUE) |
          grepl("This extension has been DEPRECATED", inText, fixed = TRUE)
        )
      }
      outVal <- FALSE
      if(length(private$isReplacedBy) > 0) {
        outVal <- TRUE
      }
      if(length(private$miscInformation) > 0 && !outVal) {
        outVal <- deprTextSearch(private$miscInformation)
      }
      if(length(private$notes) > 0 && !outVal) {
        outVal <- deprTextSearch(private$miscInformation)
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
      if(length(private$vocabularyURI) > 0) {
        cat("\tVocabulary URI:\n", paste("\t\t", private$vocabularyURI, sep = "", collapse = "\n"), "\n", sep = "")
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
      self$examples
    },
    # ====== 1.17. Retrieve Living Norway supplementary information ======
    #' Retrieve supplementary information about the term provided by Living Norway
    #'
    #' @return A \code{character} vector containing the Living Norway supplementary information
    #'
    getTermInformationLN = function() {
      self$termInformationLN
    },
    # ====== 1.18. Retrieve executive committee decisions ======
    #' Retrieve executive committee decisions about the use of the term
    #'
    #' @return A \code{character} vector containing the executive committee decisions
    #'
    getExecCommitteeDecisions = function() {
      self$execCommitteeDecisions
    },
    # ====== 1.19. Retrieve miscellaneous information ======
    #' Retrieve miscellaneous information on the usage of the term
    #'
    #' @return A \code{character} vector containing the miscellaneous information
    #'
    getMiscInformation = function() {
      self$miscInformation
    },
    # ====== 1.20. Retrieve the source of the term definition ======
    #' Retrieve the source of the term definition
    #'
    #' @return A \code{character} scalar containing the source of the term definition
    #'
    getTermDef = function() {
      self$termDef
    },
    # ===== 1.21. Retrieve the URI of the vocabulary definition ======
    #' Retrieve the vocabulary URI
    #'
    #' @return A \code{character} scalar containing the URI where the vocabulary for the
    #' term is described
    #'
    getVocabularyURI = function() {
      self$vocabularyURI
    }
  )
)

# ------ 2. FUNCTION TO RETRIEVE DATA FRAME OF DARWIN CORE TERMS ------
#' Retrieve terms used by Darwin core
#'
#' @param includeExtensions A \code{logical} scalar that, if \code{TRUE}, instructs the fuction to also
#' include terms used in registered GBIF extensions to Darwin Core. If \code{FALSE}, only the terms specified by
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
      namespaceName = switch(
        gsub("^.*\\s([\\w\\-]+)\\:[\\w\\-]+\\s*$", "\\1", xml_text(xml_find_first(curNode, ".//thead")), perl = TRUE),
        "dwc" = "http://rs.tdwg.org/dwc/terms/",
        "dwciri" = "http://rs.tdwg.org/dwc/iri/",
        "dc" = "http://purl.org/dc/elements/1.1/",
        "dcterms" = "http://purl.org/dc/terms/",
        ""
      ),
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
      vocabularyURI = "",
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
      termDef = outList$termDef, vocabularyURI = outList$vocabularyURI)
  })
  # ====== 2.2. Retrieve GBIF extensions terms ======
  # If requested, download terms defined in the GBIF registered extensions
  if(tryCatch(as.logical(includeExtensions)[1], error = function(err) {
    stop("error encountered during processing of extensions inclusion parameter: ", err)
  })) {
    # Function to read all the terms specified at a given URL
    retrieveXMLSpecs <- function(specAddress) {
      # Function to read the GBIF XML file
      readGBIFXML <- function(specAddress) {
        curDoc <- read_xml(specAddress)
        # Class name
        className <- switch(xml_attr(curDoc, "rowType"),
          "http://data.ggbn.org/schemas/ggbn/terms/Cloning" = "Cloning",   # Cloning class has the 'Amplification' name.  Manually need to over-ride that here
          xml_attr(curDoc, "name"))
        # Convert each of the child nodes of the xml specification to terms objects
        append(lapply(X = xml_children(curDoc), FUN = function(curNode, curDoc) {
          DwCTerm$new(
            termName = xml_attr(curNode, "name"),
            namespaceName = xml_attr(curNode, "namespace"),
            termIRI = xml_attr(curNode, "rowType"),
            termVersionIRI = xml_attr(curNode, "rowType"),
            dateModified = xml_attr(curDoc, "issued"),
            label = xml_attr(curNode, "name"),
            isReplacedBy = "",
            definition = xml_attr(curNode, "description"),
            notes = xml_attr(curNode, "description"),
            type = xml_name(curNode),
            examples = xml_attr(curNode, "examples"),
            termInformationLN = "",
            execCommitteeDecisions = "",
            miscInformation = paste("GBIF sub-class designation: ", ifelse(is.na(xml_attr(curNode, "group")), "unknown", xml_attr(curNode, "group")), sep = ""),
            vocabularyURI = xml_attr(curNode, "thesaurus")
          )
        }, curDoc = curDoc), list(
          # Convert the countaining class to a a terms object
          DwCTerm$new(
            termName = className,
            namespaceName = xml_attr(curDoc, "namespace"),
            termIRI = xml_attr(curDoc, "rowType"),
            termVersionIRI = xml_attr(curDoc, "rowType"),
            dateModified = xml_attr(curDoc, "issued"),
            label = className,
            isReplacedBy = "",
            definition = xml_attr(curDoc, "description"),
            notes = xml_attr(curDoc, "description"),
            type = "class",
            examples = "",
            termInformationLN = "",
            miscInformation = paste("GBIF core/extension class"),
            vocabularyURI = xml_attr(curDoc, "relation")
          )
        ))
      }
      # Retrieve the link nodes for all the entries in the specification page
      if(!grepl("/$", specAddress, perl = TRUE)) {
        specAddress <- paste(specAddress, "/", sep = "")
      }
      aNodes <- xml_find_all(read_html(specAddress), "//td/a")
      aLinks <- sapply(X = aNodes[2:length(aNodes)], FUN = xml_attr, attr = "href")
      # Go through each of the links and process each entry
      unlist(lapply(X = aLinks, FUN = function(curLink, curBaseAddress) {
        outVals <- list()
        if(grepl("\\.xml$", curLink, perl = TRUE)) {
          # If the link is to a XML file then scrape the property information from it
          outVals <- readGBIFXML(paste(curBaseAddress, curLink, sep = ""))
        } else if(grepl("/$", curLink, perl = TRUE)) {
          # If the link is to another folder then call the function recursively
          outVals <- retrieveXMLSpecs(paste(curBaseAddress, curLink, sep = ""))
        }
        outVals
      }, curBaseAddress = specAddress))
    }
    # Retrieve the term specifications for both the core and extension elements
    GBIFSpecs <- append(
      retrieveXMLSpecs("https://rs.gbif.org/core/"),
      retrieveXMLSpecs("https://rs.gbif.org/extension/")
    )
    # Remove those entries in the GBIF specifications that are already in the Darwin Core specification and append those to in the
    # Darwin core specification
    termList <- append(termList, GBIFSpecs[sapply(X = GBIFSpecs, FUN = function(curSpec, termList) {
      !any(curSpec$getQualifiedName() == sapply(X = termList, FUN = function(curTerm) { curTerm$getQualifiedName() }))
    }, termList = termList)])
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
  }
  termList
}

# ------ 3. FUNCTION TO RETRIEVE LIST OF DARWIN CORE CLASSES ------
#' Retrieve classes and their related terms used by Darwin core
#'
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
retrieveDwCClassSpecifications <- function(includeDeprecated = FALSE) {
  # TODO: add in some error handling if a connection to the server can't be made
  # ====== 3.1. Retrieve terms from the Darwin core standard ======
  termList <- retrieveDwCTermSpecifications(FALSE, includeDeprecated)
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

# ------ 4. FUNCTION TO RETRIEVE LIST OF GBIF REGISTERED CLASSES ------
#' Retrieve classes and their related terms used by GBIF
#'
#' @param classOption A \code{character} scalar that if set to \code{"core"} returns only the classes and the associated terms of GBIF's accepted core types.
#' If set to \code{"extension"} returns only the classes and associated terms of GBIF's \url{https://tools.gbif.org/dwca-validator/extensions.do}{registered extensions}.
#' \code{"all"} (the default) returns all of GBIF's registered class types.
#' @param includeDeprecated A \code{logical} scalar that, if \code{TRUE}, instructs the function to also include terms in the GBIF classes that are depracated
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
#' @export
retrieveGBIFClassSpecifications <- function(classOption = "all", includeDeprecated = FALSE) {
  # ====== 4.1. Retrieve terms from the GBIF list of used terms ======
  termList <- retrieveDwCTermSpecifications(TRUE, includeDeprecated)
  # ====== 4.2. Assign terms to their respective GBIF classes ======
  # Sanity test the class option input
  inClassOption <- tryCatch(tolower(as.character(classOption)), error = function(err) {
    stop("error encountered processing the class option parameter: ", err)
  })
  if(length(inClassOption) > 1) {
    warning("class option parameter length greater than one: only the first element will be used")
    inClassOption <- inClassOption[1]
  } else if(length(inClassOption) == 0) {
    stop("error encountered processing the class option parameter: parameter has length 0")
  }
  # The URLs to check the class structure from
  urlCheck <- c("https://rs.gbif.org/core/", "https://rs.gbif.org/extension/")
  if(inClassOption != "all") {
    if(inClassOption == "core") {
      urlCheck <- urlCheck[1]
    } else if(inClassOption == "extension") {
      urlCheck <- urlCheck[2]
    } else {
      stop("error encountered processing the class option parameter: values must be \"all\", \"core\", or \"extension\"")
    }
  }
  # Function to find the classes contained in the current URL
  findGBIFClasses <- function(specAddress, termList) {
    # Function to generate a GBIF class from a list of terms
    createGBIFClass <- function(specAddress, termList) {
      # Function to add a trailing "/" if there isn't one
      addTrailingDir <- function(inText) {
        outText <- as.character(inText)
        if(!grepl("\\/$", outText, perl = TRUE)) {
          outText <- paste(outText, "/", sep = "")
        }
        outText
      }
      curDoc <- read_xml(specAddress)
      # Get the qualified names of each of the members of the class
      memberTerms <- sapply(X = xml_children(curDoc), FUN = function(curNode) {
        paste(addTrailingDir(xml_attr(curNode, "namespace")), xml_attr(curNode, "name"), sep = "")
      })
      memberTerms <- memberTerms[memberTerms %in% names(termList)]
      # Format the output object into a list of terms for the class
      list(
        termInfo = termList[[paste(addTrailingDir(xml_attr(curDoc, "namespace")),
          switch(specAddress,
            "https://rs.gbif.org/extension/ggbn/cloning.xml" = "Cloning",    # Cloning class been given an incorrect name in the GBIF API so need to over-ride it here
            xml_attr(curDoc, "name")
          ),
          sep = "")]],
        compositeTerms = termList[memberTerms]
      )
    }
    # Retrieve the link nodes for all the entries in the specification page
    if(!grepl("/$", specAddress, perl = TRUE)) {
      specAddress <- paste(specAddress, "/", sep = "")
    }
    aNodes <- xml_find_all(read_html(specAddress), "//td/a")
    aLinks <- sapply(X = aNodes[2:length(aNodes)], FUN = xml_attr, attr = "href")
    # Retrieve the links that are directories
    dirLinks <- aLinks[grepl("/$", aLinks, perl = TRUE)]
    # For the links that are XML files: make sure the links are only the most recently defined
    xmlLinks <- aLinks[grepl("\\.xml$", aLinks, perl = TRUE)]
    xmlLinks <- sapply(X = unique(gsub("_*\\d\\d\\d\\d[_-]\\d\\d[_-]\\d\\d_*", "", xmlLinks, perl = TRUE)), FUN = function(curLink, xmlLinks) {
      possLinks <- xmlLinks[curLink == gsub("_*\\d\\d\\d\\d[_-]\\d\\d[_-]\\d\\d_*", "", xmlLinks, perl = TRUE)]
      if(length(possLinks) > 1) {
        # If the there are multiple possible links for the class definition then use the most recent definition
        curDates <- strptime(
          gsub("^.*(\\d\\d\\d\\d)[_-](\\d\\d)[_-](\\d\\d).*$", "\\1-\\2-\\3", possLinks, perl = TRUE),
          "%Y-%m-%d")
        possLinks <- possLinks[which.max(ifelse(is.na(curDates), -Inf, as.double(as.POSIXlt(curDates))))]
      }
      if(length(possLinks) == 0) {
        possLinks <- curLink
      }
      possLinks
    }, xmlLinks = xmlLinks)
    # Go through each of the links and process each entry
    do.call(c, lapply(X = c(dirLinks, xmlLinks), FUN = function(curLink, curBaseAddress, termList) {
      outVals <- list()
      if(grepl("\\.xml$", curLink, perl = TRUE)) {
        # If the link is to a XML file then scrape the property information from it
        outVals <- list(createGBIFClass(paste(curBaseAddress, curLink, sep = ""), termList))
      } else if(grepl("/$", curLink, perl = TRUE)) {
        # If the link is to another folder then call the function recursively
        outVals <- findGBIFClasses(paste(curBaseAddress, curLink, sep = ""), termList)
      }
      outVals
    }, curBaseAddress = specAddress, termList = termList))
  }
  outList <- do.call(c, lapply(X = urlCheck, FUN = findGBIFClasses, termList = termList))
  outList <- outList[sapply(X = outList, FUN = function(curEl) {!is.null(curEl$termInfo)})]
  names(outList) <- sapply(X = outList, FUN = function(curEl) {curEl$termInfo$getQualifiedName()})
  outList
}

# TODO: document helper function
isDwCTerm <- function(inOb) {
  any(class(inOb) == "DwCTerm")
}

# TODO: document GBIF core class retrieval function
#' Class retrieval function
#' @export
#
getGBIFCoreClasses <- function() {
  setNames(lapply(X = GBIFCoreClassList, FUN = function(curClass) {curClass$termInfo}),
    paste("GBIF", sapply(X = GBIFCoreClassList, FUN = function(curClass) {curClass$termInfo$getTermName()}), sep = ""))
}

# TODO: document GBIF extension class retrieval function
getGBIFExtensionClasses <- function() {
  setNames(lapply(X = GBIFExtClassList, FUN = function(curClass) {curClass$termInfo}),
           paste("GBIF", sapply(X = GBIFExtClassList, FUN = function(curClass) {curClass$termInfo$getTermName()}), sep = ""))
}
