# ------ 1. INTERNAL TAG CREATION FUNCTIONS ------

# ====== 1.1. Generate tag IDs ======
#' Check tag ID
#' Check values given as IDs for EML tag. It the entry is NA then a unique ID is generated
#' @param tagID A \code{character} scalar for with the ID code
#' @return A \code{character} scalar containing the processed ID code
#'
#' @seealso \code{\link[uuid::UUIDgenerate]{UUIDgenerate}}
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @keywords internal
#' @export
#'
generateID <- function(tagID) {
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
  if(!is.na(outArg)) {
    # If the tag ID is NA then generate a unique tag code from the UUID package
    outArg <- uuid::UUIDgenerate()
  }
  outArg
}

# ====== 1.2. Add tag ======
#' Generate a Living Norway metadata tag
#' Add a metadata HTML tag.  This function is designed to be run in RMarkdown documents.  This function
#' is not designed to be run directly by the users but serves as a utility functions for other
#' HTML tag generation functions.
#' @param tagTxt A \code{character} scalar containing the text to be enclosed in the tag
#' @param tagtype A \code{character} scalar containing the name of the EML element that will be tagged
#' @param tagID A \code{character} scalar containing the unique ID of the tag
#' @param parentID A \code{character} scalar containing the unique ID of the parent of the tag
#' (if there is a parent)
#' @param isHidden A \code{logical} scalar determining whether the tag should appear in the rendered HTML text
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @keywords  internal
#' @export
#'
LNaddTag <- function(tagText, tagType, tagID = NA, parentID = NA, isHidden = FALSE, ...) {
  # Function to sanity check the tag related inputs
  checkArgText <- function(inArg, argName) {
    outArg <- tryCatch(as.character(inArg), error = function(err) {
      stop("error encountered whilst processing ", argName, " parameter: ", err)
    })
    if(length(outArg) <= 0) {
      outArg <- NA
    } else if(length(outArg) > 1) {
      warning("parameter ", argName, " has a length greater than 1: only using the first element")
      outArg <- outArg[1]
    }
    if(is.na(outArg)) {
      outArg <- NA
    } else if(grepl("\\s+", outArg, perl = TRUE)) {
      stop("error encountered whilst processing ", argName, "parameter: whitespace is present")
    }
    outArg
  }
  # Sanity check the input text
  curText <- tryCatch(paste(as.character(tagText), collapse = " "), error = function(err) {
    stop("error encountered whilst processing tagText parameter: ", err)
  })
  if(is.na(curText)) {
    curText <- ""
  }
  # Sanity check the tag ID
  curTagID <- generateID(tagID)
  # Sanity check the tag type
  curTagType <- checkArgText(tagType, "tagType")
  if(is.na(curTagType) || !(curTagType %in% names(getTagGenerationFunctions()))) {
    stop("error encountered whilst processing tagType parameter: tag type not found")
  }
  # Sanity check the parent ID
  curParentID <- checkArgText(parentID, "parentID")
  # Sanity check the hidden parameter
  curHidden <- tryCatch(as.logical(isHidden), error = function(err) {
    stop("error encountered whilst processing isHidden parameter: ", err)
  })
  if(length(curHidden) < 0) {
    stop("error encountered whilst processing isHidden parameter: vector length is zero")
  } else if(length(curHidden) > 1) {
    warning("parameter isHidden has a length greater than 1: only using the first element")
    curHidden <- curHidden[1]
  }
  if(is.na(curHidden)) {
    # Default to FALSE if the hidden input is NA
    curHidden <- FALSE
  }
  outText <- ""
  # Ascertain whether HTML output is being requested
  isHTML <- knitr::is_html_output()
  if(isHTML) {
    # If the output is HTML then contain the text within a span tag
    if(!is.na(curText) && curText != "") {
      outText <- paste(
        "<span id=\"LN", curTagType, "_", curTagID,
        ifelse(is.na(curParentID), "", paste("_", curParentID, sep = "")),
        "\" class=\"LNmetadata\"",
        ifelse(curHidden, " style=\"display:none\"", ""),
        ">", curText, "</span>",
        sep = "")
    } else {
      outText <- paste(
        "<span id=\"LN", curTagType, "_", curTagID,
        ifelse(is.na(curParentID), "", paste("_", curParentID, sep = "")),
        "\" class=\"LNmetadata\"",
        ifelse(curHidden, " style=\"display:none\"", ""),
        "/>",
        sep = "")
    }
  } else {
    # If the output is not HTML then simply display the tag text without any HTML markup (unless the text is hidden, in which case don't display anything at all)
    outText <- ifelse(curHidden, "", curText)
  }
  outText
}

# ====== 1.3. Retrieve the tag generation functions ======
#' Retrieve the tag generation functions
#' Get a full list of functions that generate tags formatted so that metadata scraper can
#' retrieve and collate the metadata
#' @return \code{List} of functions for each of the available Living Norway metadata tags
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @keywords  internal
#' @export
#'
getTagGenerationFunctions <- function() {
  list(
    # Low-level tags that don't have any children
    "alternateIdentifier" = LNalternateIdentifier,
    "taxonomicClassification" = LNtaxonomicClassification,
    "commonName" = LNcommonName,
    "taxonRankValue" = LNtaxonRankValue,
    "taxonRankName" = LNtaxonRankName,
    "calendarDate" = LNcalendarDate,
    "northBoundingCoordinate" = LNnorthBoundingCoordinate,
    "southBoundingCoordinate" = LNsouthBoundingCoordinate,
    "eastBoundingCoordinate" = LNeastBoundingCoordinate,
    "westBoundingCoordinate" = LNwestBoundingCoordinate,
    "geographicDescription" = LNgeographicDescription ,
    "intellectualRights" = LNintellectualRights,
    "keywordThesaurus" = LNkeywordThesaurus,
    "keyword" = LNkeyword,
    "abstract" = LNabstract,
    "language" = LNlanguage,
    "pubDate" = LNpubDate,
    "electronicMail" = LNelectronicMail,
    "postalCode" = LNpostalCode,
    "city" = LNcity,
    "deliveryPoint" = LNdeliveryPoint,
    "positionName" = LNpositionName,
    "organizationName" = LNorganizationName,
    "lastName" = LNlastName,
    "firstName" = LNfirstName,
    "title" = LNtitle,
    "methodStep" = LNmethodStep,
    "qualityControl" = LNqualityControl,
    "sampling" = LNsampling,
    "studyExtent" = LNstudyExtent,
    "samplingDescription" = LNsamplingDescription,
    "purpose" = LNpurpose,
    # Collection tags that can have children
    "individualName" = LNindividualName,             # individualName tag is: (firstName, lastName)
    "creator" = LNcreator,                           # creator tag is: (individualName, organizationName, positionName, deliveryPoint, city, postalCode, electronicMail)
    "metadataProvider" = LNmetadataProvider,         # metadataProvider tag is: (individualName, organizationName, positionName, deliveryPoint, city, postalCode, electronicMail)
    "keywordSet" = LNkeywordSet,                     # keywordSet tag is: (keyword, keywordThesaurus)
    "coverage" = LNcoverage,                         # coverage tag is: (geographicCoverage, temporalCoverage, taxonomicCoverage)
    "geographicCoverage" = LNgeographicCoverage,     # geographicCoverage tag is: (geographicDecription, boundingCoordinates)
    "boundingCoordinates" = LNboundingCoordinates,   # boundingCoordinates tag is: (westBoundingCoordinate, eastBoundingCoordinate, northBoundingCoordinate, southBoundingCoordinate)
    "temporalCoverage" = LNtemporalCoverage,         # temporalCoverage tag is: (calenderDate (beginDate, endDate), rangeOfDates)
    "beginDate" = LNbeginDate,                       # beginDate tag is: (calendarDate)
    "endDate" = LNendDate,                           # endDate is: (calendarDate)
    "rangeOfDates" = LNrangeOfDates,                 # rangeOfDates is: (beginDate, endDate)
    "taxonomicCoverage" = LNtaxonomicCoverage        # taxonomicCoverage is: (taxonRankName, taxonRankValue, commonName, taxonomicClassification)
  )
}

# ====== 1.4. Process the child tags ======
#' Process the child-parent metadata tags
#' Create a series of child metadata tags based on a naming convention of \code{...} arguments
#' @param parentID A \code{character} scalar containing the unique ID of the parent of the tag
#' (if there is a parent)
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param isHiddenDefault A \code{logical} scalar that set the default \code{isHidden} parameter
#' for each of the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @keywords  internal
#' @export
#'
processChildArgs <- function(parentID, sep = sep, isHiddenDefault = FALSE, ...) {
  # Process the input ID
  curID <- generateID(parentID)
  # Go through each of the potential child tag generation functions and
  outTags <- sapply(X = names(getTagGenerationFunctions()), FUN = function(curTag, curID, genFuncs, inArgs) {
    outText <- NA
    # Retrieve the arguments that correspond to the current tag
    isTagArg <- grepl(paste("^", curTag, "\\.", sep = ""), names(inArgs), perl = TRUE)
    if(any(isTagArg)) {
      # Rename the arguments so that they can used correctly in the relevant function
      newParams <- setNames(inArgs[isTagArg], gsub(paste("^", curTag, "\\.", sep = ""), "", names(inArgs)[isTagArg], perl = TRUE))
      if(!("parentID" %in% names(newParams))) {
        newParams <- append(newParams, list(parentID = curID))
      }
      if(!("isHidden" %in% names(newParams))) {
        newParams <- append(newParams, list(isHidden = isHiddenDefault))
      }
      outText <- do.call(genFuncs[[curTag]], newParams)
    }
    outText
  }, curID = curID, genFuncs = getTagGenerationFunctions(), inArgs = list(...))
  # Collect together the processed tags and collapse them into one character string
  tryCatch(paste(outTags[!is.na(outTags)], collapse = as.character(sep)), error = function(err) {
    stop("error encountered during processing of metadata tag parent-child arguments")
  })
}

# ------ 2. TAG GENERATION FUNCTIONS ------

# ====== 2.1. LNindividualName tag ======
#' Add a Living Norway metadata tag corresponding to the individualName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNindividualName <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.2. LNcreator tag ======
#' Add a Living Norway metadata tag corresponding to the creator EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNcreator <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.3. LNmetadataProvider tag ======
#' Add a Living Norway metadata tag corresponding to the metadataProvider EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNmetadataProvider <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.4. LNkeywordSet tag ======
#' Add a Living Norway metadata tag corresponding to the keywordSet EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNkeywordSet <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.5. LNcoverage tag ======
#' Add a Living Norway metadata tag corresponding to the coverage EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNcoverage <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.6. LNgeographicCoverage tag ======
#' Add a Living Norway metadata tag corresponding to the geographicCoverage EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNgeographicCoverage <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.7. LNboundingCoordinates ======
#' Add a Living Norway metadata tag corresponding to the boundingCoordinates EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNboundingCoordinates <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.8. LNtemporalCoverage tag ======
#' Add a Living Norway metadata tag corresponding to the temporalCoverage EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNtemporalCoverage <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.9. LNbeginDate tag ======
#' Add a Living Norway metadata tag corresponding to the beginDate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNbeginDate <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.10. LNendDate tag ======
#' Add a Living Norway metadata tag corresponding to the endDate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNendDate <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.11. LNrangeOfDates tag ======
#' Add a Living Norway metadata tag corresponding to the rangeOfDates EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNrangeOfDates <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.12. LNtaxonomicCoverage tag ======
#' Add a Living Norway metadata tag corresponding to the taxonomicCoverage EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @param sep A \code{character} scalar providing the delimiter to use between the child tags
#' @param ... A series of parameters defining the child tags to generate and the parameters of the
#' generation functions
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNtaxonomicCoverage <- function(tagText = NA, tagID = NA, parentID = NA, isHidden = FALSE, sep = " ", ...) {
  # Retrieve the tag for the function
  curID <- generateID(tagID)
  paste(
    # Format the collection tag
    LNaddTag(tagText, "individualName", curID, parentID, isHidden),
    # Format the children tags (if provided)
    processChildArgs(curID, sep, isHidden, ...),
    sep = "")
}

# ====== 2.13. LNalternativeIdentifier tag ======
#' Add a Living Norway metadata tag corresponding to the alternativeIdentifier EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNalternateIdentifier <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "alternateIdentifier", tagID, parentID, isHidden)
}

# ====== 2.14. LNtitle tag ======
#' Add a Living Norway metadata tag corresponding to the title EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNtitle <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "title", tagID, parentID, isHidden)
}

# ====== 2.15. LNfirstName tag ======
#' Add a Living Norway metadata tag corresponding to the firstName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNfirstName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "firstName", tagID, parentID, isHidden)
}

# ====== 2.16. LNlastName tag ======
#' Add a Living Norway metadata tag corresponding to the lastName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNlastName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "lastName", tagID, parentID, isHidden)
}

# ====== 2.17. LNorganizationName tag ======
#' Add a Living Norway metadata tag corresponding to the organizationName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNorganizationName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "organizationName", tagID, parentID, isHidden)
}

# ====== 2.18. LNpositionName tag ======
#' Add a Living Norway metadata tag corresponding to the positionName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNpositionName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "positionName", tagID, parentID, isHidden)
}

# ====== 2.19. LNdeliveryPoint tag ======
#' Add a Living Norway metadata tag corresponding to the deliveryPoint EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNdeliveryPoint <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "deliveryPoint", tagID, parentID, isHidden)
}

# ====== 2.20. LNcity tag ======
#' Add a Living Norway metadata tag corresponding to the city EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNcity <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "city", tagID, parentID, isHidden)
}

# ====== 2.21. LNpostalCode tag ======
#' Add a Living Norway metadata tag corresponding to the postalCode EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNpostalCode <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "postalCode", tagID, parentID, isHidden)
}

# ====== 2.22. LNelectronicMail tag ======
#' Add a Living Norway metadata tag corresponding to the electronicMail EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNelectronicMail <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "electronicMail", tagID, parentID, isHidden)
}

# ====== 2.23. LNpubDate tag ======
#' Add a Living Norway metadata tag corresponding to the pubDate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNpubDate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "pubDate", tagID, parentID, isHidden)
}

# ====== 2.24. LNlanguage tag ======
#' Add a Living Norway metadata tag corresponding to the language EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNlanguage <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(toupper(tagText), "language", tagID, parentID, isHidden)
}

# ====== 2.25. LNabstract tag ======
#' Add a Living Norway metadata tag corresponding to the abstract EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNabstract <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "abstract", tagID, parentID, isHidden)
}

# ====== 2.26. LNkeyword tag ======
#' Add a Living Norway metadata tag corresponding to the keyword EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNkeyword <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "keyword", tagID, parentID, isHidden)
}

# ====== 2.27. LNkeywordThesaurus tag ======
#' Add a Living Norway metadata tag corresponding to the keyworkThesaurus EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNkeywordThesaurus <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "keywordThesaurus", tagID, parentID, isHidden)
}

# ====== 2.28. LNintellectualRights tag ======
#' Add a Living Norway metadata tag corresponding to the intellectualRights EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNintellectualRights <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "intellectualRights", tagID, parentID, isHidden)
}

# ====== 2.29. LNgeographicDescription tag ======
#' Add a Living Norway metadata tag corresponding to the geographicDescriprion EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNgeographicDescription  <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "geographicDescription", tagID, parentID, isHidden)
}

# ====== 2.30. LNwestBoundingCoordinate tag ======
#' Add a Living Norway metadata tag corresponding to the westBoundingCoordinate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNwestBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "westBoundingCoordinate", tagID, parentID, isHidden)
}

# ====== 2.31. LNeastBoundingCoordinate tag ======
#' Add a Living Norway metadata tag corresponding to the eastBoundingCoordinate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNeastBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "eastBoundingCoordinate", tagID, parentID, isHidden)
}

# ====== 2.32. LNsouthBoundingCoordinate tag ======
#' Add a Living Norway metadata tag corresponding to the southBoundingCoordinate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNsouthBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "southBoundingCoordinate", tagID, parentID, isHidden)
}

# ====== 2.33. LNnorthBoundingCoordinate ======
#' Add a Living Norway metadata tag corresponding to the northBoundingCoordinate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNnorthBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "northBoundingCoordinate", tagID, parentID, isHidden)
}

# ====== 2.34. LNcalendarDate tag ======
#' Add a Living Norway metadata tag corresponding to the calendarDate EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNcalendarDate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "calendarDate", tagID, parentID, isHidden)
}

# ====== 2.35. LNtaxonRankName tag ======
#' Add a Living Norway metadata tag corresponding to the taxonRankName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNtaxonRankName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "taxonRankName", tagID, parentID, isHidden)
}

# ====== 2.36. LNtaxonRankValue tag ======
#' Add a Living Norway metadata tag corresponding to the taxonRankValue EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNtaxonRankValue <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "taxonRankValue", tagID, parentID, isHidden)
}

# ====== 2.37. LNcommonName tag ======
#' Add a Living Norway metadata tag corresponding to the commonName EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNcommonName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "commonName", tagID, parentID, isHidden)
}

# ====== 2.38. LNtaxanomicClassification tag ======
#' Add a Living Norway metadata tag corresponding to the taxanomicClassification EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNtaxonomicClassification <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "taxonomicClassification", tagID, parentID, isHidden)
}

# ====== 2.39. LNmethodStep tag ======
#' Add a Living Norway metadata tag corresponding to the methodStep EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNmethodStep <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "methodStep", tagID, parentID, isHidden)
}

# ====== 2.40. LNqualityControl tag ======
#' Add a Living Norway metadata tag corresponding to the qualityControl EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNqualityControl <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "qualityControl", tagID, parentID, isHidden)
}

# ====== 2.41. LNsampling tag ======
#' Add a Living Norway metadata tag corresponding to the sampling EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNsampling <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "sampling", tagID, parentID, isHidden)
}

# ====== 2.42. LNstudyExtent tag ======
#' Add a Living Norway metadata tag corresponding to the studyExtent EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNstudyExtent <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "studyExtent", tagID, parentID, isHidden)
}

# ====== 2.43. LNsamplingDescription tag ======
#' Add a Living Norway metadata tag corresponding to the samplingDescription EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNsamplingDescription <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "samplingDescription", tagID, parentID, isHidden)
}

# ====== 2.44. LNpurpose tag ======
#' Add a Living Norway metadata tag corresponding to the purpose EML element
#' @param tagText A \code{character} scalar containing the text to encolse in the tag
#' @param tagID A \code{character} scalar containing a unique identifier for the tag element. If
#' the parameter is \code{NA} then a UUID will be generated
#' @param parentID A \code{character} scalar containing the ID for the parent tag (if there is one).
#' \code{NA} denotes that there is no parent for the tag
#' @param isHidden A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
#' hidden when rendered
#' @return A \code{character} scalar containing the rendered output including any HTML tags if a
#' HTML document is being knitted
#' @author Joseph D. Chipperfield, \email{joechip90@@googlemail.com}; Matthew Grainger, \email{matthew.grainger@@nina.no}
#' @export
#'
LNpurpose <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  LNaddTag(tagText, "purpose", tagID, parentID, isHidden)
}

