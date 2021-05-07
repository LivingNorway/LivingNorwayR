#' LNaddTag
#' Add metadata html tag (only run in RMarkdown)
#' @param tagTxt The text for the tag
#' @param tagtype The name of the EML element that will be tagged
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

# ------ 1. TAG CREATION FUNCTION ------
LNaddTag <- function(tagText, tagType, tagID = NA, parentID = NA, isHidden = FALSE) {
  # ====== 1.1. Sanity check the inputs ======
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
  curTagID <- checkArgText(tagID, "tagID")
  if(is.na(curTagID)) {
    # If the tag ID hasn't been provided then generate one with a UUID
    curTagID <- uuid::UUIDgenerate()
  }
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
  # ====== 1.2. Write the tag outputs ======
  # Ascertain whether HTML output is being requested
  isHTML <- knitr::is_html_output()
  if(isHTML) {
    # If the output is HTML then contain the text within a span tag
    outText <- paste(
      "<span id=\"LN", curTagID,
      ifelse(is.na(curParentID), "", paste("_", curParentID, sep = "")),
      " class=\"LNmetadata_", curTagType, "\"",
      ifelse(curHidden, " style=\"display:none\"", ""),
      ">", curText, "</span>",
      sep = "")
  } else {
    # If the output is not HTML then simply display the tag text without any HTML markup (unless the text is hidden, in which case don't display anything at all)
    outText <- ifelse(curHidden, "", curText)
  }
  cat(outText)
  invisible(outText)
}

#' getTagGenerationFunctions
#' Get full list of available LNtags
#' @return list of available LivingNorwayR metadata tags
#' @export


getTagGenerationFunctions <- function() {
  list(
    "alternateIdentifier" = LNalternateIdentifier,
    "taxonomicClassification"=LNtaxonomicClassification,
    "commonName"=LNcommonName,
    "taxonRankValue"=LNtaxonRankValue,
    "taxonRankName"=LNtaxonRankName,
    "calendarDate"=LNcalendarDate,
    "northBoundingCoordinate"=LNnorthBoundingCoordinate,
    "southBoundingCoordinate"=LNsouthBoundingCoordinate,
    "eastBoundingCoordinate"=LNeastBoundingCoordinate,
    "westBoundingCoordinate"=LNwestBoundingCoordinate,
    "geographicDescription"=LNgeographicDescription ,
    "intellectualRights"=LNintellectualRights,
    "keywordThesaurus"=LNkeywordThesaurus,
    "keyword"=LNkeyword,
    "abstract"=LNabstract,
    "language"=LNlanguage,
    "pubDate"=LNpubDate,
    "electronicMail"=LNelectronicMail,
    "postalCode"=LNpostalCode,
    "city"=LNcity,
    "deliveryPoint"=LNdeliveryPoint,
    "positionName"=LNpositionName,
    "organizationName"=LNorganizationName,
    "lastName"=LNlastName,
    "firstName"=LNfirstName,
    "title"=LNtitle,
    "methodStep"=LNmethodStep,
    "qualityControl"= LNqualityControl,
    "sampling"=LNsampling,
    "studyExtent"=LNstudyExtent,
    "samplingDescription"=LNsamplingDescription
  )
}

#' LNalternativeIdentifier
#' Add alternativeIdentifier metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNalternateIdentifier <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "alternateIdentifier", tagID, parentID, isHidden))
}


#' LNtitle
#' Add title metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNtitle <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "title", tagID, parentID, isHidden))
}

#######################################################################################################
####################### individualName tag is firstName, lastName ############################################
#######################################################################################################
#' LNfirstName
#' Add firstName metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNfirstName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "firstName", tagID, parentID, isHidden))
}


#' LNlastName
#' Add lastName metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNlastName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "lastName", tagID, parentID, isHidden))
}


#######################################################################################################
####################### creator tag is individualName, organizationName, positionName, ################
####################### deliveryPoint, city, postalCode, electronicMail ###############################
#######################################################################################################


#' LNorganizationName
#' Add organizationName metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNorganizationName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "organizationName", tagID, parentID, isHidden))
}


#' LNpositionName
#' Add positionName metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNpositionName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "positionName", tagID, parentID, isHidden))
}


#' LNdeliveryPoint
#' Add deliveryPoint metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNdeliveryPoint <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "deliveryPoint", tagID, parentID, isHidden))
}


#' LNcity
#' Add city metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNcity <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "city", tagID, parentID, isHidden))
}

#' LNpostalCode
#' Add postalCode metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNpostalCode <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "postalCode", tagID, parentID, isHidden))
}


#' LNelectronicMail
#' Add electronicMail metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNelectronicMail <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "electronicMail", tagID, parentID, isHidden))
}


#######################################################################################################
####################### metadataProvider tag is individualName, organizationName, positionName, ################
####################### deliveryPoint, city, postalCode, electronicMail ###############################
#######################################################################################################


#' LNpubDate
#' Add pubDate metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNpubDate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "pubDate", tagID, parentID, isHidden))
}



#' LNlanguage
#' Add language metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNlanguage <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(toupper(tagText), "language", tagID, parentID, isHidden))
}

#' LNabstract
#' Add abstract metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNabstract <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "abstract", tagID, parentID, isHidden))
}



#######################################################################################################
####################### keywordSet tag is keyword, keywordThesaurus   #################################
#######################################################################################################


#' LNkeyword
#' Add keyword metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNkeyword <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "keyword", tagID, parentID, isHidden))
}



#' LNkeywordThesaurus
#' Add keywordThesaurus metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNkeywordThesaurus <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "keywordThesaurus", tagID, parentID, isHidden))
}

#######################################################################################################
#######################################################################################################

#' LNintellectualRights
#' Add intellectualRights metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNintellectualRights <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "intellectualRights", tagID, parentID, isHidden))
}

#######################################################################################################
####################### coverage is geographicCoverage, temporalCoverage, taxonomicCoverage ###########
#######################################################################################################


### geographicCoverage
#######################################################################################################
####################### geographicCoverage is geographicDecription,boundingCoordinates ################
#######################################################################################################

#'LNgeographicDescription
#' Add geographicDescription  metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNgeographicDescription  <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "geographicDescription", tagID, parentID, isHidden))
}



#######################################################################################################
####################### boundingCoordinates is  westBoundingCoordinate, eastBoundingCoordinate, #######
####################### northBoundingCoordinate,southBoundingCoordinate   #############################
#######################################################################################################




#'LNwestBoundingCoordinate
#' Add westBoundingCoordinate metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNwestBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "westBoundingCoordinate", tagID, parentID, isHidden))
}


#'LNeastBoundingCoordinate
#' Add eastBoundingCoordinate metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNeastBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "eastBoundingCoordinate", tagID, parentID, isHidden))
}


#'LNsouthBoundingCoordinate
#' Add southBoundingCoordinate metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNsouthBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "southBoundingCoordinate", tagID, parentID, isHidden))
}


#'LNnorthBoundingCoordinate
#' Add northBoundingCoordinate metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNnorthBoundingCoordinate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "northBoundingCoordinate", tagID, parentID, isHidden))
}



#Temporal coverage

#######################################################################################################
####################### TemporalCoverage is  calenderDate (beginDate, endDate), rangeOfDates###########
#######################################################################################################

#######################################################################################################
####################### beginDate is CalendarDate #####################################################
#######################################################################################################

#######################################################################################################
####################### endDate is CalendarDate #######################################################
#######################################################################################################


#######################################################################################################
####################### rangeOfDates is beginDate,endDate #############################################
#######################################################################################################


#' LNcalendarDate
#' Add calendarDate metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNcalendarDate <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "calendarDate", tagID, parentID, isHidden))
}

#Taxonomic coverage

#######################################################################################################
####################### TaxonomicCoverage is  taxonRankName, taxonRankValue, commonName, ##############
####################### taxonomicClassification                                         ###############
#######################################################################################################



#' LNtaxonRankName
#' Add taxonRankName metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNtaxonRankName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "taxonRankName", tagID, parentID, isHidden))
}


#' LNtaxonRankValue
#' Add taxonRankValue metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNtaxonRankValue <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "taxonRankValue", tagID, parentID, isHidden))
}

#' LNcommonName
#' Add commonName metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNcommonName <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "commonName", tagID, parentID, isHidden))
}

#' LNtaxonomicClassification
#' Add taxonomicClassification metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNtaxonomicClassification <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "taxonomicClassification", tagID, parentID, isHidden))
}

#' LNmethodStep
#' Add methodStep metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNmethodStep <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "methodStep", tagID, parentID, isHidden))
}

#' LNqualityControl
#' Add qualityControl metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNqualityControl <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "qualityControl", tagID, parentID, isHidden))
}

#' LNsampling
#' Add sampling metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNsampling <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "sampling", tagID, parentID, isHidden))
}

#' LNstudyExtent
#' Add studyExtent metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNstudyExtent <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "studyExtent", tagID, parentID, isHidden))
}


#' LNsamplingDescription
#' Add samplingDescription metadata html tag (only run in RMarkdown)
#' @param tagText the text of the tag
#' @param tagID Any unique ID of the tag
#' @param parentID Any unique ID of the parent of the tag
#' @param isHidden Hide the tag in the rendered html text
#' @return Output: html tag
#' @export

LNsamplingDescription <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisible(LNaddTag(tagText, "samplingDescription", tagID, parentID, isHidden))
}


