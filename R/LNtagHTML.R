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
    if(outArg == "") {
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
  if(is.na(curTagType) || !(curTageType %in% names(getTagGenerationFunctions()))) {
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
    # If the output is not HTML then simply display the tag text without any HTML markup
    outText <- curText
  }
  cat(outText)
  invisible(outText)
}

LNalternateIdentifier <- function(tagText, tagID = NA, parentID = NA, isHidden = FALSE) {
  invisble(LNaddTag(tagText, "alternateIdentifier", tagID, parentID, isHidden))
}

getTagGenerationFunctions <- function() {
  list(
    "alternateIdentifier" = LNalternateIdentifier
  )
}
