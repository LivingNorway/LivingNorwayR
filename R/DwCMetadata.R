# ------ 1. METADATA HANDLING CLASS ------
#' R6 metadata class
#' @description R6 class that represents metadata in DwC
#'
#' @details The \code{DwCMetadata} class exists as a handling class for the encapsulation and
#' handling of metadata being presented
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
DwCMetadata<-R6::R6Class(
  classname = "DwCMetadata",
  # ====== 1.1. Define private members of the generic class ======
  private = list(
    # An XML object containing the metadata according to the EML schema
    xmlContent = NULL,
    # A character vector containing the entire unprocessed long-form metadata (if any)
    longForm = character(),
    # ---- 1.1.1. Validate the EML against a schema ----
    validateEML = function(fileEncoding = "") {
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Call the EML validator in the 'emld' package (this will parse the validation schema and return any validation errors)
      outValue <- emld::eml_validate(private$xmlContent, inFileEncoding)
      # Retrieve any errors encountered during the validation
      errorMsgs <- attr(outValue, "errors")
      if(!is.null(errorMsgs) && length(errorMsgs) > 0) {
        # Display the validation errors as warning messages
        sapply(X = as.character(errorMsgs), FUN = warning)
      }
      outValue
    },
    # ---- 1.1.2. Helper function for sanity checking ----
    # Helper function for sanity checking
    charSanityCheck = function(inVal, paramName, defaultValue) {
      proVal <- tryCatch(as.character(inVal), error = function(err, paramName = paramName) {
        stop("error encountered processing ", paramName, " parameter: ", err)
      })
      if(length(proVal) <= 0) {
        proVal <- defaultValue
      } else if(length(proVal) > 1) {
        warning("parameter ", paramName, " has length greater than one: only the first element will be used")
        proVal <- proVal[1]
      }
      if(is.na(proVal) || proVal == "") {
        proVal <- defaultValue
      }
      proVal
    },
    # ---- 1.1.3. Helper function for language-based EML traversal ----
    # Return node-set that match the language specification
    retrieveLangNodes = function(xmlPath,lang) {
      # Sanity check the language and XPath arguments
      inLang <- private$charSanityCheck(lang, "lang", NA)
      xmlPath <- private$charSanityCheck(xmlPath, "xmlPath", "//*")
      outNodes <- NULL
      if(!is.null(private$xmlContent)) {
        if(is.na(inLang)) {
          # If a language specification isn't set then select the nodes according to XPath
          outNodes <- xml2::xml_find_all(private$xmlContent, xmlPath)
        } else {
          # If a language specification is set then only select the node or "value" tagged
          # children of the XPath matching nodes that have language attributes that match
          outNodes <- xml2::xml_find_all(private$xmlContent, paste(
            xmlPath, "[@lang=\"", inLang, "\" or @xml:lang=\"", inLang, "\"]|",
            xmlPath, "/value[@lang=\"", inLang, "\" or @xml:lang=\"", inLang, "\"]",
            sep = ""))
          # If no elements could be found of the correct language then look for the default
          # entries instead
          if(length(outNodes) <= 0) {
            outNodes <- xml2::xml_find_all(private$xmlContent, xmlPath)
          }
        }
      }
      outNodes
    }
  ),
  public = list(
    # ====== 1.2. Import metadata from a Living Norway HTML file ======
    #' @description
    #' Retrieve metadata information from a HTML file that has been created with a
    #' Living Norway HTML tag schema
    #' @param fileLocation A \code{character} scalar containing the location of the
    #' HTML file
    #' @param fileEncoding A \code{character} scalar. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @param ... A set of arguments to be passed to the \code{\link[xml2::xml_new_root]{xml_new_root}}
    #' function to define namespace information.  The values returned by the helper function
    #' \code{\link[getDefaultEMLDefinitionInfo]{getDefaultEMLDefinitionInfo}} will be used unless
    #' overwritten by user input here.  In addition, arguments required by the EML standard but
    #' not provided here (such as the \link[https://ediorg.github.io/data-package-best-practices/EMLmetadata/Root-element.html]{packageID}
    #' attribute) will be otherwise auto-generated
    #' @export
    importFromLivingNorwayHTML = function(fileLocation, fileEncoding = "", ...) {
      # Process the file encoding parameter
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- private$charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Import the HTML file into an XML object
      inXML <- tryCatch(xml2::read_html(as.character(inFileLocation)), error = function(err) {
        stop("error importing HTML file: ", err)
      })
      # Search the document for any span tags that have the "LNmetadata" class
      metadataNodes <- xml2::xml_find_all(inXML, "//span[@class=\"LNmetadata\"]")
      # Retrieve the full formatted IDs of each of the metadata tags
      fullIDs <- t(sapply(X = metadataNodes, FUN = function(curNode) {
        # Complete ID string giving hierarchy and attribute information
        completeIDStr <- tryCatch(as.character(xml2::xml_attr(curNode, "id")), error = function(err) {
          stop("invalid ID code encountered when processing Living Norway HTML tags")
        })
        if(length(completeIDStr) <= 0) {
          stop("invalid ID code encountered when processing Living Norway HTML tags")
        } else if(length(completeIDStr) > 1) {
          warning("ID code in Living Norway HTML tag has length greater than one: only the first entry will be used")
          completeIDStr <- completeIDStr[1]
        }
        if(is.na(completeIDStr) || completeIDStr == "") {
          stop("invalid ID code encountered when processing Living Norway HTML tags")
        }
        # Initialise a string to hold the attribute information
        attrStr <- NA
        if(grepl("__", completeIDStr, fixed = TRUE)) {
          # If there is a double underscore then some attribute information is being included in the tag
          # too:
          #   The first section of text is the standard extended ID code
          #   Everything after the first is a set of attribute and value pairs
          tokenisedIDStr <- strsplit(completeIDStr, "__", fixed = TRUE)[[1]]
          completeIDStr <- tokenisedIDStr[1]
          attrStr <- paste(sapply(X = tokenisedIDStr[2:length(tokenisedIDStr)], FUN = function(curAttr) {
            gsub("_", "=", curAttr, fixed = TRUE)
          }), collapse = " ")
        }
        # Retrieve the components of the ID
        idComponents <- strsplit(completeIDStr, "_", fixed = TRUE)[[1]]
        # Test to ensure that the ID is formatted correctly
        if(length(idComponents) < 2) {
          stop("error encountered processing metadata ID codes: invalid ID format")
        }
        # Initialise an output vector with the ID code formatted to allow easier traversal
        outVector <- setNames(c(gsub("^LN", "", idComponents[1], perl = TRUE), idComponents[2], ifelse(length(idComponents) > 2, idComponents[3], NA), NA, attrStr), c("type", "id", "parent", "text", "attributes"))
        if(is.na(outVector[3])) {
          # Node does not have a parent specified - check the nestedness of the tag to see if it does have a parent
          parIDs <- sapply(X = xml2::xml_parents(curNode), FUN = function(curParNode) {
            outVal <- NA
            if(xml2::xml_name(curParNode) == "span" && xml2::xml_attr(curParNode, "class") == "LNmetadata") {
              # Retrieve the components of the ID of the parent node if it is of the correct node type
              idComponents <- strsplit(gsub("__.*$", "", xml2::xml_attr(curParNode, "id"), perl = TRUE), "_", fixed = TRUE)[[1]]
              # Test to ensure that the ID is formatted correctly
              if(length(idComponents) < 2) {
                stop("error encountered processing metadata ID codes: invalid ID format")
              }
              outVal <- idComponents[2]
            }
            outVal
          })
          # If there are valid metadata parents then set the nearest in the hierarchy to be the parent ID
          if(any(!is.na(parIDs))) {
            outVector[3] <- parIDs[!is.na(parIDs)][1]
          }
        }
        nodeText <- xml2::xml_text(curNode)
        if(length(nodeText) <= 0 | any(is.na(nodeText))) {
          nodeText <-NA
        } else {
          nodeText <- paste(nodeText, collapse = " ")
        }
        outVector[4] <- ifelse(nodeText == "", NA, nodeText)
        outVector
      }))
      # Check to ensure that all the IDs are unique
      if(anyDuplicated(fullIDs[, 2])) {
        stop("error encountered processing metadata ID codes: tag IDs are not unique")
      }
      # Initialise a set of attributes for the EML root tag
      initParams <- c(list(
        ".value" = "eml:eml"
      ), as.list(getDefaultEMLDefinitionInfo()), list(
        "packageId" = uuid::UUIDgenerate(),
        ".encoding" = inFileEncoding
      ))
      # Retrieve the parameters provided as extra arguments to the functions
      inputParams <- list(...)
      # Override the initialised parameters with those provided as arguments
      initParams[names(inputParams)] <- inputParams
      initParams <- setNames(lapply(X = initParams, FUN = function(curParam) {
        outVal <- private$charSanityCheck(curParam, "EML initialisation parameter", "")
        if(gsub(" ", "", outVal, fixed = TRUE) == "") {
          stop("invalid value given for an EML initialisation parameter")
        }
        outVal
      }), names(initParams))
      # Generate an EML structure
      emlOut <- do.call(xml2::xml_new_root, initParams)
      # Function to populate children of a node from an ID list
      makeChildren <- function(curParent, curNode, fullIDs) {
        idMatch <- rep(FALSE, nrow(fullIDs))
        if(is.na(curParent)) {
          idMatch <- is.na(fullIDs[, 3])
        } else {
          idMatch <- !is.na(fullIDs[, 3]) & curParent == fullIDs[, 3]
        }
        if(any(idMatch)) {
          apply(X = fullIDs[idMatch, , drop = FALSE], FUN = function(curRow, curNode) {
            # Add a child node
            if(is.na(curRow[5])) {
              outNode <- xml2::xml_add_child(curNode, curRow[1])
            } else {
              # If the node has attributes then set those
              attrVec <- strsplit(curRow[5], " ", fixed = TRUE)[[1]]
              attrVec <- setNames(
                gsub("^.*=", "", attrVec, perl = TRUE),
                gsub("=.*$", "", attrVec, perl = TRUE)
              )
              outNode <- do.call(xml2::xml_add_child, c(list(.x = curNode, .value = curRow[1]), as.list(attrVec)))
            }
            # Add text to the node if it has it
            if(!is.na(curRow[4])) {
              xml2::xml_text(outNode) <- curRow[4]
            }
            # Call the function recursively to populate any further children
            makeChildren(curRow[2], outNode, fullIDs)
          }, MARGIN = 1, curNode = curNode)
        }
      }
      makeChildren(NA, emlOut, fullIDs)
      private$xmlContent <- emlOut
      # Validate the EML
      #if(!private$validateEML(fileEncoding = inFileEncoding)) {
      #  stop("EML is not valid according to the selected EML schema")
      #}
      invisible(self)
    },
    # ====== 1.2. Import metadata from a Living Norway R markdown file ======
    #' @description
    #' Retrieve metadata information from a RMD file that has been created with a
    #' Living Norway HTML tag schema
    #' @param fileLocation A \code{character} scalar containing the location of the
    #' RMD file
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @param ... A set of arguments to be passed to the \code{\link[xml2::xml_new_root]{xml_new_root}}
    #' function to define namespace information.  The values returned by the helper function
    #' \code{\link[getDefaultEMLDefinitionInfo]{getDefaultEMLDefinitionInfo}} will be used unless
    #' overwritten by user input here.  In addition, arguments required by the EML standard but
    #' not provided here (such as the \link[https://ediorg.github.io/data-package-best-practices/EMLmetadata/Root-element.html]{packageID}
    #' attribute) will be otherwise auto-generated
    #' @export
    importFromLivingNorwayRMD = function(fileLocation, fileEncoding = "", ...) {
      # Process the file encoding parameter
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- private$charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Generate a temporary file to store the intermediate HTML output to
      interLoc <- paste(tempfile(), ".html", sep = "")
      # Knit the markdown file to HTML
      rmarkdown::render(input = inFileLocation, output_file = interLoc, encoding = inFileEncoding, output_format = "html_document", quiet = TRUE)
      # Retrieve all the EML metadata from the rendered HTML
      self$importFromLivingNorwayHTML(fileLocation = interLoc, fileEncoding = inFileEncoding, ...)
      # Delete the temporary file
      unlink(interLoc)
      invisible(self)
    },
    # ====== 1.3. Import metadata from an EML file ======
    #' @description
    #' Retrieve metadata information from an Ecological Metadata Language (EML) file
    #' @param fileLocation A \code{character} scalar containing the location of the
    #' RMD file
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @export
    importFromEML = function(fileLocation, fileEncoding = "") {
      # Process the file encoding parameter
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- private$charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      private$xmlContent <- xml2::read_xml(inFileLocation, inFileEncoding)
      # Validate the EML according to the schema
      if(!private$validateEML(fileEncoding = inFileEncoding)) {
        stop("EML is not valid according to the selected EML schema")
      }
      invisible(self)
    },
    # ====== 1.4. Import metadata from a Darwin Core archive ======
    #' @description
    #' Retrieve metadata information from a Darwin Core archive file
    #' @param fileLocation A \code{character} scalar containing the location of the
    #' RMD file
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @export
    importFromDwCArchive = function(fileLocation, fileEncoding = "") {
      # Process the file encoding parameter
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- private$charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Create a temporary directory to store the contents of the unzipped file
      tempLoc <- file.path(tempdir(), "LNimportDir")
      if(dir.exists(tempLoc)) {
        unlink(tempLoc, recursive = TRUE)
      }
      dir.create(tempLoc, recursive = TRUE)
      # Unzip the contents of the Darwin core archive
      zip::unzip(inFileLocation, NULL, exdir = tempLoc)
      # Test to see whether the meta file exists
      if(!file.exists(file.path(tempLoc, "meta.xml"))) {
        stop("error encountered importing Darwin core archive: no meta file in archive")
      }
      # Retrieve the metafile attributes
      metaFileAttributes <- xml2::xml_attrs(xml2::read_xml(file.path(tempLoc, "meta.xml"), inFileEncoding))
      metadataLoc <- file.path(tempLoc, "eml.xml")
      if(is.null(names(metaFileAttributes)) || !("metadata" %in% names(metaFileAttributes))) {
        warning("metadata EML file not specified in the meta.xml document: searching for an \'eml.xml\' file instead")
      } else {
        # Read the location of the metadata file from the meta.xml file
        metadataLoc <- file.path(tempLoc, metaFileAttributes["metadata"])
      }
      # Import the metadata information from the metadata file
      self$importFromEML(metadataLoc, inFileEncoding)
      unlink(tempLoc)
      invisible(self)
    },
    # ====== 1.5. Export the metadata as an EML file ======
    #' @description
    #' Export the metadata as an EML XML file
    #' @param fileLocation A \code{character} scalar containing the location to store the EML fiøe
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @export
    exportToEML = function(fileLocation, fileEncoding = "") {
      # Process the file encoding parameter
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- private$charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      xml2::write_xml(private$xmlContent, inFileLocation, encoding = inFileEncoding)
    },
    # ====== 1.6. Initialise the metadata object ======
    #' @description
    #' Initialise a metadata object from an import file
    #' @param fileLocation A \code{character} scalar containing the location of the import file
    #' @param fileEncoding A \code{character} string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @param fileType A \code{character} scalar stating the type of the file.  File type can be either
    #' \code{"rmarkdown"}, \code{"html"}, \code{"eml"}, or \code{"darwincore"}. If \code{NA} then the
    #' file type will be determined from the file extension
    #' @param ... A set of arguments to be passed to the \code{\link[xml2::xml_new_root]{xml_new_root}}
    #' function to define namespace information.  The values returned by the helper function
    #' \code{\link[getDefaultEMLDefinitionInfo]{getDefaultEMLDefinitionInfo}} will be used unless
    #' overwritten by user input here.  In addition, arguments required by the EML standard but
    #' not provided here (such as the \link[https://ediorg.github.io/data-package-best-practices/EMLmetadata/Root-element.html]{packageID}
    #' attribute) will be otherwise auto-generated
    #' @export
    initialize = function(fileLocation, fileEncoding = "", fileType = NA, ...) {
      # Process the file encoding parameter
      inFileEncoding <- private$charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- private$charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Process the file type
      inFileType <- private$charSanityCheck(fileType, "fileType", NA)
      if(is.na(inFileType)) {
        inFileType <- switch(tolower(gsub("^.*\\.", "", inFileLocation, perl = TRUE)),
          rmd = "rmarkdown", md = "rmarkdown",
          html = "html",
          xml = "eml", eml = "eml",
          zip = "darwincore",
          NA)
      }
      inFileType <- tolower(inFileType)
      if(inFileType == "rmarkdown") {
        # Import file is an R markdown file
        self$importFromLivingNorwayRMD(fileLocation = inFileLocation, fileEncoding = inFileEncoding, ...)
      } else if(inFileType == "html") {
        # Import file is a HTML file
        self$importFromLivingNorwayHTML(fileLocation = inFileLocation, fileEncoding = inFileEncoding, ...)
      } else if(inFileType == "eml") {
        # Import file is an EML file
        self$importFromEML(inFileLocation, inFileEncoding)
      } else if(inFileType == "darwincore") {
        # Import file is a Darwin Core archive
        self$importFromDwCArchive(inFileLocation, inFileEncoding)
      } else {
        stop("error encountered importing metadata: unknown import file type")
      }
      invisible(self)
    },
    # ====== 1.7. Print the metadata information to the console ======
    #' @description
    #' Print the metadata information to the console
    print = function() {
      titleText <- self$getTitle()
      creatorInfo <- self$getCreatorInfo()
      abstractText <- self$getAbstract()
      if(!is.na(titleText)) {
        # Print the title of the dataset
        cat("Title: ", titleText, "\n", sep = "")
      }
      # Print the names and affiliations of the dataset creators
      if(length(creatorInfo) > 0) {
        cat("Creator")
        if(length(creatorInfo) > 1) {
          cat("s")
        }
        cat(": ")
        cat(paste(sapply(X = creatorInfo, FUN = function(curCreator) {
          outText <- ""
          if("individualName" %in% names(curCreator)) {
            # Creator is an individual
            if("givenName" %in% names(curCreator$individualName)) {
              outText <- paste(outText, curCreator$individualName$givenName, " ", sep = "")
            }
            if("surName" %in% names(curCreator$individualName)) {
              outText <- paste(outText, curCreator$individualName$surName, sep = "")
            }
            if("organizationName" %in% names(curCreator)) {
              outText <- paste(outText, " (", curCreator$organizationName, ")", sep = "")
            }
          } else if("organizationName" %in% names(curCreator)) {
            # Creator is an organization
            outText <- paste(outText, curCreator$organizationName, sep = "")
          }
          outText
        }), collapse = ", "), "\n", sep = "")
      }
      # Print the abstract of the dataset
      if(!is.na(abstractText)) {
        cat("Abstract: ", abstractText, "\n", sep = "")
      }
    },
    # ====== 1.8. Retrieve the title of the dataset ======
    #' @description
    #' Retrieve the title of the dataset
    #' @param lang A \code{character} scalar that specifies the language of the title to return.  This
    #' is useful when the title has multiple translations in the metadata
    #' @export
    getTitle = function(lang = NA) {
      outValue <- NA
      if(!is.null(private$xmlContent)) {
        # Retrieve all the "title" nodes that are of the specified language type
        titleNodes <- private$retrieveLangNodes("//dataset/title", lang)
        titleNames <- ""
        if(!is.null(titleNodes)) {
          # Retrieve the text of the respective elements
          titleNames <- xml2::xml_text(xml2::xml_find_all(titleNodes, "text()"), trim = TRUE)
        }
        # Remove any empty strings
        titleNames <- titleNames[!is.na(titleNames) && titleNames != ""]
        if(length(titleNames) > 0) {
          outValue <- paste(titleNames, collapse = " ")
        }
      }
      outValue
    },
    # ====== 1.9. Retrieve the information of the dataset creators ======
    #' @description
    #' Retrieve the information of the dataset creators
    #' @param lang A \code{character} scalar that specifies the language of the elements to return.  This
    #' is useful when the elements have multiple translations in the metadata
    #' @export
    getCreatorInfo = function(lang = NA) {
      # Helper function to retrieve information from nodeset
      retrieveAsList <- function(curNode, lang) {
        outVal <- list()
        # Retrieve the output node
        langNode <- private$retrieveLangNodes(xml2::xml_path(curNode), lang)
        # Get the children of the current node
        curChildren <- xml2::xml_children(langNode[[1]])
        if(length(curChildren) > 0) {
          # If there are children then call the function recursively
          outVal <- setNames(lapply(X = curChildren, FUN = retrieveAsList, lang = lang), xml2::xml_name(curChildren))
        } else {
          # Otherwise return the text associated with the element (after tidying up whitespace)
          outVal <- xml2::xml_text(curNode, trim = TRUE)
          outVal <- outVal[!is.na(outVal) && outVal != ""]
          outVal <- paste(outVal, collapse = " ")
        }
        outVal
      }
      outValue <- list()
      if(!is.null(private$xmlContent)) {
        # Retrieve all the creator nodes
        creatorNodes <- xml2::xml_find_all(private$xmlContent, "//dataset/creator")
        if(length(creatorNodes) > 0) {
          outValue <- lapply(X = creatorNodes, FUN = retrieveAsList, lang = lang)
        }
      }
      outValue
    },
    # ====== 1.10. Retrieve the abstract of the dataset ======
    #' @description
    #' Retrieve the dataset abstract
    #' @param lang A \code{character} scalar that specifies the language of the elements to return.  This
    #' is useful when the elements have multiple translations in the metadata
    #' @export
    getAbstract = function(lang = NA) {
      outValue <- NA
      if(!is.null(private$xmlContent)) {
        # Retrieve all the "abstract" nodes that are of the specified language type
        abstractNodes <- private$retrieveLangNodes("//dataset/abstract", lang)
       absText <- ""
        if(!is.null(abstractNodes)) {
          # Retrieve the text of the respective elements
          absText <- xml2::xml_text(abstractNodes, trim = TRUE)
        }
        # Remove any empty strings
        absText <- absText[!is.na(absText) && absText != ""]
        if(length(absText) > 0) {
          outValue <- paste(absText, collapse = "\n\n")
        }
      }
      outValue
    },
    # ====== 1.11. Retrieve the coverage of the dataset ======
    #' @description
    #' Retrieve the dataset coverage
    #' @param lang A \code{character} scalar that specifies the language of the elements to return.  This
    #' is useful when the elements have multiple translations in the metadata
    #' @export

getCoverage = function(lang = NA) {
  outValue <- NA
  if(!is.null(private$xmlContent)) {
    # Retrieve all the "abstract" nodes that are of the specified language type
    coverageNodes <- private$retrieveLangNodes("//dataset/coverage", lang)
    coverageText <- ""
    if(!is.null(coverageNodes)) {
      # Retrieve the text of the respective elements
      coverageText <- xml2::xml_text(coverageNodes, trim = TRUE)
    }
    # Remove any empty strings
    coverageText <- coverageText[!is.na(coverageText) && coverageText != ""]
    if(length(coverageText) > 0) {
      outValue <- paste(coverageText, collapse = "\n\n")
    }
  }
  outValue
}
)
)


# ------ 2. INITIALISATION FUNCTION ------

#' Initialise a metadata object from an import file
#' @param fileLocation A \code{character} scalar containing the location of the import file
#' @param fileEncoding A \code{character} string. If non-empty, declares the encoding to be used on a file so the
#' character data can be re-encoded as they are written
#' @param fileType A \code{character} scalar stating the type of the file.  File type can be either
#' \code{"rmarkdown"}, \code{"html"}, \code{"eml"}, or \code{"darwincore"}. If \code{NA} then the
#' file type will be determined from the file extension
#' @param ... A set of arguments to be passed to the \code{\link[xml2::xml_new_root]{xml_new_root}}
#' function to define namespace information.  The values returned by the helper function
#' \code{\link[getDefaultEMLDefinitionInfo]{getDefaultEMLDefinitionInfo}} will be used unless
#' overwritten by user input here.  In addition, arguments required by the EML standard but
#' not provided here (such as the \link[https://ediorg.github.io/data-package-best-practices/EMLmetadata/Root-element.html]{packageID}
#' attribute) will be otherwise auto-generated
#' @return A new \code{DwCMetadata} object
#' @seealso \code{\link[DwCMetadata]{DwCMetadata}}
#' @export
initializeDwCMetadata <- function(fileLocation, fileEncoding = "", fileType = NA, ...) {
  DwCMetadata$new(fileLocation = fileLocation, fileEncoding = fileEncoding, fileType = fileType, ...)
}

# ------ 3. EML HANDLING FUNCTIONS ------

# ====== 3.1. Retrieve EML definition information ======
#' Retrieve the default namespace definition information used for EML document
#' initialisation
#' @return A \code{character} vector containing named elements that will be added to the
#' call of the \code{\link[xml2::xml_new_root]{}} function when initialising new
#' EML documents from Living Norway metadata documents
#' @export
getDefaultEMLDefinitionInfo <- function() {
  unlist(list(
    "xmlns:eml" = "https://eml.ecoinformatics.org/eml-2.2.0",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xmlns:stmml" = "http://www.xml-cml.org/schema/stmml-1.1",
    "xsi:schemaLocation" = "https://eml.ecoinformatics.org/eml-2.2.0 xsd/eml.xsd",
    "xml:lang" = "eng",
    "system" = "undefined"
  ))
}
