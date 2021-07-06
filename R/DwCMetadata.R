# ------ 1. METADATA HANDLING CLASS ------
#' R6 class that represents metadata in DwC
#'
#' The \code{DwCMetadata} class exists as a handling class for the encapsulation and
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
    # A character vector containing the entire unprocessed longform metadata
    longForm = character()
  ),
  public = list(
    # ====== 1.2. Import metadata from a Living Norway HTML file ======
    #' @description
    #' Retrieve metadata information from a HTML file that has been created with a
    #' Living Norway HTML tag schema
    #' @param fileLocation A \code{character} scalar containing the location of the
    #' HTML file
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    importFromLivingNorwayHTML = function(fileLocation, fileEncoding = "") {
      # Helper function for sanity checking
      charSanityCheck <- function(inVal, paramName, defaultValue) {
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
      }
      # Process the file encoding parameter
      inFileEncoding <- charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Import the HTML file into an XML object
      inXML <- tryCatch(read_html(as.character(inFileLocation)), error = function(err) {
        stop("error importing HTML file: ", err)
      })
      # Search the document for any span tags that have the "LNmetadata" class
      metadataNodes <- xml_find_all(inXML, "//span[@class=\"LNmetadata\"]")
      # Retrieve the full formatted IDs of each of the metadata tags
      fullIDs <- t(sapply(X = metadataNodes, FUN = function(curNode) {
        # Retrieve the components of the ID
        idComponents <- strsplit(xml_attr(curNode, "id"), "_", fixed = TRUE)[[1]]
        # Test to ensure that the ID is formatted correctly
        if(length(idComponents) < 2) {
          stop("error encountered processing metadata ID codes: invalid ID format")
        }
        # Initialise an output vector with the ID code formatted to allow easier traversal
        outVector <- setNames(c(gsub("^LN", "", idComponents[1], perl = TRUE), idComponents[2], ifelse(length(idComponents) > 2, idComponents[3], NA), NA), c("type", "id", "parent", "text"))
        if(is.na(outVector[3])) {
          # Node does not have a parent specified - check the nestedness of the tag to see if it does have a parent
          parIDs <- sapply(X = xml_parents(curNode), FUN = function(curParNode) {
            outVal <- NA
            if(xml_name(curParNode) == "span" && xml_attr(curParNode, "class") == "LNmetadata") {
              # Retrieve the components of the ID of the parent node if it is of the correct node type
              idComponents <- strsplit(xml_attr(curParNode, "id"), "_", fixed = TRUE)[[1]]
              # Test to ensure that the ID is formatted correctly
              if(length(idComponents) < 2) {
                stop("error encountered processing metadata ID codes: invalid ID format")
              }
              outVal <- idComponents[2]
            }
            outVal
          })
          # If there are valid metadat parents then set the nearest in the hierarchy to be the parent ID
          if(any(!is.na(parIDs))) {
            outVector[3] <- parIDs[!is.na(parIDs)][1]
          }
        }
        nodeText <- xml_text(curNode)
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
      # Generate an EML structure
      emlOut <- xml_new_root("eml:eml",
        packageId = "eml.1.1",
        system = "knb",
        "xmlns:eml"="eml://ecoinformatics.org/eml-2.1.1",
        "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance",
        "xsi:schemaLocation"="eml://ecoinformatics.org/eml-2.1.1 eml.xsd",
        # TODO: add language attribute options (use xml:lang attribute to set the default language of the document)
        .encoding = inFileEncoding)
      # Create the base set of nodes
      nodeList <- apply(X = fullIDs[is.na(fullIDs[, 3]), , drop = FALSE], FUN = function(curRow, emlOut) {
        outNode <- xml_add_child(emlOut, curRow[1], id = curRow[2])
        if(!is.na(curRow[4])) {
          xml_text(outNode) <- curRow[4]
        }
        outNode
      }, emlOut = emlOut, MARGIN = 1)
      # Function to populate children of a node from the ID list
      makeChildren <- function(inNode, fullIDs) {
        # For each entry in the ID list that has a parent ID that matches the ID of the current node then add a child
        idMatch <- xml_attr(inNode, "id") == fullIDs[, 3]
        if(any(idMatch)) {
          apply(X = fullIDs[idMatch, , drop = FALSE], FUN = function(curRow, fullIDs, inNode) {
            # Add a child node
            outNode <- xml_add_child(inNode, curRow[1], id = curRow[2])
            if(!is.na(curRow[4])) {
              xml_text(outNode) <- curRow[4]
            }
            # Call the function recursively to populate any children of the new created child node
            makeChildren(outNode, fullIDs)
            outNode
          }, MARGIN = 1, fullIDs = fullIDs, inNode = inNode)
        }
        inNode
      }
      private$xmlContent <- lapply(X = nodeList, FUN = makeChildren, fullIDs = fullIDs[!is.na(fullIDs[, 3]), , drop = FALSE])
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
    importFromLivingNorwayRMD = function(fileLocation, fileEncoding = "") {
      # Helper function for sanity checking
      charSanityCheck <- function(inVal, paramName, defaultValue) {
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
      }
      # Process the file encoding parameter
      inFileEncoding <- charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Generate a temporary file to store the intermediate HTML output to
      interLoc <- paste(tempfile(), ".html", sep = "")
      # Knit the markdown file to HTML
      rmarkdown::render(input = inFileLocation, output_file = interLoc, encoding = inFileEncoding, output_format = "html_document", quiet = TRUE)
      # Retrieve all the EML metadata from the rendered HTML
      self$importFromLivingNorwayHTML(interLoc, inFileEncoding)
      # Delete the temporary file
      unlink(interLoc)
      invisible(self)
    },

    # ====== 1.2. Import metadata from an eml file ======
    #' @description
    #' Retrieve metadata information from an eml file that has been created externally (e.g. from GBIF)
    #' @param fileLocation A \code{character} scalar containing the location of the
    #' eml.xml file
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written

    importFromLivingNorwayEML= function(fileLocation, fileEncoding = "") {
      # Helper function for sanity checking
      charSanityCheck <- function(inVal, paramName, defaultValue) {
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
      }
      # Process the file encoding parameter
      inFileEncoding <- charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Retrieve all the EML metadata from file locatio
      private$xmlContent<- xml2::read_xml(fileLocation)
      invisible(self)
    },


    # ====== 1.3. Export the metadata as an EML file ======
    #' @description
    #' Export the metadata as an EML XML file
    #' @param fileLocation A \code{character} scalar containing the location to store the EML fiøe
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    exportToEML = function(fileLocation, fileEncoding = "") {
      # Helper function for sanity checking
      charSanityCheck <- function(inVal, paramName, defaultValue) {
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
      }
      # Process the file encoding parameter
      inFileEncoding <- charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      write_xml(private$xmlContent, inFileLocation, encoding = inFileEncoding)
    },
    # ====== 1.4. Initialise the metadata object ======
    #' @description
    #' Initialise a metadata object from an import file
    #' @param fileLocation A \code{character} scalar containing the location of the import file
    #' @param fileEncoding A \code{character} string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    #' @param fileType A \code{character} scalar stating the type of the file.  File type can be either
    #' \code{"rmarkdown"}, \code{"html"}, \code{"eml"}, or \code{"darwincore"}. If \code{NA} then the
    #' file type will be determined from the file extension
    initialize = function(fileLocation, fileEncoding = "", fileType = NA) {
      # Helper function for sanity checking
      charSanityCheck <- function(inVal, paramName, defaultValue) {
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
      }
      # Process the file encoding parameter
      inFileEncoding <- charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # Process the file location parameter
      inFileLocation <- charSanityCheck(fileLocation, "fileLocation", NA)
      if(is.na(inFileLocation)) {
        stop("error encountered processing fileLocation parameter: invalid parameter value (NA or vector is length zero)")
      }
      # Process the file type
      inFileType <- charSanityCheck(fileType, "fileType", NA)
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
        self$importFromLivingNorwayRMD(inFileLocation, inFileEncoding)
      } else if(inFileType == "html") {
        # Import file is a HTML file
        self$importFromLivingNorwayHTML(inFileLocation, inFileEncoding)
      } else if(inFileType == "eml") {
        # Import file is a eml
        self$importFromLivingNorwayEML(inFileLocation, inFileEncoding)
      } else {
        stop("error encountered importing metadata: unknown import file type")
      }
      invisible(self)
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
#' @return A new \code{DwCMetadata} object
#' @seealso \code{\link[DwCMetadata]{DwCMetadata}}
initializeDwCMetadata <- function(fileLocation, fileEncoding = "", fileType = NA) {
  DwCMetadata$new(fileLocation = fileLocation, fileEncoding = fileEncoding, fileType = fileType)
}
