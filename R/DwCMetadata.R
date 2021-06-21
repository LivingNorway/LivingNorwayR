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
    xmlContent = NULL
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
      lapply(X = nodeList, FUN = makeChildren, fullIDs = fullIDs[!is.na(fullIDs[, 3]), , drop = FALSE])
      write_xml(emlOut, "C:/Temp/emlOut.xml")
    },
    importFromLivingNorwayRMD = function(fileLocation, fileEncoding) {

    }
  ))

#test<-DwCMetadata$new(metadata = NA)
#test$getMetadata(filepath ="C:/Users/matthew.grainger/Documents/Projects_in_development/Test_the_dataPackage/Rock_ptarmigan/metadata/metadata/metadata.Rmd")

# metadata=NA,
# initialize=function(metadata){
#  self$metadata<-metadata},
# #' read_metadata
# #' read the metadata from a rmarkdown file
# #' @param filepath a filepath to a RMarkdown file
# #' @return Output: text string of yaml information

# getMetadata = function(filepath) {
# x = readr::read_lines(filepath) # read markdown using readlines
# rng = grep("^---$", x)
# rng = rng + c(1, -1)
# x = x[rng[1]:rng[2]]
# names(x) = gsub("(.*):.*", "\\1", x)
# x = gsub(".*: (.*)", "\\1", x)
# return(as.list(x))
# }
