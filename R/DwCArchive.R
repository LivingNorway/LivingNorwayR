# ------ 1. DARWIN CORE ARCHIVE CLASS ------
#' R6 class representing a complete data structure for a Darwin Core archive
#'
#' The \code{DwcArchive} class serves a base class to all Darwin Core archive file types. This class supports
#' all kinds of Darwin Core archive files but may miss some of the specialised functionality of the more specialist
#' classes. See \url{https://dwc.tdwg.org/text/}{the Darwin core archive guide} for more information on the
#' structure of Darwin core archive files.
DwCArchive <- R6Class("DwCArchive",
  # ====== 1.1. Define private members of the archive class ======
  private = list(
    # A LNMetadata Object containing the metadata associated with the archive
    metadataObject = NULL,
    # A DwCGeneric object (or a derived class) that contains the core object type (GBIF current supports)
    coreObject = NULL,
    # A list of DwCGeneric objects (or a derived class) that contains the extensions
    extObjects = list(),
    # ====== 1.2. Generate a metafile XML object ======
    generateMetafileXML = function(sep = ",", eol = "\n", na = "NA", fileEncoding = "") {
      # ------ 1.2.1. Sanity test the inputs ------
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
      # Helper function to handle special characters being provided as delimitation information
      resolveSpecialChars <- function(inVal) {
        gsub("\r", "\\r", gsub("\n", "\\n", gsub("\t", "\\t", inVal, fixed = TRUE), fixed = TRUE), fixed = TRUE)
      }
      inSep <- resolveSpecialChars(charSanityCheck(sep, "sep", ","))
      inEol <- resolveSpecialChars(charSanityCheck(eol, "eol", "\n"))
      inEncoding <- charSanityCheck(fileEncoding, "fileEncoding", localeToCharset(Sys.getlocale("LC_CTYPE")))
      # ------ 1.2.2. Create a new XML document and root node ------
      # Create a archive base node
      xmlOutput <- xml_new_root(
        .value = "archive",
        xmlns = "http://rs.tdwg.org/dwc/text/",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xmlns:xs" = "http://www.w3.org/2001/XMLSchema",
        "xsi:schemaLocation" = "http://rs.tdwg.org/dwc/text/ http://rs.tdwg.org/dwc/text/tdwg_dwc_text.xsd",
        .version = "1.0",
        .encoding = "UTF-8"
      )
      # ------ 1.2.3. Populate child nodes ------
      # Helper function to generate XML for child nodes
      makeXMLNodes <- function(inObject, rootNode, obType, inSep, inEol, inEncoding) {
        # Create the child node of the relevant type (core or extension)
        childNode <- xml_add_child(rootNode, .value = obType,
          rowType = inObject$getDwCTermInfo()$getTermIRI(),
          fieldsTerminatedBy = inSep,
          linesTerminatedBy = inEol,
          fieldsEnclosedBy = "\"",
          encoding = inEncoding,
          ignoreHeaderLines = "1"
        )
        # Create the files elements of the child node
        xml_add_child(xml_add_child(childNode, .value = "files"), .value = "location", paste(inObject$getFileName(), ".txt", sep = ""))
        # Create the ID elements of the child node
        xml_add_child(childNode, .value = ifelse(obType == "core", "id", "coreid"), index = as.character(inObject$getIDIndex()))
        # Create the field elements of the child node
        outMapFrame <- inObject$getTermMapping()
        outMapFrame <- outMapFrame[!is.na(outMapFrame$columnIndex), ]
        apply(X = cbind(row.names(outMapFrame), as.matrix(outMapFrame)), FUN = function(curRow, childNode, associatedTerms) {
          # Retrieve the current term
          curTerm <- associatedTerms[[curRow[1]]]
          if(length(curTerm$getVocabularyURI()) <= 0 || is.na(curTerm$getVocabularyURI()) || curTerm$getVocabularyURI() == "") {
            xml_add_child(childNode, .value = "field", index = curRow[2], term = curTerm$getTermIRI())
          } else {
            xml_add_child(childNode, .value = "field", index = curRow[2], term = curTerm$getTermIRI(), vocabulary = curTerm$getVocabularyURI())
          }
        }, childNode = childNode, associatedTerms = inObject$getAssociatedTerms(), MARGIN = 1)
        childNode
      }
      # Make the core node
      makeXMLNodes(private$coreObject, xmlOutput, "core", inSep, inEol, inEncoding)
      # Make the nodes for the extensions
      if(length(private$extObjects) > 0) {
        lapply(X = private$extObjects, FUN = makeXMLNodes, rootNode = xmlOutput, obType = "extension", inSep = inSep, inEol = inEol, inEncoding = inEncoding)
      }
      xmlOutput
    },
    # ====== 1.3. Print table information ======
    printTableInfo = function(tabToPrint) {
      cat("Table name: ", tabToPrint$getFileName(), "\tID column: ", tabToPrint$getIDIndex(),
        ifelse(is.na(tabToPrint$getIDName()), "", paste(" - \"", tabToPrint$getIDName(), "\"")), "\n", sep = "")
      tabTermMap <- tabToPrint$getTermMapping()
      print(tabTermMap[!is.na(tabTermMap$columnIndex), ])
      print(head(tabToPrint$exportAsDataFrame()))
    },
    # ====== 1.4. Function to initiate the archive file from a Darwin core archive file ======
    importFromDwCArchive = function(location, fileEncoding = "") {
      browser()
      # Create a temporary directory to store the contents of the unzipped file
      tempLoc <- file.path(tempdir(), "LNimportDir")
      if(dir.exists(tempLoc)) {
        unlist(tempLoc, recursive = TRUE)
      }
      dir.create(tempLoc, recursive = TRUE)
      unzip()
      unlist(tempLoc, recursive = TRUE)
    }
  ),
  public = list(
    # ====== 1.4. Define an initialization function for the Darwin core archive ======
    #' @description
    #' Create a new \code{DwCAchive} object
    #' @param coreDwC A \code{DwCGeneric} (or derived class) object that represents the
    #' table that corresponds to the 'core' table
    #' @param extDwC A \code{list} of \code{DwCGeneric} (or derived class) objects that represent
    #' the tables used as extension objects in the Darwin Core archive
    initialize = function(coreDwC, extDwC = NULL) {
      if(is.character(coreDwC) || is.factor(coreDwC)) {
        # If the core object is a character vector then treat it like a file and import the data from it
        private$importFromDwCArchive(as.character(coreDwC))
        if(!is.null(extDwC) && length(inExtDwC) > 0) {
          warning("archive object is initiated from a Darwin core archive folder and so the extension specification parameter is not required and unused")
        }
      } else {
        # Sanity test the core DwC object
        if(isDwCGeneric(coreDwC)) {
          private$coreObject <- coreDwC
        } else {
          stop("error encountered during archive object creation: core class is not a valid object")
        }
        # Sanity test the extension input
        inExtDwC <- NULL
        if(!is.null(extDwC)) {
          inExtDwC <- extDwC
          if(isDwCGeneric(inExtDwC)) {
            inExtDwC <- vector(mode = "list", length = 1)
            inExtDwC[[1]] <- extDwC
          }
          inExtDwC <- tryCatch(as.list(inExtDwC), error = function(err) {
            stop("error encountered during processing of Darwin core archive extension elements: ", err)
          })
        }
        private$extObjects <- list()
        if(!is.null(inExtDwC) && length(inExtDwC) > 0) {
          if(all(sapply(X = inExtDwC, FUN = isDwCGeneric))) {
            private$extObjects <- inExtDwC
          } else {
            stop("error encountered during processing of Darwin core archive extension elements: some list memebers are not valid objects")
          }
        }
      }
      invisible(self)
    },
    # ====== 1.5. Export the archive as a Darwin core archive file ======
    #' @description
    #' Export the archive as a Darwin core archive file
    #' @param fileName A \code{character} string containing the file location of the output Darwin Core Archive.
    #' @param quote A \code{logical} scalar or a \code{numeric} vector. If \code{TRUE}, any character or factor
    #' columns will be surrounded by double quotes. If a \code{numeric} vector, its elements are taken as the
    #' indeces of columns to quote. In both cases, row and column names are quoted if they are written. If \code{FALSE},
    #' nothing is quoted.
    #' @param sep The field seperator stirng. Values within each row are separated by this string.
    #' @param eol The character(s) to print at the end of each line (row).
    #' @param na The string to use for missing values in the data.
    #' @param dec The string to use for decimal points in numeric or complex columns: must be a single character
    #' @param qmethod A character string specifying how to deal with embedded double quote characters when
    #' quoting strings. Must be one of \code{"escape"}, in which case the quote character is escaped in C style by a
    #' backslash, or \code{"double"}, in which case it is doubled
    #' @param fileEncoding A character string. If non-empty, declares the encoding to be used on a file so the
    #' character data can be re-encoded as they are written
    exportAsDwCArchive = function(fileName, quote = TRUE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = "escape", fileEncoding = "") {
      # Find the temporary location to place the files temporarily (before compressing them)
      tempLoc <- file.path(tempdir(), "LNexportDir")
      if(dir.exists(tempLoc)) {
        unlist(tempLoc, recursive = TRUE)
      }
      dir.create(tempLoc, recursive = TRUE)
      # Locations of the table files
      coreLoc <- file.path(tempLoc, paste(private$coreObject$getFileName(), "txt", sep = "."))
      extLocs <- c()
      metafileLoc <- file.path(tempLoc, "meta.xml")
      # Write the core table
      private$coreObject$exportTable(coreLoc, FALSE, quote, sep, eol, na, dec, qmethod, fileEncoding)
      if(length(private$extObjects) > 0) {
        # Write any extension tables
        extLocs <- sapply(X = private$extObjects, FUN = function(curOb, quote, sep, eol, na, dec, qmethod, fileEncoding, tempLoc) {
          outLoc <- file.path(tempLoc, paste(curOb$getFileName(), "txt", sep = "."))
          curOb$exportTable(outLoc, FALSE, quote, sep, eol, na, dec, qmethod, fileEncoding)
          outLoc
        }, quote = quote, sep = sep, eol = eol, na = na, dec = dec, qmethod = qmethod, fileEncoding = fileEncoding, tempLoc = tempLoc)
      }
      # Produce the Darwin core meta file
      outXML <- private$generateMetafileXML(sep, eol, na, fileEncoding)
      inFileEncoding <- fileEncoding
      if(length(inFileEncoding) <= 0 || is.na(inFileEncoding) || inFileEncoding == "") {
        # Use the default system file encoding if it is not set by the function
        inFileEncoding <- localeToCharset(Sys.getlocale("LC_CTYPE"))
      }
      write_xml(outXML, metafileLoc, encoding = inFileEncoding)
      # Zip all the files together
      zip::zip(fileName, c(coreLoc, extLocs, metafileLoc), mode = "cherry-pick")
      # Remove the temporary directory
      unlist(tempLoc, recursive = TRUE)
      invisible(self)
    },
    # ====== 1.6. Function to print the object to the console ======
    #' @description
    #' Print the archive information
    print = function() {
      # Display a summary of the core table
      cat("CORE TABLE\n\n")
      private$printTableInfo(private$coreObject)
      if(length(private$extObjects) > 0) {
        # Display a summary for any extension table
        cat("\n\nEXTENSION TABLES\n")
        lapply(X = private$extObjects, FUN = function(curOb) {
          cat("\n")
          private$printTableInfo(curOb)
          cat("\n")
        })
      }
    }
  )
)
