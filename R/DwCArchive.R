# ------ 1. DARWIN CORE ARCHIVE CLASS ------
#' R6 DwC archive class
#' @description R6 class representing a complete data structure for a Darwin Core archive
#'
#' @details The \code{DwcArchive} class serves a base class to all Darwin Core archive file types. This class supports
#' all kinds of Darwin Core archive files but may miss some of the specialised functionality of the more specialist
#' classes. See \url{https://dwc.tdwg.org/text/}{the Darwin core archive guide} for more information on the
#' structure of Darwin core archive files.
#' @importFrom R6 R6Class
#' @export
#' @format \code{\link{R6Class}} object.
DwCArchive <- R6Class("DwCArchive",
  # ====== 1.1. Define private members of the archive class ======
  private = list(
    # A DwCGeneric object (or a derived class) that contains the core object type (GBIF current supports)
    coreObject = NULL,
    # A list of DwCGeneric objects (or a derived class) that contains the extensions
    extObjects = list(),
    # A DwCMetadata object containing the metadata specification
    metadata = NULL,
    # ====== 1.2. Generate a metafile XML object ======
    generateMetafileXML = function(sep = ",", eol = "\n", na = "NA", fileEncoding = "", emlLocation = "eml.xml") {
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
      inEMLLocation <- charSanityCheck(emlLocation, "emlLocation", "eml.xml")
      # ------ 1.2.2. Create a new XML document and root node ------
      # Create a archive base node
      xmlOutput <- xml_new_root(
        .value = "archive",
        xmlns = "http://rs.tdwg.org/dwc/text/",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xmlns:xs" = "http://www.w3.org/2001/XMLSchema",
        "xsi:schemaLocation" = "http://rs.tdwg.org/dwc/text/ http://rs.tdwg.org/dwc/text/tdwg_dwc_text.xsd",
        metadata = iNEMLLocation,
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
        xml_add_child(xml_add_child(childNode, .value = "files"), .value = "location", paste(inObject$getTableName(), ".txt", sep = ""))
        # Create the ID elements of the child node
        xml_add_child(childNode, .value = ifelse(obType == "core", "id", "coreid"), index = as.character(inObject$getIDIndex() - 1))
        # Create the field elements of the child node
        outMapFrame <- inObject$getTermMapping()
        outMapFrame <- outMapFrame[!is.na(outMapFrame$columnIndex), ]
        apply(X = cbind(row.names(outMapFrame), as.matrix(outMapFrame)), FUN = function(curRow, childNode, associatedTerms) {
          # Retrieve the current term
          curTerm <- associatedTerms[[curRow[1]]]
          if(length(curTerm$getVocabularyURI()) <= 0 || is.na(curTerm$getVocabularyURI()) || curTerm$getVocabularyURI() == "") {
            xml_add_child(childNode, .value = "field", index = as.character(as.integer(curRow[2]) - 1), term = curTerm$getTermIRI())
          } else {
            xml_add_child(childNode, .value = "field", index = as.character(as.integer(curRow[2]) - 1), term = curTerm$getTermIRI(), vocabulary = curTerm$getVocabularyURI())
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
      cat("Table name: ", tabToPrint$getTableName(), " | ID column: ", tabToPrint$getIDIndex(),
        ifelse(is.na(tabToPrint$getIDName()), "", paste(" - \"", tabToPrint$getIDName(), "\"", sep = "")),
        " | Table class: ", tabToPrint$getTableTermName(), "\n", sep = "")
      tabTermMap <- tabToPrint$getTermMapping()
      print(tabTermMap[!is.na(tabTermMap$columnIndex), ])
      print(head(tabToPrint$exportAsDataFrame()))
    },
    # ====== 1.4. Function to initiate the archive file from a Darwin core archive file ======
    importFromDwCArchive = function(location, fileEncoding = "") {
      inFileEncoding <- ""
      if(!is.null(fileEncoding)) {
        # Sanity check the file encoding parameter
        inFileEncoding <- tryCatch(as.character(fileEncoding), error = function(err) {
          stop("error encountered during the processing of the file encoding parameter: ", err)
        })
        if(length(inFileEncoding) <= 0) {
          inFileEncoding <- ""
        } else if(length(inFileEncoding) > 1) {
          warning("file encoding parameter has a length greater than one: only the first element will be used")
          inFileEncoding <- inFileEncoding[1]
        }
        if(is.na(inFileEncoding)) {
          inFileEncoding <- ""
        }
      }
      if(inFileEncoding == "") {
        inFileEncoding <- localeToCharset(Sys.getlocale("LC_CTYPE"))
      }
      # Sanity check the location parameter
      inLocation <- tryCatch(as.character(location), error = function(err) {
        stop("error encountered during the processing of the Darwin core archive location: ", err)
      })
      if(length(inLocation) <= 0) {
        stop("error encountered during the processing of the Darwin core archive location: vector has length zero")
      } else if(length(inLocation) > 1) {
        warning("Darwin core archive location parameter has a length greater than one: only the first element will be used")
        inLocation <- inLocation[1]
      }
      if(is.na(inLocation)) {
        stop("error encountered during the processing of the Darwin core archive location: parameter is NA")
      }
      # Create a temporary directory to store the contents of the unzipped file
      tempLoc <- file.path(tempdir(), "LNimportDir")
      if(dir.exists(tempLoc)) {
        unlist(tempLoc, recursive = TRUE)
      }
      dir.create(tempLoc, recursive = TRUE)
      # Unzip the contents of the Darwin core archive
      unzip(inLocation, NULL, exdir = tempLoc)
      # Test to see whether the meta file exists
      if(!file.exists(file.path(tempLoc, "meta.xml"))) {
        stop("error encountered importing Darwin core archive: no meta file in archive")
      }
      metaFileContents <- read_xml(file.path(tempLoc, "meta.xml"), inFileEncoding)
      # Retrieve the metafile attributes
      metaFileAttributes <- xml_attrs(metaFileContents)
      metadataLoc <- file.path(tempLoc, "eml.xml")
      if(!is.null(names(metaFileAttributes)) && !("metadata" %in% names(metaFileAttributes))) {
        warning("metadata EML file not specified in the meta.xml document: searching for an \'eml.xml\' file instead")
      } else {
        # Read the location of the metadata file from the meta.xml file
        metadataLoc <- file.path(tempLoc, metaFileAttributes["metadata"])
      }
      private$metadata <- initializeDwCMetadata(fileLocation = metadataLoc, fileEncoding = inFileEncoding, fileType = "eml")
      # Make a list of augmented data tables
      fileList <- lapply(X = xml_children(metaFileContents), FUN = function(curChild, tempLoc) {
        # Function to return special characters
        returnSpecialChars <- function(inChar) {
          gsub("\\t", "\t", gsub("\\r", "\r", gsub("\\n", "\n", inChar, fixed = TRUE), fixed = TRUE), fixed = TRUE)
        }
        # Retrieve the relevant attributes from the metafile
        qualName <- xml_attr(curChild, "rowType")
        if(is.na(qualName)) {
          stop("error encountered importing Darwin core archive: unspecified table type")
        }
        # Get environment associated with the type
        classEnv <- GBIFClassLookup(qualName)[[1]]
        if(is.null(classEnv)) {
          stop("error encountered importing Darwin core archive: unknown class type specified in meta file")
        }
        # Retrieve the information relating to the formatting of the data files
        fieldTerm <- xml_attr(curChild, "fieldsTerminatedBy")
        if(is.na(fieldTerm)) {
          fieldTerm <- ","
        } else {
          fieldTerm <- returnSpecialChars(fieldTerm)
        }
        lineTerm <- xml_attr(curChild, "linesTerminatedBy")
        if(is.na(lineTerm)) {
          lineTerm <- "\n"
        } else {
          lineTerm <- returnSpecialChars(lineTerm)
        }
        fieldEnc <- xml_attr(curChild, "fieldsEnclosedBy")
        if(is.na(fieldEnc)) {
          fieldEnc <- "\""
        } else {
          fieldEnc <- returnSpecialChars(fieldEnc)
        }
        fileEnc <- xml_attr(curChild, "encoding")
        if(is.na(fileEnc)) {
          fileEnc <- "UTF-8"
        }
        headIgnore <- xml_attr(curChild, "ignoreHeaderLines")
        if(is.na(headIgnore)) {
          headIgnore <- 0
        } else {
          headIgnore <- as.integer(headIgnore)
        }
        dateFormat <- xml_attr(curChild, "dateFormat")
        if(is.na(dateFormat)) {
          dateFormat <- "YYYY-MM-DD"
        }
        # Find the location of any file names in the current node
        tableName <- unlist(lapply(X = xml_children(curChild), FUN = function(curNode) {
          filePaths <- c()
          if(xml_name(curNode) == "files") {
            filePaths <- sapply(X = xml_children(curNode), FUN = function(fileNode) {
              xml_text(fileNode)
            })
          }
          filePaths
        }))
        # Sanity check the file input
        if(length(tableName) <= 0) {
          stop("error encountered importing Darwin core archive: no file specified for core or extension table in meta XML specification")
        } else if(length(tableName) > 1) {
          stop("error encountered importing Darwin core archive: multiple files specified for core or extension table in meta XML specification")
        }
        fileName <- file.path(tempLoc, tableName)
        if(!file.exists(fileName)) {
          stop("error encountered importing Darwin core archive: specified file for core or extension table does not exist in the archive")
        }
        # Sanity check the node type
        fileType <- xml_name(curChild)
        if(!(fileType %in% c("core", "extension"))) {
          stop("error encountered importing Darwin core archive: file type is not core or extension in meta XML specifiction")
        }
        # Retrieve the index of the id column (if it exists)
        idIndex <- unlist(lapply(X = xml_children(curChild), FUN = function(curNode, idText) {
          outIndex <- c()
          if(xml_name(curNode) == idText) {
            outIndex <- as.integer(xml_attr(curNode, "index")) + 1
          }
          outIndex
        }, idText = ifelse(fileType == "core", "id", "coreid")))
        # Sanity check the ID index
        if(length(idIndex) <= 0) {
          idIndex <- NA
        } else if(length(idIndex) > 1) {
          stop("error encountered importing Darwin core archive: multiple ID column specifications in meta XML specification")
        }
        # Retrieve the set of Darwin core mapped terms
        mappedTerms <- lapply(X = xml_children(curChild), FUN = function(curNode) {
          outMap <- NULL
          if(xml_name(curNode) == "field") {
            # Retrieve the attributes associated with the current node
            outMap <- list(
              term = xml_attr(curNode, "term"),
              default = xml_attr(curNode, "default"),
              index = as.integer(xml_attr(curNode, "index")) + 1,
              vocabulary = xml_attr(curNode, "vocabulary"),
              shortName = NA
            )
            # Sanity check the term attribute
            if(is.na(outMap$term)) {
              stop("error encountered importing Darwin core archive: term attribute not set in a field tag")
            } else {
              # Retrieve the short name of the term
              outMap$shortName <- gsub("^.*[\\/\\:]", "", outMap$term, perl = TRUE)
            }
          }
          outMap
        })
        mappedTerms <- mappedTerms[!sapply(X = mappedTerms, FUN = is.null)]
        # Ensure that the proper line delimiter is used
        allText <- gsub(lineTerm, "\n", paste(readLines(fileName, encoding = fileEnc), collapse = "\n"), fixed = TRUE)
        # Import the data from the file location
        inTableData <- read.table(text = allText, header = FALSE, skip = as.integer(headIgnore), sep = fieldTerm, quote = fieldEnc, na.strings = "", fileEncoding = fileEnc, stringsAsFactors = FALSE)
        if(as.integer(headIgnore) == 1) {
          # If only one line has been ignored at the start of the file then check to see whether those entries can be coerced into column names
          possCols <- read.table(text = allText, header = FALSE, skip = 0, nrows = 1, sep = fieldTerm, quote = fieldEnc, na.strings = "", fileEncoding = fileEnc, stringsAsFactors = FALSE)
          possCols <- as.character(as.matrix(possCols)[1, ])
          if(length(possCols) == ncol(inTableData) && !any(duplicated(possCols))) {
            colnames(inTableData) <- possCols
          }
        }
        for(curIndex in 1:length(mappedTerms)) {
          if(is.na(mappedTerms[[curIndex]]$index)) {
            # An index attribute is not set so check to see if there is a default set
            if(is.na(mappedTerms[[curIndex]]$default)) {
              stop("error encountered importing Darwin core archive: index attribute is missing in a field tag with no default value")
            } else {
              # If a default has been set then pad the input data accordingly so that the extra information can be imported
              # whilst maintaining R's data frame structure
              tempFrame <- data.frame(inCol = rep(mappedTerms[[curIndex]]$default, nrow(inTableData)))
              colnames(tempFrame) <- mappedTerms[[curIndex]]$shortName
              inTableData <- cbind(inTableData, tempFrame)
              mappedTerms[[curIndex]]$index <- ncol(inTableData)
            }
          }
        }
        # Make a list of mapped parameters
        mappedParams <- as.list(setNames(
          sapply(X = mappedTerms, FUN = function(curTerm) { curTerm$index }),
          sapply(X = mappedTerms, FUN = function(curTerm) { curTerm$shortName })))
        if(is.na(idIndex)) {
          # No ID index given so use the defaults to determine the ID column
          if(fileType == "extension") {
            stop("error encountered importing Darwin core archive: id and coreid attributes must be set when extensions are used")
          }
        } else {
          # Otherwise add the ID column to the list of mapped parameters
          mappedParams <- append(list(idColumnInfo = idIndex), mappedParams)
        }
        # Create the GBIF object
        GBIFTableOb <- do.call(classEnv$new, append(list(
          objectData = inTableData,
          nameAutoMap = FALSE,
          defDateFormat = dateFormat
        ), mappedParams))
        # Set the attributes of the table (extension or core)
        attr(GBIFTableOb, "fileType") <- fileType
        # Remove any file suffixes for the table name
        tableName <- gsub("\\..*$", "", tableName, perl = TRUE)
        GBIFTableOb$setTableName(tableName)
        GBIFTableOb
      }, tempLoc = tempLoc)
      # Retrieve information about which augmented tables are core tables and which are extensions
      outTypes <- sapply(X = fileList, FUN = function(curFile) { attr(curFile, "fileType") })
      # Find the index of the core table
      coreIndex <- which(outTypes == "core")
      if(length(coreIndex) <= 0 || length(coreIndex) > 1) {
        stop("error encountered importing Darwin core archive: there must be exactly one core element in the archive file")
      }
      # Initialise the archive using the imported tables
      self$initialize(fileList[[coreIndex]], fileList[-coreIndex])
      # Remove the temporary directory
      unlist(tempLoc, recursive = TRUE)
      invisible(self)
    }
  ),
  public = list(
    # ====== 1.4. Define an initialization function for the Darwin core archive ======
    #' @description
    #' Create a new \code{DwCAchive} object
    #' @param coreDwC A \code{DwCGeneric} (or derived class) object that represents the
    #' table that corresponds to the 'core' table.  Alternatively, this parameter can be
    #' \code{character} scalar giving the location of the Darwin core archive file to
    #' initialize the object from
    #' @param extDwC A \code{list} of \code{DwCGeneric} (or derived class) objects that represent
    #' the tables used as extension objects in the Darwin Core archive.  If \code{coreDwC}
    #' is a character scalar then \code{extDwC} can also be a character scalar that contains the
    #' default file encodings for the files in the Darwin core archive
    initialize = function(coreDwC, extDwC = NULL) {
      if(is.character(coreDwC) || is.factor(coreDwC)) {
        # If the core object is a character vector then treat it like a file and import the data from it
        private$importFromDwCArchive(coreDwC, extDwC)
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
    #' @param emlLocation The location to store the EML metadata in the Darwin Core archive
    exportAsDwCArchive = function(fileName, quote = TRUE, sep = "\t", eol = "\n", na = "", dec = ".", qmethod = "escape", fileEncoding = "", emlLocation = "eml.xml") {
      # Find the temporary location to place the files temporarily (before compressing them)
      tempLoc <- file.path(tempdir(), "LNexportDir")
      if(dir.exists(tempLoc)) {
        unlink(tempLoc, recursive = TRUE)
      }
      dir.create(tempLoc, recursive = TRUE)
      # Locations of the table files
      coreLoc <- file.path(tempLoc, paste(private$coreObject$getTableName(), "txt", sep = "."))
      extLocs <- c()
      metafileLoc <- file.path(tempLoc, "meta.xml")
      # Write the core table
      private$coreObject$exportTable(coreLoc, FALSE, quote, sep, eol, na, dec, qmethod, fileEncoding)
      if(length(private$extObjects) > 0) {
        # Write any extension tables
        extLocs <- sapply(X = private$extObjects, FUN = function(curOb, quote, sep, eol, na, dec, qmethod, fileEncoding, tempLoc) {
          outLoc <- file.path(tempLoc, paste(curOb$getTableName(), "txt", sep = "."))
          curOb$exportTable(outLoc, FALSE, quote, sep, eol, na, dec, qmethod, fileEncoding)
          outLoc
        }, quote = quote, sep = sep, eol = eol, na = na, dec = dec, qmethod = qmethod, fileEncoding = fileEncoding, tempLoc = tempLoc)
      }
      # Produce the Darwin core meta file
      outXML <- private$generateMetafileXML(sep, eol, na, fileEncoding, emlLocation)
      inFileEncoding <- tryCatch(as.character(fileEncoding), error = function(err) {
        stop("error encountered whilst processing the file encoding parameter: ", err)
      })
      if(length(inFileEncoding) <= 0 || is.na(inFileEncoding) || inFileEncoding == "") {
        # Use the default system file encoding if it is not set by the function
        inFileEncoding <- localeToCharset(Sys.getlocale("LC_CTYPE"))
      } else if(length(inFileEncoding) > 1) {
        warning("file encoding parameter has length greater than one: only the first element will be used")
        inFileEncoding <- inFileEncoding[1]
      }
      write_xml(outXML, metafileLoc, encoding = inFileEncoding)
      # Produce the EML metadata file
      inEMLLocation <- tryCatch(as.character(emlLocation), error = function(err) {
        stop("error encountered whilst processing the EML location parameter: ", err)
      })
      if(length(inEMLLocation) <= 0 || is.na(inEMLLocation) || inEMLLocation == "") {
        # Use the default system file encoding if it is not set by the function
        inEMLLocation <- "eml.xml"
      } else if(length(inEMLLocation) > 1) {
        warning("EML location parameter has length greater than one: only the first element will be used")
        inEMLLocation <- inEMLLocation[1]
      }
      metadataLoc <- file.path(tempLoc, inEMLLocation)
      private$metadata$exportToEML(fileLocation = metadataLoc, fileEncoding = inFileEncoding)
      # Zip all the files together
      zip::zip(fileName, c(coreLoc, extLocs, metafileLoc, metadataLoc), mode = "cherry-pick")
      # Remove the temporary directory
      unlink(tempLoc, recursive = TRUE)
      invisible(self)
    },
    # ====== 1.6. Function to print the object to the console ======
    #' @description
    #' Print the archive information
    print = function() {
      # Display the metadata summary
      cat("METADATA\n\n")
      print(private$metadata)
      cat("\n\n")
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
    },
    # ====== 1.7. Retrieve the number of extensions used in the archive ======
    #' @description
    #' Retrieve the number of extensions used in the archive
    #' @return An \code{integer} scalar containing the number of extensions used in the archive
    getNumberExtensions = function() {
      length(private$extObjects)
    },
    # ====== 1.8. Retrieve the core table ======
    #' @description
    #' Retrieve the core table in the archive
    #' @return An object derived from \code{DwCGeneric} that contains the core table information
    getCoreTable = function() {
      private$coreObject$clone()
    },
    # ====== 1.9. Retrieve an extension object ======
    #' @description
    #' Retrieve an extension table from the archive
    #' @param extIndex Either an \code{integer} vector giving the indeces of the extension tables to
    #' retrieve from the archive or a \code{character} vector giving the names of the tables to
    #' retrieve from the archive.  If this parameter is \code{NULL} then all extension tables are
    #' retrieved
    #' @return A \code{list} of objects derived from \code{DwCGeneric} that contains the extension table
    #' information
    getExtensionTables = function(extIndex = NULL) {
      outIndeces <- extIndex
      outVals <- list()
      if(length(private$extObjects) > 0) {
        if(is.null(outIndeces)) {
          # If NULL is entered then just use indeces for every extension object
          outIndeces <- 1:length(private$extObjects)
        } else if(is.character(outIndeces) || is.factor(outIndeces)) {
          # If a character vector is entered then lookup indeces from the table names
          outIndeces <- sapply(X = as.character(outIndeces), FUN = function(curName, tableNames) {
            outVal <- NA
            if(!is.na(curName)) {
              outVal <- which(curName == tableNames)
              if(length(outVal) <= 0) {
                outVal <- NA
              }
              outVal <- outVal[1]
            }
            outVal
          }, tableNames = sapply(X = private$extObjects, FUN = function(curOb) { curOb$getTableName() }))
        }
        # Use the indeces to find the relevant extension objects
        outVals <- tryCatch(lapply(X = as.integer(outIndeces), FUN = function(curInteger, extObjects) {
          outOb <- NULL
          if(is.na(curInteger) || curInteger <= 0 || curInteger > length(extObjects)) {
            stop("invalid index value given")
          } else {
            outOb <- extObjects[[curInteger]]$clone()
          }
          outOb
        }, extObjects = private$extObjects), error = function(err) {
          stop("error encountered retrieving the extension tables: ", err)
        })
      }
      outVals
    },
    # ====== 1.10. Retrieve the metadata for the archive ======
    #' @description
    #' Retrieve the metadata for the archive
    #' @return A \code{DwCMetadata} object that contains the metadata of the archive
    getMetadata = function() {
      private$metadata$clone()
    }
  )
)

# ------ 2. DARWIN CORE ARCHIVE INITIALIZATION FUNCTION ------
#' Create a new \code{DwCAchive} object
#' @param coreDwC Either a \code{DwCGeneric} (or derived class) object that represents the
#' table that corresponds to the 'core' table.  Alternatively, this parameter can be
#' \code{character} scalar giving the location of the Darwin core archive file to
#' initialize the object from
#' @param extDwC A \code{list} of \code{DwCGeneric} (or derived class) objects that represent
#' the tables used as extension objects in the Darwin Core archive.  If \code{coreDwC}
#' is a character scalar then \code{extDwC} can also be a character scalar that contains the
#' default file encodings for the files in the Darwin core archive
#' @return A new \code{DwCArchive} object
#' @seealso \code{\link[DwCGeneric]{DwCGeneric}}
#' @export
initializeDwCArchive = function(coreDwC, extDwC = NULL) {
  DwCArchive$new(coreDwC = coreDwC, extDwC = extDwC)
}
