# ------ 1. GENERIC DARWIN CORE FILE CLASS ------
#' R6 class representing a generic data structure for a Darwin Core archive file
#'
#' The \code{DwcGeneric} class serves a base class to all Darwin Core archive file types. This class supports all kinds of Darwin Core
#' archive files but may miss some of the specialised functionality of the more specialist classes.
#' See \url{https://dwc.tdwg.org/terms/}{the Darwin core reference guide} for more information on Darwin core classes and the terms
#' supported by them.
#'
DwCGeneric <- R6::R6Class("DwCGeneric",
  # ====== 1.1. Define private members of the generic class ======
  private = list(
    # A DwCTerm object containing the term associated with the class
    classTermInfo = NULL,
    # A name for the particular object
    objectName = character(),
    # An integer containing the index of the ID column in the dataset
    idColumnIndex = integer(),
    # An integer vector containing the indeces associated with terms associated with the class (those terms are given in the names attribute of the vector)
    termMapping = integer(),
    # A list of terms associated with the DwC class
    associatedTerms = list(),
    # A data frame containing the data held in the DwC object
    objectData = data.frame(),
    # ---- 1.1.1. Define a function to import the class information ----
    # This is a private function called by other functions (called usually during initialisation of the class)
    setClassInfo = function(classTermInfo, associatedTerms) {
      # Utility function to test if the object is DwCTerm object
      isTermsObject <- function(inVal) {
        any(class(inVal) == "DwCTerm")
      }
      # Sanity check the class definition term
      if(!isTermsObject(classTermInfo)) {
        stop("error encountered during the processing of the class term information: input is not a DwCTerm object")
      }
      private$classTermInfo <- classTermInfo
      # Sanity check the associated terms
      private$associatedTerms <- tryCatch(associatedTerms, error = function(err) {
        stop("error encountered during the processing of the associated terms information: ", err)
      })
      # Ensure that the composite terms are all DwCTerm objects
      if(!all(sapply(X = private$associatedTerms, FUN = isTermsObject))) {
        stop("error encountered during the processing of the associated terms information: one or more associated terms in not a DwcTerm object")
      }
      # Initialise the term mapping vector based on the names of the associated terms
      private$termMapping <- setNames(as.integer(rep(NA, length(private$associatedTerms))), sapply(X = private$associatedTerms, FUN = function(curElement) {
        curElement$getQualifiedName()
      }))
      if(any(duplicated(names(private$termMapping)))) {
        stop("error encountered during the processing of the associated terms informations: duplicated qualified names of terms")
      }
      # Initialise a generic name for the object
      private$objectName <- private$classTermInfo$getTermName()
      # Initialise the data objects
      private$objectData <- data.frame()
      private$idColumnIndex <- integer()
      # Return the object invisibly
      invisible(self)
    },
    # ---- 1.1.2. Define a function to import the data ----
    # This is a private function called by other functions (called usually during initialisation of the class)
    setDataInfo = function(objectData, mappingInfo, idColumnInfo) {
      # Utility function to convert an input value giving a column title into an index
      getColIndex <- function(inName, inColNames) {
        tempName <- inName
        if(!is.character(tempName) && !is.factor(tempName)) {
          tempName <- tryCatch(as.vector(tempName), error = function(err) {
            stop("error encountered during processing of column index: ", err)
          })
        } else {
          tempName <- tryCatch(as.character(tempName), error = function(err) {
            stop("error encountered during processing of column index: ", err)
          })
        }
        if(length(tempName) <= 0) {
          stop("error encountered during procesing of column index: entry has zero length")
        } else if(length(tempName) > 2) {
          warning("more than one column option provided: only the first will be used")
          tempName <- tempName[1]
        }
        if(is.character(inName)) {
          # If the input is a character vector then find the corresponding index
          tempName <- tryCatch(which(inName == as.character(inColNames)), error = function(err) {
            stop("error encountered during processing of column index: ", err)
          })
          if(length(tempName) <= 0) {
            stop("error encountered during processing of column index: name does not correspond to data column")
          } else if(length(tempName) > 2) {
            stop("error encountered during processing of column index: corresponding column name is duplicated")
          }
        }
        # Convert the column index to an integer
        tempName <- tryCatch(as.integer(tempName), error = function(err) {
          stop("error encountered during processing of column index: ", err)
        })
        # Final quality control checks to ensure column indeces are valid
        if(!is.null(inColNames)) {
          stop("error encountered during processing of column index: data object does not have column names")
        } else if(tempName <= 0 || tempName > length(inColNames)) {
          stop("error encountered during processing of column index: index falls outside width of data object")
        }
        tempName
      }
      # Sanity test the input data
      private$objectData <- tryCatch(as.data.frame(objectData), error = function(err) {
        stop("error encountered during import of object data: ", err)
      })
      # Reinitialise the term mapping
      private$termMapping <- setNames(as.integer(rep(NA, length(private$associatedTerms))), sapply(X = private$associatedTerms, FUN = function(curElement) {
        curElement$getQualifiedName()
      }))
      # Sanity test the column mappings
      termMapping <- tryCatch(
        setNames(sapply(X = as.list(mappingInfo), FUN = getColIndex, inColNames = colnames(private$objectData)), names(mappingInfo)),
        error = function(err) {
          stop("error encountered during processing of mapping information: ", err)
        })
      if(is.null(names(termMapping))) {
        stop("error encountered during processing of mapping information: mapping info has no names attribute")
      }
      private$termMapping[sapply(X = names(termMapping), FUN = function(curTerm, possTermNames) {
        matchingIndeces <- unique(c(which(curTerm == possTermNames), which(curTerm == gsub("^.*\\:\\:", "", possTermNames, perl = TRUE))))
        if(length(matchingIndeces) <= 0) {
          stop("error encountered during processing of mapping information: specified term is not one recognised by the class")
        } else if(length(matchingIndeces) > 1) {
          stop("error encountered during processing of mapping information: ")
        }
        matchingIndeces[1]
      }, possTermNames = names(private$termMapping))] <- termMapping
      # Sanity test the ID column entry
      private$idColumnIndex <- getColIndex(idColumnInfo, colnames(private$objectData))
      # Return the object
      invisible(self)
    }
  ),
  public = list(
    # ====== 1.2. Function to set the name of the file in the Darwin core archive ======
    #' @description
    #' Set the name of the file that the data will print to when preparing the Darwin core archive
    #' @param inFileName A character scalar giving the name to give the data file in a Darwin core archive
    setTableName = function(inTableName) {
      private$objectName <- tryCatch(as.character(inTableName), error = function(err) {
        stop("error encountered whilst processing file name: ", err)
      })
      if(length(private$objectName) == 0) {
        private$objectName <- private$classTermInfo$getTermName()
      } else if(length(private$objectName) > 1) {
        warning("entry for file name has more than one entry: only the first will be used")
        private$objectName <- private$objectName[1]
      }
      if(is.na(private$objectName)) {
        private$objectName <- private$classTermInfo$getTermName()
      }
      invisible(self)
    },
    # ====== 1.3. Function to retrieve the name of the file in the Darwin core archive ======
    #' @description
    #' Retrieve the name of the file that the data will print to when preparing the Darwin core archive
    #' @return The name currently set as the file name in the output archive
    getFileName = function() {
      private$objectName
    },
    # ====== 1.4. Import data into the table ====
    #' @description
    #' Import data from a \code{data.frame} into a \code{DwCGeneric} object
    #' @param objectData A \code{data.frame} containing the data to import into the object
    #' @param idColumnInfo Either a \code{character} scalar containing the column name
    #' of \code{objectData} or an \code{integer} scalar giving the index of the column of
    #' \code{objectData} that corresponds to the ID variable
    #' @param ... A named set of parameter corresponding to Darwin core terms associated
    #' with the DwCGeneric class type. Each is either a a \code{character} scalar containing
    #' the column name of \code{objectData} or an \code{integer} scalar giving the index of
    #' the column of \code{objectData} that corresponds to the term
    importDataTable = function(objectData, idColumnInfo, ...) {
      private$setDataInfo(objectData, list(...), idColumnInfo)
      invisible(self)
    },
    # ====== 1.5. Define an initialisation function for the generic class ======
    #' @description
    #' Create a new DwCGeneric object
    #' @param classTermInfo A \code{DwCTerm} object containing the term information for the class
    #' @param associatedTerms A \code{list} of \code{DwCTerm} objects that contain all the terms associated with the class
    #' @param objectData A \code{data.frame} containing the data to import into the object
    #' @param idColumnInfo Either a \code{character} scalar containing the column name
    #' of \code{objectData} or an \code{integer} scalar giving the index of the column of
    #' \code{objectData} that corresponds to the ID variable
    #' @param ... A named set of parameter corresponding to Darwin core terms associated
    #' with the DwCGeneric class type. Each is either a a \code{character} scalar containing
    #' the column name of \code{objectData} or an \code{integer} scalar giving the index of
    #' the column of \code{objectData} that corresponds to the term
    #' @return A new \code{DwCGeneric} object
    #' @seealso \code{\link[DwCTerm]{DwCTerm}}
    initialize = function(classTermInfo, associatedTerms, objectData, idColumnInfo, ...) {
      # Set the class information
      private$setClassInfo(classTermInfo, associatedTerms)
      # Set the data information
      private$setDataInfo(objectData, list(...), idColumnInfo)
      invisible(self)
    },
    # ====== 1.6. Function to get the name of the Darwin core class that the object holds ======
    #' @description
    #' Retrieve the name of the class used in Darwin core
    #' @return A \code{character} scalar contining the name of the Darwin core class
    getDwCClassName = function() {
      private$classTermInfo$getQualifiedName()
      },
    # ====== 1.7. Function to retrieve the term information of the Darwin core class ======
    #' @description
    #' Retrieve the term information of thee Darwin core class of the object
    #' @return A \code{DwCTerm} object containing the term information of the Darwin core class
    #' of the object
    #' @seealso \code{\link[DwCTerm]{DwCTerm}}
    getDwCTermInfo = function() {
      private$classTermInfo
    },
    # ====== 1.8. Function to retrieve the mapping information =======
    #' @description
    #' Retrieve the mapping information of the Darwin core terms associated with the class to the
    #' columns in the data table
    #' @return A \code{data.frame} with one row for each Darwin core term associated with the object's
    #' class with two columns: \code{columnIndex} containing the column index of the associated term in the
    #' table and \code{columnName} containg the name of the column
    getTermMapping = function() {
      data.frame(
        columnIndex = private$termMapping,
        columnName = colnames(private$objectData),
        row.names = names(private$termMapping))
    },
    # ====== 1.9. Function to retrieve the terms associated with the class ======
    #' @description
    #' Retrieve the terms associated with the Darwin core class of the object
    #' @return A \code{list} of \code{DwCTerm} objects containing the term information
    getAssociatedTerms = function() {
      private$associatedTerms
    },
    # ====== 1.10. Function to print the object to the console ======
    #' Print the term information
    print = function() {
      cat(toupper(private$objectName), "CLASS INFORMATION\n")
      private$classTermInfo$print()
      cat("MAPPED DARWIN CORE TERMS\n")
      outMapFrame <- getTermMapping()
      print(outMapFrame[!is.na(outMapFrame$columnIndex), ])
      cat("TABLE DATA\n")
      print(private$objectData)
    },
    # ====== 1.11. Function to export the data ======
    #' @description
    #' Export the data contained in the table as a \code{data.frame}
    #' @return A \code{data.frame} of the object's table data
    exportAsDataFrame = function() {
      private$objectData
    }
  )
)
