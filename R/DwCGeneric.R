# ------ 1. GENERIC DARWIN CORE FILE CLASS ------
#' R6 class representing a generic data structure for a Darwin Core archive file
#'
#' The \code{DwcGeneric} class serves a base class to all Darwin Core archive file types. This class supports all kinds of Darwin Core
#' archive files but may miss some of the specialised functionality of the more specialist classes.
#' See \url{https://dwc.tdwg.org/terms/}{the Darwin core reference guide} for more information on Darwin core classes and the terms
#' supported by them.
DwCGeneric <- R6Class("DwCGeneric",
  # ====== 1.1. Define private members of the generic class ======
  private = list(
    # A vector containing the indeces of the columns that map to terms associated with the DwC class
    termMapping = character(),
    # A data frame containing the data held in the DwC archive file
    fileData = data.frame(),
    # A string containing the name to use for the Darwin core archive file
    fileName = character(),
    # A string containing the class name for the DwC class
    className = character(),
    # An integer containing the index of the column that contains the ID information
    idColumnIndex = integer()
  ),
  public = list(
    # ====== 1.2. Define an initialisation function for the generic class ======
    #' @description
    #' Create a new DwCGeneric object
    #' @param inClassName A character scalar giving the name of the Darwin core class type that is being
    #' instantiated
    #' @param inAssociatedTerms A character vector containing the names of terms associated with the class
    #' that can form columns in a Darwin core archive output file
    #' @param inFileName A character scalar giving the name to give the data file in a Darwin core archive
    #' @return A new \code{DwCGeneric} object
    initialize = function(inClassName, inAssociatedTerms, inFileName = character()) {
      # Test the consistency of the class name parameter
      self$className <- tryCatch(as.character(inClassName), error = function(err) {
        stop("error encountered whilst processing the class name: ", err)
      })
      if(length(self$className) == 0) {
        stop("error encountered whilst processing the class name: class name vector has length 0")
      } else if(length(self$className) > 1) {
        warning("entry for class name has more than one entry: only the first will be used")
        self$className <- self$className[1]
      }
      if(is.na(self$className) || self$className == "") {
        stop("error encountered whilst processing the class name: invalid class name given")
      }
      # Test the consistency of the file name parameter
      self$setFileName(inFileName)
      # Test the consistency of the associated terms parameters
      charTerms <- tryCatch(as.character(inAssociatedTerms), error = function(err) {
        stop("error encountered whilst processing the associated Darwin core terms")
      })
      if(length(charTerms) > 0) {
        # Create a vector containing the names of the associated terms
        self$termMapping = setNames(as.character(rep(NA, length(charTerms))), charTerms)
      }
      invisible(self)
    },
    # ====== 1.3. Define a function to import data from a data frame for the generic class ======
    #' @description
    #' Import data from a data frame to populate a DwC file object
    #' @param inDataFrame A \code{data.frame} containing the data to populate the Darwin core file
    #' object with
    #' @param idColumn Either a \code{character} scalar giving the name of the column in \code{inDataFrame} that
    #' represents a column containing information relating to the ID of the entry in the core file that this row
    #' pertains to, or a \code{integer} scalar giving the index of it
    #' @param inFileName A character scalar giving the name to give the data file in a Darwin core archive
    #' @param ... A series of named parameters taking names from the Darwin core terms associated with
    #' the defined class that are either \code{character} scalar values giving the
    #' column name of \code{inDataFrame} to be associated with the Darwin core term or an \code{integer} scalar
    #' giving the column number of \code{inDataFrame} to be associated with the Darwin core term
    importDataFrame = function(inDataFrame, idColumn, inFileName = character(), ...) {
      # Reset the mapping vector
      self$termMapping <- setNames(rep(NA, length(self$termMapping)), names(self$termMapping))
      # Import the data frame
      self$fileData <- tryCatch(as.data.frame(inDataFrame), error = function(err) {
        stop("error encountered processing the input data: ", err)
      })
      # Import the ID column
      idColumnIndex <- NA
      if(is.numeric(idColumn)) {
        idColumnIndex <- tryCatch(as.integer(idColumn), error = function(err) {
          stop("error encountered processing the ID column specification: ", err)
        })
      } else {
        idColumnIndex <- unlist(lapply(X = tryCatch(as.character(idColumn), error = function(err) {
          stop("error encountered processing the ID column specification: ", err)
        }), FUN = function(curVal, compVals) {
          which(curVal == compVals)
        }, compVals = colnames(self$fileData)))
      }
      if(length(idColumnIndex) == 0) {
        stop("error encountered processing the ID column specification: invalid column index given")
      } else if(length(idColumnIndex) > 1) {
        warning("more than one ID column index provided: only using the first value provided")
        idColumnIndex <- idColumnIndex[1]
      }
      if(is.na(idColumnIndex) || idColumnIndex < 1 || idColumnIndex > ncol(self$fileData)) {
        stop("error encountered processing the ID column specification: invalid column index given")
      }
      self$idColumnIndex <- idColumnIndex
      # Import the file name
      self$setFileName(inFileName)
      # Retrieve and process the named elements
      namedElements <- list(...)
      if(is.null(names(namedElements))) {
        stop("error encountered processing the Darwin core elements: extra parameters do not have names")
      }
      # Iterate over the extra parameters passed to the function
      for(curName in names(namedElements)) {
        if(curName == "" || is.na(curName)) {
          stop("error encountered processing the Darwin core elements: one or more extra parameters are not named")
        }
        termIndex <- which(curName == gsub("^.*\\:", "", names(self$termMapping), perl = TRUE))
        if(length(termIndex) <= 0) {
          stop("error encountered processing the Darwin core elements: extra parameter ", curName, " refers to a term that is not defined for this Darwin core class")
        } else if(length(termIndex) > 1) {
          warning("multiple terms with the same name associated with a single Darwin core class: possible class misspecification")
          termIndex <- termIndex[1]
        }
        curMapping <- as.character(NA)
        if(is.numeric(namedElements[[curName]])) {
          curMapping <- tryCatch(as.integer(namedElements[[curName]]), error = function(err) {
            stop("error encountered processing the Darwin core elements: ", err, " (when processing ", curName, ")")
          })
        } else {
          curMapping <- tryCatch(as.character(namedElements[[curName]]), error = function(err) {
            stop("error encountered processing the Darwin core elements: ", err, " (when processing ", curName, ")")
          })
        }
        if(length(curMapping) == 0) {
          # If the mapped value is length zero then treat it as unmapped
          curMapping <- as.character(NA)
        } else if(length(curMapping) > 1) {
          # Provide a warning if multiple mappings are present
          warning("multiple values given for the ", curName, " parameter: only the first value will be used")
          curMapping <- curMapping[1]
        }
        if(!is.na(curMapping)) {
          if(is.character(curMapping)) {
            curMapping <- which(curMapping == colnames(self$fileData))
            if(length(curMapping) == 0) {
              stop("error encountered processing the Darwin core elements: value associated with ", curName, " is not present in the data file")
            } else if(length(curMapping) > 1) {
              warning("multiple columns match value for parameter ", curName, ": only the first will be used")
              curMapping <- curMapping[1]
            }
          }
          if(curMapping < 1 || curMapping > ncol(self$fileData)) {
            stop("error encountered processing the Darwin core elements: value associated with ", curName, " is not valid")
          }
          self$termMapping[termIndex] <- curMapping
        }
      }
      invisible(self)
    },
    # ====== 1.4. Function to set the name of the file in the Darwin core archive ======
    #' @description
    #' Set the name of the file that the data will print to when preparing the Darwin core archive
    #' @param inFileName A character scalar giving the name to give the data file in a Darwin core archive
    setFileName = function(inFileName) {
      self$fileName <- tryCatch(as.character(inFileName), error = function(err) {
        stop("error encountered whilst processing file name: ", err)
      })
      if(length(self$fileName) == 0) {
        self$fileName <- self$className
      } else if(length(self$fileName) > 1) {
        warning("entry for file name has more than one entry: only the first will be used")
        self$fileName <- self$fileName[1]
      }
      if(is.na(self$fileName)) {
        self$fileName <- self$className
      }
      invisible(self)
    },
    # ====== 1.5. Function to retrieve the name of the file in the Darwin core archive ======
    #' @description
    #' Retrieve the name of the file that the data will print to when preparing the Darwin core archive
    #' @return The name currently set as the file name in the output archive
    getFileName = function() {
      self$fileName
    },
    # ====== 1.6. Function to get the name of the Darwin core class that the object holds ======
    #' @description
    #' Retrieve the name of the class used in Darwin core
    #' @return A \code{character} scalar contining the name of the Darwin core class
    getDwCClassName = function() {
      self$className
    },
    # ====== 1.7. Function to retrieve the names of the terms associated with the class ======
    #' @description
    #' Retrieve a data frame of terms associated with the class
    #' @return A \code{data.frame} containing information pertaining to the Darwin core terms associated
    #' with the Darwin core class of the object. The \code{data.frame} has the following columns:
    #' \itemize{
    #'     \item{termName}{The name of the term}
    #'     \item{namespaceName}{The name of the namespace containing the term}
    #'     \item{termIRI}{The URL of the term definition}
    #'     \item{termVersionIRI}{The URL of the versioned term definition}
    #'     \item{dateModified}{The date of the last modification of the term definition}
    #'     \item{label}{A long version of the term name}
    #'     \item{isReplacedBy}{For depracated terms this entry contains the URL of the definition of the replacing term}
    #'     \item{definition}{The written definition of the usage of the term}
    #'     \item{notes}{Notes on the use of the term}
    #'     \item{type}{The data type of the term}
    #'     \item{examples}{An example of data values that could be used for this term}
    #'     \item{termInformationLN}{Supplementary information provided by Living Norway to guide usage of the term}
    #'     \item{execCommitteeDecision}{A set of URLs containing decisions made about the term made by the relevant executive committee}
    #'     \item{miscInformation}{Other information about the term}
    #'     \item{termDef}{A label denoting under what standard the term is defined}
    #' }
    getTermInformation = function() {
      # Initialise an output data frame
      outFrame <- as.data.frame(matrix(character(), nrow = 0, ncol = ncol(DwCTermFrame), dimnames = list(NULL, colnames(DwCTermFrame))))
      if(length(self$termMapping) > 0) {
        # Retrieve the information of the terms associated with the Darwin core class
        outFrame <- DwCTermFrame[names(self$termMapping), ]
      }
      outFrame
    },
    # ====== 1.8. Function to retrieve variable mapping information ======
    #' @description
    #' Retrieve information describing the mapping between Darwin core terms and column names
    #' @return An \code{integer} vector with a \code{names} attribute that contains the names of the Darwin core terms associated with the
    #' class and the values of vector containing the column index of the variable mapped to that term. \code{NA} values mean that the associated term
    #' is not present in the file
    getMappingInformation = function() {
      self$termMapping
    }
  )
)
