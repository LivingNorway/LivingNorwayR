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
