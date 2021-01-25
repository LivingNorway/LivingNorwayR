getGBIFMeasurementOrFactTerm <- function() {
  GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]]$termInfo
}

getGBIFMeasurementOrFactMembers <- function() {
  GBIFExtClassList[["http://rs.tdwg.org/dwc/terms/MeasurementOrFact"]]$compositeTerms
}

# ------ 1. GBIF MEASUREMENT OR FACT EXTENSION FILE CLASS ------
#' R6 class representing a generic data structure for a GBIF measuremnet or fact extension (based on the Darwin core)
#'
#' The \code{GBIFMeasurementOrFact} class serves allows for the specification of data tables that comply with the
#' measurement or fact extension specification of GBIF.
GBIFMeasurementOrFact <- R6Class("GBIFMeasurementOrFact",
  inherit = DwCGeneric,
  # ====== 1.1. Define private members of the GBIF event class ======
  private = list(
  ),
  public = list(
    # ====== 1.2. Define an initialization function for the GBIF measurement or fact class ======
    #' @description
    #' Create a new GBIFMeasurementOrFact object
    #' @param objectData A \code{data.frame} containing the data to import into the object
    #' @param idColumnInfo Either a \code{character} scalar containing the column name
    #' of \code{objectData} or an \code{integer} scalar giving the index of the column of
    #' \code{objectData} that corresponds to the ID variable
    #' @param ... A named set of parameter corresponding to Darwin core terms associated
    #' with the GBIFMeasurementOrFact class type. Each is either a a \code{character} scalar containing
    #' the column name of \code{objectData} or an \code{integer} scalar giving the index of
    #' the column of \code{objectData} that corresponds to the term
    #' @return A new \code{GBIFMeasurementOrFact} object
    #' @seealso \code{\link[DwCTerm]{DwCTerm}}
    initialize = function(objectData, idColumnInfo, ...) {
      super$initialize(
        getGBIFMeasurementOrFactTerm(),
        getGBIFMeasurementOrFactMembers(),
        objectData,
        idColumnInfo,
        ...
      )
      invisible(self)
    }
  )
)
