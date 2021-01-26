getGBIFEventTerm <- function() {
  GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Event"]]$termInfo
}

getGBIFEventMembers <- function() {
  GBIFCoreClassList[["http://rs.tdwg.org/dwc/terms/Event"]]$compositeTerms
}

# ------ 1. GBIF EVENT CORE FILE CLASS ------
#' R6 class representing a generic data structure for a GBIF event core (based on the Darwin core)
#'
#' The \code{GBIFEvent} class serves allows for the specification of data tables that comply with the
#' event core specification of GBIF.  This class specification is to help facilitate
#' \url{https://github.com/gbif/ipt/wiki/BestPracticesSamplingEventData}{best practices in publishing sampling-event data}.
GBIFEvent <- R6Class("GBIFEvent",
  inherit = DwCGeneric,
  # ====== 1.1. Define private members of the GBIF event class ======
  private = list(
  ),
  public = list(
    # ====== 1.2. Define an initialization function for the GBIF event class ======
    #' @description
    #' Create a new GBIFEvent object
    #' @param objectData A \code{data.frame} containing the data to import into the object
    #' @param idColumnInfo Either a \code{character} scalar containing the column name
    #' of \code{objectData} or an \code{integer} scalar giving the index of the column of
    #' \code{objectData} that corresponds to the ID variable
    #' @param ... A named set of parameter corresponding to Darwin core terms associated
    #' with the GBIFEvent class type. Each is either a a \code{character} scalar containing
    #' the column name of \code{objectData} or an \code{integer} scalar giving the index of
    #' the column of \code{objectData} that corresponds to the term
    #' @return A new \code{GBIFEvent} object
    #' @seealso \code{\link[DwCTerm]{DwCTerm}}
    initialize = function(objectData, idColumnInfo = "http://rs.tdwg.org/dwc/terms/eventID", ...) {
      super$initialize(
        getGBIFEventTerm(),
        getGBIFEventMembers(),
        objectData,
        idColumnInfo,
        ...
      )
      invisible(self)
    }
  )
)
