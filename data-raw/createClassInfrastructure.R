# ------ 1. CLASS SOURCE FILE GENERATION FUNCTION ------
#' Function to generate source files to support all of the GBIF supported formats
#'
#' @param fileLocation A \code{character} scalar providing the location to store the
#' source files
createGBIFClassInfrastructure <- function(fileLocation) {
  # Retrieve all the GBIF classes and their members
  allGBIFClassList <- retrieveGBIFClassSpecifications("all", TRUE)
  # Produce a series of R scripts for each GBIF class
  shortNames <- sapply(X = names(allGBIFClassList), FUN = function(curTermName, fileLocation) {
    # Sanity check the file location
    if(length(fileLocation) > 1) {
      warning("location for files has length greater than one: only the first entry will be used")
    } else if(length(fileLocation) <= 0) {
      stop("error encountered during processing of folder location: vector has length of zero")
    }
    if(!dir.exists(fileLocation[1])) {
      stop("error encountered during processing of folder location: directory does not exist")
    }
    # Retrieve the short name for the class
    shortName <- gsub("^.*[\\/\\:]", "", curTermName, perl = TRUE)
    # Set the file name for the class definition file
    fileLoc <- file.path(fileLocation[1], paste("GBIF", shortName, ".R", sep = ""))
    if(!file.exists(fileLoc)) {
      cat(
        paste("# ------ 1.", toupper(shortName), "TERM INFORMATION ------"),
        paste("#' Return the information of the term associated with the GBIF", shortName, "class"),
        "#' @return A \\code{DwCTerm} object containing the term information",
        "#' @seealso \\code{\\link[DwCTerm]{DwCTerm}}",
        paste("getGBIF", shortName, "Term <- function() {", sep = ""),
          "\toutValue <- NULL",
          paste("\tif(\"", curTermName, "\" %in% names(GBIFCoreClassList)) {", sep = ""),
            paste("\t\toutValue <- GBIFCoreClassList[[\"", curTermName, "\"]]$termInfo", sep = ""),
          paste("\t} else if(\"", curTermName, "\" %in% names(GBIFExtClassList)) {", sep = ""),
            paste("\t\toutValue <- GBIFExtClassList[[\"", curTermName, "\"]]$termInfo", sep = ""),
          "\t}",
          "\toutValue",
        "}\n",
        paste("# ------ 2.", toupper(shortName), "MEMBER INFORMATION ------"),
        paste("#' Return a list of properties associated with the GBIF", shortName, "class"),
        "#' @return A \\code{list} of \\code{DwCTerm} objects, one for each member of the the",
        paste("#' GBIF", shortName, "class"),
        "#' @seealso \\code{\\link[DwCTerm]{DwCTerm}}",
        paste("getGBIF", shortName, "Members <- function() {", sep = ""),
          "\toutValue <- NULL",
          paste("\tif(\"", curTermName, "\" %in% names(GBIFCoreClassList)) {", sep = ""),
            paste("\t\toutValue <- GBIFCoreClassList[[\"", curTermName, "\"]]$compositeTerms", sep = ""),
          paste("\t} else if(\"", curTermName, "\" %in% names(GBIFExtClassList)) {", sep = ""),
            paste("\t\toutValue <- GBIFExtClassList[[\"", curTermName, "\"]]$compositeTerms", sep = ""),
          "\t}",
          "\toutValue",
        "}\n",
        paste("# ------ 3. GBIF", toupper(shortName), "CLASS ------"),
        paste("#' R6 class representing a data structure for a GBIF", shortName, "augmented data table (based on the Dawrin core)"),
        "#'",
        paste("#' The \\code{GBIF", shortName, "} class allows for the specification of data tables that comply with the ", shortName, sep = ""),
        paste("#' \\url{", curTermName, "}{class specification} of GBIF.", sep = ""),
        paste("GBIF", shortName, " <- R6Class(\"GBIF", shortName, "\",", sep = ""),
          "\tinherit = DwCGeneric,",
          paste("\t# ====== 3.1. Define private members of the GBIF", shortName, "class ======"),
          "\tprivate = list(",
          "\t),",
          "\tpublic = list(",
          paste("\t\t# ====== 3.2. Define an initialization function for the GBIF", shortName, "class ======"),
          "\t\t#' @description",
          paste("\t\t#' Create a new GBIF", shortName, " object", sep = ""),
          "\t\t#' @param objectData A \\code{data.frame} containing the data to import into the object",
          "\t\t#' @param idColumnInfo Either a \\code{character} scalar containing the column name of",
          "\t\t#' \\code{objectData} or an \\code{integer} scalar giving the index of the column of",
          "\t\t#' \\code{objectData} that corresponds to the ID variable.  Alternatively, this parameter",
          "\t\t#' may be the qualified name of the Darwin core term for which the appropriately mapped column",
          "\t\t#' will be used as the ID variable (the possible Darwin core term names can be found by running",
          paste("\t\t#' \\code{names(getGBIF", shortName, "Members())})", sep = ""),
          "\t\t#' @param nameAutoMap A \\code{logical} scalar that if \\code{TRUE} maps the columns of \\code{objectData}",
          "\t\t#' to their respective Darwin core terms based on the column names",
          "\t\t#' @param defDateFormat A \\code{character} scalar providing the default format for strings denoting dates in the",
          "\t\t#' data table.  See the \\url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values",
          "\t\t#' for this string.",
          paste("\t\t#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIF", shortName, sep = ""),
          "\t\t#' class type.  Each is either a \\code{character} scalar containing the column name of \\code{objectData}",
          "\t\t#' or an \\code{integer} scalar giving the index of the column of \\code{objectData} that corresponds to the",
          paste("\t\t#' term. Mappable terms can be found using: \\code{sapply(X = getGBIF", shortName, "Members(), FUN = function(curTerm) { curTerm$getTermName() })}", sep = ""),
          paste("\t\t#' @return A new \\code{GBIF", shortName, "} object", sep = ""),
          paste("\t\t#' @seealso \\code{\\link[DwCTerm]{DwCTerm}} \\code{\\link[getGBIF", shortName, "Members]{getGBIF", shortName, "Members}}", sep = ""),
          "\t\tinitialize = function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = \"YYYY-MM-DD\", ...) {",
            "\t\t\tsuper$initialize(",
              paste("\t\t\t\tgetGBIF", shortName, "Term(),", sep = ""),
              paste("\t\t\t\tgetGBIF", shortName, "Members(),", sep = ""),
              "\t\t\t\tobjectData,",
              "\t\t\t\tidColumnInfo,",
              "\t\t\t\tnameAutoMap,",
              "\t\t\t\tdefDateFormat,",
              "\t\t\t\t...",
            "\t\t\t)",
            "\t\t\tinvisible(self)",
          "\t\t}",
          "\t)",
        ")\n",
        "# ------ 4. INITIALISATION FUNCTION ------",
        paste("#' Initialize a new GBIF", shortName, "object"),
        "#' @param objectData A \\code{data.frame} containing the data to import into the object",
        "t#' @param idColumnInfo Either a \\code{character} scalar containing the column name of",
        "#' \\code{objectData} or an \\code{integer} scalar giving the index of the column of",
        "#' \\code{objectData} that corresponds to the ID variable.  Alternatively, this parameter",
        "#' may be the qualified name of the Darwin core term for which the appropriately mapped column",
        "#' will be used as the ID variable (the possible Darwin core term names can be found by running",
        paste("#' \\code{names(getGBIF", shortName, "Members())})", sep = ""),
        "#' @param nameAutoMap A \\code{logical} scalar that if \\code{TRUE} maps the columns of \\code{objectData}",
        "#' to their respective Darwin core terms based on the column names",
        "#' @param defDateFormat A \\code{character} scalar providing the default format for strings denoting dates in the",
        "#' data table.  See the \\url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values",
        "#' for this string.",
        paste("#' @param ... A named set of paramaeters corresponding to Darwin core terms associated with the GBIF", shortName, sep = ""),
        "#' class type.  Each is either a \\code{character} scalar containing the column name of \\code{objectData}",
        "#' or an \\code{integer} scalar giving the index of the column of \\code{objectData} that corresponds to the",
        paste("#' term. Mappable terms can be found using: \\code{sapply(X = getGBIF", shortName, "Members(), FUN = function(curTerm) { curTerm$getTermName() })}", sep = ""),
        paste("#' @return A new \\code{GBIF", shortName, "} object", sep = ""),
        paste("#' @seealso \\code{\\link[DwCTerm]{DwCTerm}} \\code{\\link[getGBIF", shortName, "Members]{getGBIF", shortName, "Members}}", sep = ""),
        paste("initializeGBIF", shortName, " <- function(objectData, idColumnInfo, nameAutoMap = FALSE, defDateFormat = \"YYYY-MM-DD\", ...) {", sep = ""),
          paste("\tGBIF", shortName, "$new(objectData = objectData, idColumnInfo = idColumnInfo, nameAutoMap = nameAutoMap, defDateFormat = defDateFormat, ...)", sep = ""),
        "}\n",
        sep = "\n", file = fileLoc)
    }
    shortName
  }, fileLocation = tryCatch(as.character(fileLocation), error = function(err) {
    stop("error encountered during processing of folder location:", err)
  }))
  # Replace the environment lookup file
  envirLookupFile <- file.path(fileLocation, "GBIFEnvironmentLookup.R")
  if(file.exists(envirLookupFile)) {
    unlink(envirLookupFile)
  }
  # Produce the environment lookup function source file
  cat(
    "# ------ 1. GBIF ENVIRONMENT LOOKUP FUNCTION ------",
    "#' Lookup up a GBIF class environment from a qualified name",
    "#' @param qualName A \\code{character} of qualifed names for the GBIF class to lookup",
    "#' @return A \\code{list} of environments representing \\code{R6} classes derived from \\code{DwCGeneric}",
    "#' corresponding to each element in \\code{qualName}.",
    "GBIFClassLookup <- function(qualName) {",
      "\tinQualName <- tryCatch(as.character(qualName), error = function(err) {",
      "\t\tstop(\"error encountered processing the qualified class name:\", err)",
      "\t})",
      "\toutVal <- list()",
      "\tif(length(inQualName) > 0) {",
      "\t\toutVal <- lapply(X = inQualName, FUN = function(curQualName) {",
      "\t\t\tswitch(curQualName,",
      paste("\t\t\t\t\"", names(allGBIFClassList), "\" = GBIF", shortNames, ",", sep = "", collapse = "\n"),
      "\t\t\t\tNULL",
      "\t\t\t)",
      "\t\t})",
      "\t}",
      "\toutVal",
    "}",
    sep = "\n", file = envirLookupFile)
}
