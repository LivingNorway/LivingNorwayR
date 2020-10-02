# This source file requires that a "WORKSPACE_LIVINGNORWAY_RPACKAGE" variable is included in your environment that specifies the location
# of the source code for the Living Norway package.  You cna edit the environmental variables present when R loads by using the function
# usethis::edit_r_environ()

library(xml2)
# ------ 1. IMPORT FUNCTIONS AND CLASS SPECIFICATIONS ------
# Retrieve the functions that determine current Darwin core standard terms
source(paste(Sys.getenv("WORKSPACE_LIVINGNORWAY_RPACKAGE"), "R/DwCTerms.R", sep = "/"))

# ------ 2. UPDATE DARWIN CORE TERM LISTS AND CLASS ASSOCIATIONS ------
termList <- retrieveDwCTermSpecifications(TRUE, TRUE)
classList <- retrieveDwCClassSpecifications(TRUE, TRUE)

# ------ 3. FUNCTION TO GENERATE A DARWIN CORE CLASS SOURCE FILE FROM A SPECIFICATION ------
generateDwCClassFile <- function(fileLocation, classInformation, publicOverride, privateOverride) {
  classText <- paste(
    paste("DwC", classInformation$termName, " <- R6Class(\"DwC", classInformation$termName, "\", ", sep = ""),
    ")",
    sep = "\n")
}
