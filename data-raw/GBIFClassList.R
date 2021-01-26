# Source file to generate the list of classes (and the terms associated with them) that are supported by GBIF
# ------ 1. RETRIEVE CLASS SPECIFICATIONS AND REGISTER AS DATA ------
# Retrieve the class specifications for both the core and extension classes
GBIFCoreClassList <- retrieveGBIFClassSpecifications("core", TRUE)
GBIFExtClassList <- retrieveGBIFClassSpecifications("extension", TRUE)
# Store the retrieved class specifications as data in the package
usethis::use_data(GBIFCoreClassList, GBIFExtClassList, overwrite = TRUE, internal = TRUE)
