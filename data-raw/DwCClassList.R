# Source file to generate the list of classes (and the terms associated with them) in Darwin core used by some package functions
# ------ 1. RETRIEVE CLASS SPECIFICATIONS AND REGISTER AS DATA ------
# Retrieve the class specifications
DwCClassList <- retrieveDwCClassSpecifications(TRUE)
# Store the retrieved class specifications as data in the package
usethis::use_data(DwCClassList, overwrite = TRUE, internal = TRUE)
