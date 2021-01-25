# Source file to generate the list of Darwin core terms used by some package functions
# ------ 1. RETRIEVE TERM SPECIFICATIONS AND REGISTER AS DATA ------
# Retrieve the term specifications
DwCTermList <- retrieveDwCTermSpecifications(TRUE, TRUE)
# Store the retrieved term specifications as data in the package
usethis::use_data(DwCTermList, overwrite = TRUE, internal = TRUE)
