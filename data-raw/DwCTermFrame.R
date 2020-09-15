# Source file to generate the data frame of Darwin core terms used by some package functions
# ------ 1. RETRIEVE TERM SPECIFICATIONS AND REGISTER AS DATA ------
# Retrieve the term specifications
DwCTermFrame <- retrieveDwCTermSpecifications(FALSE, TRUE)
# Store the retrieved term specifications as data in the package
usethis::use_data(DwCTermFrame, overwrite = TRUE, internal = TRUE)
