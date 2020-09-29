library(R6)
DwCMetadata<-R6::R6Class(
  classname = "DwCMetadata",
  public = list(
    metadata=NA,
    initialize=function(metadata){
        self$metadata<-metadata},
    #' read_metadata
    #' read the metadata from a rmarkdown file
    #' @param filepath a filepath to a RMarkdown file
    #' @return Output: text string of yaml information

    getMetadata = function(filepath) {
      x = readr::read_lines(filepath) # read markdown using readlines
      rng = grep("^---$", x)
      rng = rng + c(1, -1)
      x = x[rng[1]:rng[2]]
      names(x) = gsub("(.*):.*", "\\1", x)
      x = gsub(".*: (.*)", "\\1", x)
      return(as.list(x))
    }
    ))

test<-DwCMetadata$new(metadata = NA)
test$getMetadata(filepath ="C:/Users/matthew.grainger/Documents/Projects_in_development/Test_the_dataPackage/Rock_ptarmigan/metadata/metadata/metadata.Rmd")

