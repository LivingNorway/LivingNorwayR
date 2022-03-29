#' Function for getting data from the Livng Norway Portal
#' @description using the dataset key for a Living Norway dataset (and other GBIF datasets) you can download and extract the elements of the Darwin Core Archive file
#' @param datasetID the dataset unique identifying key
#' @examples
#' Archive=getLNportalData(datasetID = "b848f1f3-3955-4725-8ad8-e711e4a9e0ac" )
#' core<-Archive$getCoreTable()
#' extension<-Archive$getExtensionTables()
#' @export

getLNportalData=function(datasetID){
  dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint"))
  endpoint_url <- dataset[[1]]$url
  datasetName=sub(".*r=", "", endpoint_url)
  tempDirLoc <- tempdir()
  localDataLoc <- file.path(tempDirLoc,datasetName)
  download.file(endpoint_url, localDataLoc, mode = "wb")
  unzip ("data/temp.zip", exdir = "data")

  # Use LivingNorwayR to extract the Archive
  LivingNorwayR::initializeDwCArchive(localDataLoc, "UTF-8")
}
