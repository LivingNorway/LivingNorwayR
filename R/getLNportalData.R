#' Function for getting data from the Livng Norway Portal
#' @description using the dataset key for a Living Norway dataset (and other GBIF datasets) you can download and extract the elements of the Darwin Core Archive file
#' @param datasetKey the dataset unique identifying key
#' @param version the dataset version as indicated on the IPT from which the dataset was published
#' @examples
#' Archive=getLNportalData(datasetKey = "b848f1f3-3955-4725-8ad8-e711e4a9e0ac" )
#' # Then interact with the archive
#' core<-Archive$getCoreTable()
#' extension<-Archive$getExtensionTables()
#' metadata<-Archive$getMetadata()
#' # For datasets with several versions use:
#' Archive=getLNportalData(datasetKey="4a00502d-6342-4294-aad1-9727e5c24041",version=1.6)
#' @export

getLNportalData=function(datasetKey, version=NULL){
  dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",datasetKey,"/endpoint"))
  endpoint_url <- dataset[[1]]$url
  datasetName=sub(".*r=", "", endpoint_url)
  if(!is.null(version)){
    endpoint_url<-paste0(endpoint_url,"&v=",version)
    datsetName=sub("&v*.","", datasetName)
  }
  tempDirLoc <- tempdir()
  localDataLoc <- file.path(tempDirLoc,datasetName)
  download.file(endpoint_url, localDataLoc, mode = "wb")

  # Use LivingNorwayR to extract the Archive
  LivingNorwayR::initializeDwCArchive(localDataLoc, "UTF-8")
}
