# get geographic extent
#' Get the geographic extent of a dataframe with geographic coordinates
#'
#' This function plots a map of the latitude and longitude from a datafile specified by the user.
#'
#' @param lat latitude column indicated by data$lat
#' @param lon longitude column indicated by data$lon
#' @return A ggplot map of the coordinates
#' @example #get_geographic_extent(lat = raw_data$Latitude, lon = raw_data$Longitude) #use example from our own examples package
#' @export


get_geographic_extent<-function(lat,lon){
  lat<-as.numeric(lat)
  long<-as.numeric(lon)
  coords<-as.data.frame(cbind(lat,long))
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  ggplot2::ggplot(data = world) +
    ggplot2::geom_sf() +
    ggplot2::geom_point(data=coords,aes(long,lat))
}


