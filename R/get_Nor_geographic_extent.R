# get Norway geographic extent
#' Get the geographic extent of a dataframe with geographic UTM XY coordinates
#'
#' This function plots a map of the UTM X Y coordinates from a datafile specified by the user.
#' @param df the dataframe
#' @param X latitude column indicated by data$X
#' @param Y longitude column indicated by data$Y
#' @param Code Code to convert UTM to lat lon
#' @return A ggplot map of the coordinates
#' @example
#' @export

get_NOR_geographic_extent<-function(df,X,Y,Code){
  df.sf <- sf::st_as_sf(df, coords = c(X, Y) ) %>%
    sf::st_set_crs(Code) %>%   #set coordinate system used
    sf::st_transform(4326)     #transform coordinates to WGS84 coordinates
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addMarkers( data = df.sf )
}


