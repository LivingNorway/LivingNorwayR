# get geographic extent
#' Get the geographic extent of a dataframe with geographic coordinates
#'
#' This function plots a map of the latitude and longitude from a datafile specified by the user.
#' @param  df a dataframe
#' @param lat latitude column indicated by data$lat
#' @param lon longitude column indicated by data$lon
#' @param add_map add a map (default is "yes")
#' @return A leaflet map of the coordinates and the minimum and maximum of the latitude and longitude
#' @example #get_geographic_extent(lat = raw_data$Latitude, lon = raw_data$Longitude) #use example from our own examples package
#' @export


get_geographic_extent<-function(df,lon,lat, add_map="no"){
  my.sf.point <- sf::st_as_sf(x = df,
                              coords = c(lon, lat),
                              crs = "+proj=longlat +datum=WGS84")


  switch(add_map,
         yes={map<-leaflet::leaflet() %>% addTiles() %>% addCircleMarkers( data = my.sf.point)
         return(list(sf::st_bbox(my.sf.point), map))
         },
         no={return(sf::st_bbox(my.sf.point))
         }
  )
}


#d <- as_tibble(read_delim(
 # "C:/Users/matthew.grainger/Documents/Projects_in_development/LNC2020_presentation/Rock_ptarmigan_example/data/occurrence.txt", delim="\t", quote = ""))

######################################################################################

# d <- d %>%
#   mutate(dynamicProperties = purrr::map(dynamicProperties, ~ jsonlite::fromJSON(.) %>% as.data.frame())) %>%
#   unnest(dynamicProperties) %>%
#   filter(organismName!=4272265) %>%
#   mutate(Year=year(eventDate))
#
# d<-as.data.frame(d)
#
# get_geographic_extent(d, lon = "decimalLongitude",lat= "decimalLatitude", add_map = "yes")
#


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

get_NOR_geographic_extent<-function(df,X,Y,Code, add_map="no"){
  df.sf <- sf::st_as_sf(df, coords = c(X, Y) ) %>%
    sf::st_set_crs(Code) %>%   #set coordinate system used
    sf::st_transform(4326)     #transform coordinates to WGS84 coordinates

  switch(add_map,
         yes={map<-leaflet::leaflet() %>% addTiles() %>% addCircleMarkers( data = df.sf )
         return(list(sf::st_bbox(df.sf), map))
         },
         no={return(sf::st_bbox(df.sf))
         }
  )


}



