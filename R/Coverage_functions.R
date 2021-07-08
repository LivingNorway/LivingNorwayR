#' get_temporal_cover
#' Get temporal coverage from raw data / mapped data
#' @param x Date/time column from user defined dataframe needs to be formatted as class Date
#' @return Output: temporal extent chart?
#' @export

get_temporal_cover<-function(x){

  #check if x is a Date

  if(isTRUE(lubridate::is.Date(x))==FALSE){
    x=format(as.POSIXct(as.character(x),format='%Y-%m-%d %H:%M:%S.%OS'),format='%Y-%m-%d')
    mn<-min(x)
    mx<-max(x)
    print(paste0("Range of dates: ", mn, " to ", mx))
    }
    else if(isTRUE(lubridate::is.Date(x))==TRUE){
        mn<-min(x)
        mx<-max(x)
        print(paste0("Range of dates: ", mn, " to ", mx))
      }
      else{
        print("Date format is not supported; convert to character or Date")

      }

    }

#' get_taxa_cover
#' Get taxonomic coverage from raw data / mapped data.
#' @param data Dataset including a column with taxonomic information for the record/occurence
#' @param TaxaField The field (column) in the data set containing information about the taxonomic group of the record/occurence
#' @param TaxaLevel Taxonomic level (e.g. Species, Genus)
#' @param addFreq Control argument - include number of records in the table listing the taxonomic coverage
#' @param addPlot Control argument - add a barplot with number of records for each taxa defined above
#' @return Output: list of taxa. Potential chart displaying proportion of occurences from different taxa.
#'
#' @export


get_taxa_cover<-function(data, TaxaField="SpeciesName", TaxaLevel="Species",
                         addFreq="Yes", addPlot="Yes"){
  library(tidyverse)
  library(kableExtra)
  ### Setting up the species list
  data <- data %>% dplyr::rename("TaxaField"=TaxaField)

  TaxaList <- data %>% dplyr::group_by(TaxaField) %>%
    dplyr::count() %>%
    dplyr::rename("Number of records"=n, "Taxa"=TaxaField)

  ### Switch: report number of records in alongside the species list
  switch(addFreq,
         Yes={temp2 <- TaxaList %>% rename(!!paste(TaxaLevel, "name", sep=" ") :=Taxa)
         t1 <- kable(temp2)
         },
         No={temp <- select(TaxaList, Taxa)
         temp2 <- temp %>% rename(!!paste(TaxaLevel, "name", sep=" ") :=Taxa)
         t1 <- kable(temp2)
         }
  )

  ### Simple barplot;
  p1 <- ggplot2::ggplot(data=TaxaList, aes(x=`Taxa`, y=`Number of records`, fill=Taxa))+
    geom_bar(stat="identity") + scale_fill_viridis_d(option = "D", alpha=0.65)+
    theme(axis.title.x=element_blank()) +
    guides(fill=guide_legend(title=paste(TaxaLevel, "name", sep=" ")))



  ### Selecting output
  if (addPlot=="Yes") print(p1)
  return(t1)

}

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
                              crs = "+proj=longlat +datum=WGS84",
                              na.fail=FALSE)


  switch(add_map,
         yes={map<-leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::addCircleMarkers( data = my.sf.point)
         return(list(sf::st_bbox(my.sf.point), map))
         },
         no={return(sf::st_bbox(my.sf.point))
         }
  )
}






