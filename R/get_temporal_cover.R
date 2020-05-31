#' get_temporal_cover
#' Get temporal coverage from raw data / mapped data
#' @param x Date/time column from user defined dataframe needs to be formatted as class Date
#' @return Output: temporal extent chart?
#' @export


get_temporal_cover<-function(x){
  mn<-min(x)
  mx<-max(x)
  print(paste0("Range of dates: ", mn, " to ", mx))

}

