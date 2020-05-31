#' Check_time_data
#' compare with standard time_data format, and look for outliers
#' @param X time_data column from dataframe
#' @return Output: list of non-valid rows (just to check not stop the workflow)
#' @export


check_time_data<-function(X){


}

#' Check_date
#' Look for a date column
#' @param dataframe dataframe
#' @return Output: list of valid cols (is.Date)
#' @export


Check_date<-function(dataframe){
  which(purrr::map(dataframe,function(x){lubridate::is.Date(x)})==TRUE)
}

