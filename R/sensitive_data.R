#' Sensitive_data
#' mask or reduce the resolution of sensitive location data
#' @param data A dataframe with X and Y coordinates (only works for UTM at the moment)
#' @param XField The X coordinate
#' @param YField The Y coordinate
#' @param Cat The GBIF Category. Defaults to "Not sensitive"
#' @return Data frame with masked or reduced resolution of the locations
#' @keywords internal
#' @export


Sensitive_data<-function(data, XField, YField,

                              Cat="NS"){

  library(tidyverse)

  library(encryptr)

  out<-data


  ### Switch: Cats CHECK THE ROUNDING

  switch(Cat,

         "2"={
           XField<-enquo(XField)
           YField<-enquo(YField)
           out<-data %>%
             dplyr::mutate(!!XField := round(as.numeric(!!XField),-5)) %>%
             dplyr::mutate(!!YField := round(as.numeric(!!YField),-5))
         },

         "3"={
           XField<-enquo(XField)
           YField<-enquo(YField)
           out<-data %>%
             dplyr::mutate(!!XField := round(!!XField,-3)) %>%
             dplyr::mutate(!!YField := round(!!YField,-3))
         },
         "4"={
           XField<-enquo(XField)
           YField<-enquo(YField)
           out<-data %>%
             dplyr::mutate(!!XField := round(!!XField,-1)) %>%
             dplyr::mutate(!!YField :=round(!!YField,-1))
         },
         "1"={### Highest category - encrypt the data
           XField<-enquo(XField)
           YField<-enquo(YField)
           encryptr::genkeys()
           # Keys == Ptarmigan
           encrypted = data %>%
             encryptr::encrypt(!!XField, !!YField)

           out<-encrypted},

  )

  return(out)
}


#' Full decrypt
#' unencrypt encrpyted sensitive location data
#' @param data A dataframe with encrpyted X and Y coordinates (only works for UTM at the moment)
#' @param target cols The X coordinate and the Y coordinate
#' @return Data frame with unmasked coordinates - you need the password to decrypt (enter it twice when promted)
#' @keywords internal
#' @export

Full_decrypt<-function(data=data,target_cols=c(x, y)){
  library(encryptr)
  library(dplyr)
  decrypted = data %>%
    encryptr::decrypt(data$x, data$y)

  return(decrypted)

}

### NOT IN USE
