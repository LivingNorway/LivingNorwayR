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
  ### Setting up the species list
  data <- data %>% dplyr::rename("TaxaField"=TaxaField)

  TaxaList <- x %>% dplyr::group_by(TaxaField) %>%
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
