#' function to search GBIFOccurrenceMembers
#' @param term the search term
#' @return A subset of the list from the LivingNorwayR::getGBIFOccurrenceMembers() function
#' @seealso \code{\link[GBIFOccurrence]{GBIFOccurrence}}
#' @examples
#' searchGBIFEventMembers("tax")
#' @export

searchGBIFOccurenceMembers=function(term){
  list=LivingNorwayR::getGBIFOccurrenceMembers()
  # get the names of each element in the list so that it can be searched
  names_list=names(list)
  search_Occ=names_list[grep(paste0(term), names_list)]

  for (i in 1:length(search_Occ)){
    text=paste0("list$`http://rs.tdwg.org/dwc/terms/", basename(search_Occ[i]),"`")
    out=eval(parse(text=text))
    print(out)
  }

}

#' function to search GBIFEventMembers
#' @param term the search term
#' @return A subset of the list from the LivingNorwayR::getGBIFEventMembers() function
#' @seealso \code{\link[GBIFEvent]{GBIFEvent}}
#' @examples
#' searchGBIFEventMembers("georef")
#' @export

searchGBIFEventMembers=function(term){
  list=LivingNorwayR::getGBIFEventMembers()
  # get the names of each element in the list so that it can be searched
  names_list=names(list)
  search_Occ=names_list[grep(paste0(term), names_list)]

  for (i in 1:length(search_Occ)){
    text=paste0("list$`http://rs.tdwg.org/dwc/terms/", basename(search_Occ[i]),"`")
    out=eval(parse(text=text))
    print(out)
  }

}

#' function to search GBIFCoreClasses
#' @param term the search term
#' @return A subset of the list from the LivingNorwayR::getGBIFCoreClasses() function
#' @seealso \code{\link[GBIFEvent]{GBIFEvent}}
#' \code{\link[GBIFOccurrence]{GBIFOccurrence}}
#' \code{\link[GBIFTaxon]{GBIFTaxon}}
#' @examples
#' searchGBIFCoreClasses("Event")
#' @export

searchGBIFCoreClasses=function(term){
  list=LivingNorwayR::getGBIFCoreClasses()
  # get the names of each element in the list so that it can be searched
  names_list=names(list)
  search_Occ=names_list[grep(paste0(term), names_list)]

  for (i in 1:length(search_Occ)){
    text=paste0("list$", basename(search_Occ[i]))
    out=eval(parse(text=text))
    print(out)
  }

}

#' function to search GBIFExtensionClasses
#' @param term the search term
#' @return A subset of the list from the LivingNorwayR::getGBIFExtensionClasses() function
#' @seealso \code{\link[GBIFExtendedMeasurementOrFact]{GBIFExtendedMeasurementOrFact}}
#' \code{\link[GBIFMeasurementOrFact]{GBIFMeasurementOrFact}}
#' @examples
#' searchGBIFExtensionClasses("Multimedia")
#' @export

searchGBIFExtensionClasses=function(term){
  list=LivingNorwayR::getGBIFExtensionClasses()
  # get the names of each element in the list so that it can be searched
  names_list=names(list)
  search_Occ=names_list[grep(paste0(term), names_list)]

  for (i in 1:length(search_Occ)){
    text=paste0("list$", basename(search_Occ[i]))
    out=eval(parse(text=text))
    print(out)
  }

}


