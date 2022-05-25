#' function to search GBIFOccurrenceMembers
#' @param term the search term
#' @return A subset of the list from the LivingNorwayR::getGBIFOccurrenceMembers() function
#' @seealso \code{\link[GBIFOcurrence]{GBIFOcurrence}}
#' @examples
#' searchGBIFOccurenceMembers("tax")
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
