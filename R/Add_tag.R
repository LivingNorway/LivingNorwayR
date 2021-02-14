#' Add_tag
#' Add metadata html span tag (only run in RMarkdown)
#' @param tagtype The name of the EML element that will be tagged
#' @param txt The txt for the tag
#' @return Output: html span tag
#' @export


add_tag<-function(tagtype,txt){
  #fmt <- rmarkdown::default_output_format(knitr::current_input())$name # this needs to be in the RMArkdown file
  if(fmt=="html_document"){
    htmltools::span(id=tagtype, txt)
  }else{
    print(txt)
  }
}

#add_tag("Project", "ABC")

#<span id="Project">ABC</span>
