#' Add_title_tag
#' Add metadata title html tag (only run in RMarkdown)
#' @param Title The title of the project
#' @return Output: html div tag "LN_Title"
#' @export


add_tag_title<-function(Title){
  #fmt <- rmarkdown::default_output_format(knitr::current_input())$name # this needs to be in the RMArkdown file
  if(fmt=="html_document"){
    htmltools::span(id="LN_Title", Title)
  }else{
    print(Title)
  }
}


