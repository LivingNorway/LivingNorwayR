#'get project title from working directory
#' This function gets the project title from the working directory for use in the metadata.rmd
#' @return the project title as a charcter vector
#' @export

get_project_title<-function(){

  txt<-paste0(getwd())
  if (grepl("minimum", txt, fixed = TRUE)==TRUE){
    txt<-stringr::str_replace(txt, "/minimum_metadata/minimum_metadata", "")
  } else{txt<-txt}
  x<-stringr::str_locate_all(txt,"/")
  stringr::str_sub(txt,x[[c(1,dim(x[[1]])[1])]]+1)

}

get_project_title()

#get_project_title()
