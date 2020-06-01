#' get_files
#' Get a list of files in the data folders
#' @param project_name the project name
#' @return Output: temporal extent chart?List of data files, and potentially a description of the field names/types.
#' @export


get_files<-function(project_name){
  path<-paste0(getwd(),"/", project_name)
  dirslist<-list.dirs(path,recursive = TRUE)
  x <- lapply(strsplit(dirslist, "/"), function(z) as.data.frame(t(z)))
  LivingNorway_project <- plyr::rbind.fill(x)
  y=dim(LivingNorway_project)[2]
  p <- collapsibleTree::collapsibleTree( LivingNorway_project, c("V8", "V9"))
  p
  }


