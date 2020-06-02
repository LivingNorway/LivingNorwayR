#' map_folders
#' Get a list of files in the data folders
#' @param project_name the project name
#' @return Output: List of data files, and potentially a description of the field names/types.
#' @export


map_folders<-function(project_name){
  path<-paste0(getwd(),"/", project_name)
  dirslist<-list.dirs(path,recursive = TRUE, full.names = TRUE)
  x <- lapply(strsplit(dirslist, "/"), function(z) as.data.frame(t(z)))
  LivingNorway_project <- plyr::rbind.fill(x)
  y=dim(LivingNorway_project)[2]
  p <- collapsibleTree::collapsibleTree( LivingNorway_project, c(names(LivingNorway_project)[y-1], names(LivingNorway_project)[y]))
  p
  }

#' map_files
#' Get a list of files in the data folders
#' @param project_name the project name
#' @return Output: List of data files, and potentially a description of the field names/types.
#' @export

map_files<-function(project_name){
  path<-paste0(getwd(),"/", project_name)
  dirslist<-list.files(path,recursive = TRUE, full.names = TRUE)
  x <- lapply(strsplit(dirslist, "/"), function(z) as.data.frame(t(z)))
  LivingNorway_project <- plyr::rbind.fill(x)
  y=dim(LivingNorway_project)[2]
  p <- collapsibleTree::collapsibleTree( LivingNorway_project, c(names(LivingNorway_project)[y-2], names(LivingNorway_project)[y-1], names(LivingNorway_project)[y]))
  p
}

