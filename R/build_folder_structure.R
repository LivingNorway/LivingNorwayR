#' Build_folder_structure
#' This function builds the data folder structure in the working directory
#'
#' @param project_name The name of the project (it can not contain spaces; use "_" instead)
#' @return A set of folders and a rmarkdown metadata template
#'@example
#'\dontrun{
#'build_folder_structure(project_name = "My_first_LN_Project")
#'}
#' @export


build_folder_structure<-function(project_name=project_name){

  if(dir.exists(project_name)==TRUE) {
    print("Directory already exists please check the name and retry")
  } else{
    dir.create(paste0(getwd(),"/", project_name))
    dir.create(paste0(getwd(),"/", project_name,"/", "metadata"))
    dir.create(paste0(getwd(),"/",project_name,"/", "data"))
    dir.create(paste0(getwd(),"/", project_name,"/","data", "/", "raw"))
    dir.create(paste0(getwd(),"/", project_name,"/","data", "/", "derived"))
    dir.create(paste0(getwd(),"/", project_name,"/","data", "/", "scan"))
    dir.create(paste0(getwd(),"/", project_name,"/","scripts"))
    dir.create(paste0(getwd(),"/",project_name,"/", "dwc_a"))
    dir.create(paste0(getwd(),"/", project_name,"/","dwc_a", "/", "meta_xml"))
    dir.create(paste0(getwd(),"/", project_name,"/","dmp"))
    rmarkdown::draft(paste0(getwd(),"/", project_name,"/", "metadata", "/","metadata.Rmd"),
                     template="metadata", package="LivingNorwayR", edit=FALSE)

      }

}



