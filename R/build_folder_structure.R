#' Build_folder_structure
#' This function builds the data folder structure in the working directory
#'
#' @param project_name The name of the project (it can not contain spaces; use "_" instead)
#' @return A set of folders and a rmarkdown metadata template
#' @export


build_folder_structure<-function(project_name=project_name){
  neet::assert_neet(project_name, "character")

  if(dir.exists(project_name)==TRUE) {
    print("Directory already exists please check the name and retry")
  } else{
    dir.create(paste0(getwd(),"/", project_name))
    dir.create(paste0(getwd(),"/", project_name,"/", "metadata"))
    dir.create(paste0(getwd(),"/",project_name,"/", "data"))
    dir.create(paste0(getwd(),"/", project_name,"/","data", "/", "raw_data"))
    dir.create(paste0(getwd(),"/", project_name,"/","data", "/", "mapped_data"))
    dir.create(paste0(getwd(),"/", project_name,"/","data", "/", "scan_data"))
    dir.create(paste0(getwd(),"/", project_name,"/","scripts"))
    dir.create(paste0(getwd(),"/", project_name,"/","meta_xml"))
    dir.create(paste0(getwd(),"/", project_name,"/","dmp"))
    rmarkdown::draft(paste0(getwd(),"/", project_name,"/", "metadata", "/","metadata.Rmd"),
                     template="metadata", package="LivingNorwayR", edit=FALSE)

      }

}

#build_folder_structure(project_name = "Test")


#NEED TO ADD TEST
#library(neet)
# expected type will be a folder - what is this as an R object
# check list.files function documentation

