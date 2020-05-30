# Workflow example


# Are libraries installed? ------------------------------------------------

list.of.packages <- c("TheDataPackage", "tidyverse", "here", "PBSmodelling")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Load libraries ----------------------------------------------------------
library(TheDataPackage)
library(tidyverse)
library(here)
#library(PBSmodelling)


# build folder structure --------------------------------------------------

project_name= readline(prompt="Please tell me your project name (remember that R does not like spaces so use '_' instead of a space): ")
build_folder_structure(project_name)


# Load raw data -----------------------------------------------------------

raw_data<-read.csv(choose.files())
write.csv(raw_data, here(paste0(project_name,"/data","/raw_data/raw_data.csv")))

# Load data management plan
# Need to add functions
#


# open minimum metadata

#PBSmodelling::openFile(fname=here(paste0(project_name,"/minimum_metadata","/minimum_metadata/minimum_metadata.Rmd")))
