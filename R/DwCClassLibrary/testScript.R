objectModelSourceLoc <- paste(Sys.getenv("WORKSPACE_LIVINGNORWAY_DATAPACKAGE"), "R", "DwCClassLibrary", sep = "/")
source(paste(objectModelSourceLoc, "DwCComponent.R", sep = "/"))
source(paste(objectModelSourceLoc, "DwCRecordLevel.R", sep = "/"))
source(paste(objectModelSourceLoc, "DwCOccurrence.R", sep = "/"))
source(paste(objectModelSourceLoc, "DwCOrganism.R", sep = "/"))
source(paste(objectModelSourceLoc, "DwCMaterialSample.R", sep = "/"))
source(paste(objectModelSourceLoc, "DwCEvent.R", sep = "/"))
source(paste(objectModelSourceLoc, "DwCLocation.R", sep = "/"))

