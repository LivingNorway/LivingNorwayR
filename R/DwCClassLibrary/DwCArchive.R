# Container class for Darwin Core components that can be used to define a archive
setRefClass(
  Class = "DwCArchive",
  fields = list(
    componentList = "list"
  ),
  methods = list(
    # Function to export the object to a Darwin core archive file structure
    exportToArchive = function(loc) {
      # Retrieve a list of data types in DwCArchive object
      obTypes <- unique(sapply(X = componentList, FUN = class))
    }
  )
)
