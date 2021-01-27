# ------ 1. TEST ARCHIVE IMPORT/EXPORT FUNCTIONALITY ------
# Unit testing to make sure that Darwin Core Archive object can import data from a data frame, create a
# Darwin core archive file from it and the go back the other way without losing information
thestthat::test_that("Darwin core archive object can import and export losslessly (event core)", {
  # ====== 1.1. Create test data ======
  # Initialise two data frames representing test data to convert into a Darwin core archive
  # Initialise a data frame to be the core type
  testCoreFrame <- data.frame(
    idCode = c(1233:1236),
    lat = c(60.418292, 60.38143, 63.41407, 59.82642),
    long = c(5.234457, 5.33081, 10.40666, 10.69530)
  )
  # Initialise a data frame to be the extension type
  testExtFrame <- data.frame(
    idCode = 1:4,
    relatedTo = testCoreFrame$idCode,
    count = c(4, 6, 8, 9),
    valType = rep("count", 4)
  )
  # ====== 1.2. Convert test data to augmented frames ======
  eventCoreOb <- initializeGBIFEvent(testCoreFrame, "idCode", eventID = "idCode", decimalLatitude = "lat", decimalLongitude = "long")
  measurementExtOb <- initializeGBIFMeasurementOrFact(testExtFrame, "relatedTo", measurementID = "idCode", measurementUnit = "valType", measurementValue = "count")
  # ====== 1.3. Build the Darwin core archive ======
  archiveOb <- DwCArchive$new(eventCoreOb, measurementExtOb)
  # Create a temporary location to store the Darwin core archive file
  testLoc <- paste(tempfile(), ".zip", sep = "")
  archiveOb$exportAsDwCArchive(testLoc)
  # ===== 1.4. Import from the Darwin core archive ======
  inArchiveOb <- DwCArchive$new(testLoc)
  # ===== 1.5. Retrieve the original data frames from the Darwin core archive ======
  outTestCoreFrame <- inArchiveOb$getCoreTable()$exportAsDataFrame()
  outTestExtFrame <- inArchiveOb$getExtensionTables()[[1]]$exportAsDataFrame()
})
