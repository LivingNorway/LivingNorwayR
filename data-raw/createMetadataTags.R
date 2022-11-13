# ------ 1. METADATA TAG GENERATION FUNCTION ------
#' Function to generate EML metadata tags for use in markdown documents
#'
#' @param emlLocation A \code{character} scalar providing the location
#' of the eml schema information
#' @param fileLocation A \code{character} scalar providing the location
#' to store the generated function source files
#' @export
createMetadataTagFunctions <- function(emlLocation, fileLocation) {
  inEMLLocation <- tryCatch(as.character(emlLocation), error = function(err) {
    stop("error importing EML schema information: ", err)
  })
  if(length(inEMLLocation) > 1) {
    inEMLLocation <- inEMLLocation[1]
    warning("EML schema input location has a length greater than one: only the first element will be used")
  } else if(length(inEMLLocation) <= 0) {
    stop("error importing EML schema information: vector length of input parameter is zero")
  }
  if(is.na(inEMLLocation) || inEMLLocation == "") {
    stop("error importing EML schema information: invalid location for schema given")
  }
  # Create a temporary location to store intermediate products
  tempLoc <- file.path(tempdir(), paste("emlProcess", gsub("[\\-\\s\\:]+", "_", Sys.time(), perl = TRUE), sep = "_"))
  if(!dir.exists(tempLoc)) {
    dir.create(tempLoc)
  }
  # Download and unpack the EML specification
  download.file(inEMLLocation, file.path(tempLoc, "EML.tar.gz"), method = "libcurl")
  untar(file.path(tempLoc, "EML.tar.gz"), exdir = tempLoc)
  # Read in the EML specification
  emlLoc <- list.files(tempLoc, file.path("eml\\.xsd"), recursive = TRUE)
  emlLoc <- file.path(tempLoc, emlLoc[length(emlLoc)])
  # Function to search the imported schemas for a type definition
  findTypeDef <- function(srcLoc, typeName) {
    # Read in the XML file
    curXMLOb <- xml2::read_xml(srcLoc)
    # Retrieve each of the schema elements contained in the source file
    schemaDefinitions <- xml2::xml_find_all(curXMLOb, "/xs:schema")
    lapply(X = schemaDefinitions, FUN = function(curSchemaDef, curFolder, typeName) {
      # Retrieve the attributes of the current schema element
      schemaAttrs <- xml2::xml_attrs(curSchemaDef)
      namespaceDefs <- schemaAttrs[grepl("^xmlns\\:", names(schemaAttrs), perl = TRUE) & names(schemaAttrs) != "xmlns:xs"]
      names(namespaceDefs) <- gsub("^xmlns\\:", "", names(namespaceDefs), perl = TRUE)
      # Format the namespace information as a data frame
      namespaceInfo <- data.frame(
        uri = namespaceDefs,
        file = rep(NA, length(namespaceDefs)), row.names = names(namespaceDefs),
        isRelevant = sapply(X = names(namespaceDefs), FUN = function(curName, typeName) {
          grepl(paste("^", curName, "\\:", sep = ""), typeName, perl = TRUE)
        }, typeName = typeName))
      # Retrieve any import tags
      importTags <- xml2::xml_find_all(curSchemaDef, "./xs:import")
      importLocs <- setNames(
        sapply(X = importTags, FUN = function(curTag, curFolder) { file.path(curFolder, xml2::xml_attr(curTag, "schemaLocation")) }, curFolder = curFolder),
        sapply(X = importTags, FUN = function(curTag) { xml2::xml_attr(curTag, "namespace") }))
      namespaceInfo$file <- importLocs[namespaceInfo$uri]
      namespaceInfo$isRelevant <- namespaceInfo$isRelevant & !is.na(namespaceInfo$file)
      outValue <- NULL
      if(any(namespaceInfo$isRelevant)) {
        # If the type to find is in a namespace then import the definition from the
        # the relevant import file instead
        nameIndex <- which(namespaceInfo$isRelevant)[1]
        outValue <- findTypeDef(
          namespaceInfo$file[nameIndex],
          gsub(paste("^", row.names(namespaceInfo)[nameIndex], "\\:", sep = ""), "", typeName, perl = TRUE))
      }
      # If the element is not in an imported file then retrieve the element directly
      if(is.null(outValue)) {
        # Otherwise search for the relevant definition within the current import file
        typeDefNodes <- xml2::xml_find_first(curSchemaDef, paste(
          "./xs:element[@name=\"", typeName,
          "\"] | //xs:complexType[@name=\"", typeName,
          "\"] | //xs:simpleType[@name=\"", typeName, "\"]", sep = ""
        ))
        # Function to test whether a string represents an in-built XML data type
        findInBuiltType <- function(inType) {
          outType <- gsub("^xs\\:", "", inType, perl = TRUE)
          # Check the input against any of the XML predefined types
          if(!(outType %in% c(
            "anysimpleType",
            "duration", "dateTime", "time", "date", "gYearMonth", "gYear", "gMonth", "gDay", "gMonth",
            "boolean", "base64binary", "hexbinary", "float", "double", "anyURI", "QName", "NOTATION",
            "string", "decimal",
            "normalizedString", "integer",
            "token", "nonPositiveInteger", "long", "nonNegativeInteger",
            "language", "Name", "NMTOKEN", "negativeInteger", "int", "unsignedLong", "positiveInteger",
            "NCName", "NMTOKENS", "short", "unsignedInt",
            "ID", "IDREF", "ENTITY", "byte", "unsignedShort",
            "IDREFS", "ENTITIES", "unsignedByte"
          ))) {
            outType <- NA
          }
          outType
        }
        # Function to import annotation informationa
        importAnnotationInfo <- function(typeDefNode) {
          titleText <- xml2::xml_find_first(typeDefNode, "//xs:annotation//doc:tooltip")
          summaryText <- xml2::xml_find_first(typeDefNode, "//xs:annotation//doc:summary")
          descText <- xml2::xml_find_first(typeDefNode, "//xs:annotation//doc:description")
          exampleText <- xml2::xml_find_first(tyepDefNode, "//xs:annotation//doc:example")
          list(
            titleText = ifelse(is.na(titleText), NA, xml2::xml_text(titleText)),
            summaryText = ifelse(is.na(summaryYext), NA, xml2::xml_text(summaryText)),
            descText = ifelse(is.na(descText), NA, xml2::xml_text(descText)),
            exampleText = ifelse(is.na(exampleText), NA, xml2::xml_text(exampleText))
          )
        }
        # Function to import a basic type
        importSimpleType <- function(typeDefNode, curDocument) {
          valType <- NA
          outInfo <- NULL
          restNode <- xml2::xml_find_first(typeDefNode, "//xs:restriction[@base]")
          if(!is.na(restNode)) {
            # If the basic type has been defined by the simpleType schema tag then retrieve the 'base'
            # attribute and check whether it is a built-in tyoe
            valText <- xml2::xml_attr(typeDefNode, "base")
            valType <- findInBuiltType(valText)sa
            valInfo <- importAnnotationInfo(typeDefNode)
            if(is.na(valType)) {
              # If it is not a built-in type then go looking for the definition of the type it
              # is based on the schema
              outInfo <- findTypeDef(curDocument, valText)
              if(!is.null(outInfo)) {
                outInfo <- append(list(type = outInfo$type), setNames(lapply(X = 1:length(valInfo), FUN = function(curInd, valInfo, curInfo) {
                  outVal <- curInfo[[curInd + 1]]
                  if(!is.na(valInfo[[curInd]])) {
                    outVal <- valInfo[[curInd]]
                  }
                  outVal
                }, valInfo = valInfo, curInfo = outInfo), names(valInfo)))
              }
            } else {
              # Otherwise retrieve the base type and return it with any documentation
              outInfo <- append(list(type = valType), valInfo)
            }
          }
          outInfo
        }
        # Function to import a complex type
        importComplexType <- function(typeDefNode) {

        }
        if(!is.na(typeDefNodes)) {
          if(xml2::xml_name(typeDefNodes) == "xs:element") {
            # Process the element
          } else if(xml2::xml_name(typeDefNodes) == "xs:complexType") {
            # Process the complex type
            outValue <- importComplexType(typeDefNodes)
          } else {
            # Process the basic type
            outValue <- importBasicType(typeDefNodes)
          }
        }
      }
      outValue
    }, curFolder = dirname(srcLoc), typeName = typeName)
  }
}
