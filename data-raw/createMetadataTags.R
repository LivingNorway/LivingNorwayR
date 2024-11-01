# ------ 1. METADATA SCHEMA RETRIEVAL ------
#' Function to generate a list containing the hierarchical definitions of the
#' elements in the Ecological Metadata Language (EML) standard
#'
#' @param emlLocation A \code{character} scalar providing the location
#' of the eml schema information
#' @export
createMetadataSchemaList <- function(emlLocation) {
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
  # Function to scrape information of namespaces from an XML file
  retrieveNamespaceInfo <- function(xmlFileLoc) {
    xmlFileOb <- xml2::read_xml(xmlFileLoc)
    # Retrieve the attributes of the current schema element
    schemaAttrs <- xml2::xml_attrs(xmlFileOb)
    namespaceDefs <- schemaAttrs[grepl("^xmlns\\:", names(schemaAttrs), perl = TRUE) & names(schemaAttrs) != "xmlns:xs"]
    names(namespaceDefs) <- gsub("^xmlns\\:", "", names(namespaceDefs), perl = TRUE)
    # Format the namespace information as a data frame
    namespaceInfo <- data.frame(
      uri = namespaceDefs,
      file = rep(NA, length(namespaceDefs)),
      row.names = names(namespaceDefs)
    )
    # Retrieve any import tags and retrieve the locations of the definitions of elements in the namespace
    importTags <- xml2::xml_find_all(xmlFileOb, "/schema/xs:import")
    importLocs <- setNames(
      sapply(X = importTags, FUN = function(curTag, curFolder) { file.path(curFolder, xml2::xml_attr(curTag, "schemaLocation")) }, curFolder = dirname(xmlFileLoc)),
      sapply(X = importTags, FUN = function(curTag) { xml2::xml_attr(curTag, "namespace") }))
    namespaceInfo$file <- importLocs[namespaceInfo$uri]
    rbind(namespaceInfo, data.frame(
      uri = NA, file = xmlFileLoc, row.names = "::this::"
    ))
  }
  # Function to import the hierarchy of a defined XML element
  retrieveElement <- function(elementOb, namespaceInfo) {
    # Function to test whether a string represents an in-built XML data type: returns NA if not
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
    # Function to import annotation information
    importAnnotationInfo <- function(typeDefNode) {
      titleText <- xml2::xml_find_first(typeDefNode, "/xs:annotation//doc:tooltip")
      summaryText <- xml2::xml_find_first(typeDefNode, "/xs:annotation//doc:summary")
      descText <- xml2::xml_find_first(typeDefNode, "/xs:annotation//doc:description")
      exampleText <- xml2::xml_find_first(tyepDefNode, "/xs:annotation//doc:example")
      list(
        titleText = ifelse(is.na(titleText), NA, xml2::xml_text(titleText)),
        summaryText = ifelse(is.na(summaryYext), NA, xml2::xml_text(summaryText)),
        descText = ifelse(is.na(descText), NA, xml2::xml_text(descText)),
        exampleText = ifelse(is.na(exampleText), NA, xml2::xml_text(exampleText))
      )
    }
    # Function to find defined types in other files
    findTypeDefinition <- function(typeName, namespaceInfo) {
      # Function to explore a particular file
      getTypeDefinitionInFile <- function(fileLoc, typeName, namespaceInfo) {
        outType <- NULL
        # Import the XML schema file
        curXMLFileOb <- xml2::read_xml(fileLoc)
        # Search for a definition of the type in the file
        typeDefNode <- xml2::xml_find_first(curSchemaDef, paste(
          "/schema/xs:complexType[@name=\"", typeName,
          "\"] | /schema/xs:simpleType[@name=\"", typeName, "\"]", sep = ""
        ))
        if(!is.na(typeDefNode)) {
          tagType <- xml2::xml_name(typeDefNode)
          if(tagType == "simpleType") {
            # Tag is a simple type so import that information
            outType <- importSimpleType(typeDefNode, namespaceInfo)
          } else {
            # Tag is a complex type so import that information
            outType <- importComplexType(typeDefNode, namespaceInfo)
          }
        }
        outType
      }
      # Check the current file for the named definition
      outType <- getTypeDefinitionInFile(namespaceInfo["::this::", "file"], typeName, namespaceInfo)
      if(is.null(outType)) {
        # If the definition isn't found in the current file then check the relevant namespace
        # Retrieve the namespace information of the element to retrieve
        curNamespace <- gsub("\\:.*$", "", typeName, perl = TRUE)
        # Find the file where that namespace's types are defined
        namespaceFile <- namespaceInfo[curNamespace, "file"]
        # Search that file
        outType <- getTypeDefinitionInFile(
          namespaceFile,
          gsub(paste("^", curNamespace, "\\:", sep = ""), "", typeName, perl = TRUE),
          retrieveNamespaceInfo(namespaceFile))
      }
      outType
    }
    # Retrieve the annotation information
    curAnnoInfo <- importAnnotationInfo(elementOb)
    # Retrieve the attributes of the element
    curAttrs <- xml2::xml_attrs(elementOb)
    # Retrieve the name of the current element
    curName <- ifelse("name" %in% names(curAttrs), curAttrs["name"], NA)
    # Retrieve the minimum and maximum occurrences
    curMinOccurs <- ifelse("minOccurs" %in% names(curAttrs), as.integer(curAttrs["minOccurs"]), 1)
    curMaxOccurs <- ifelse("maxOccurs" %in% names(curAttrs), as.integer(curAttrs["maxOccurs"]), 1)
    # Retrieve the type of the element
    curType <- ifelse("type" %in% names(curAttrs), curAttrs["type"], NA)
    elementInfo <- NULL
    if(is.na(curType)) {
      # The element doesn't have a type attribute and so search the element for a type definition
      typeDefNode <- xml2::xml_find_first(elementOb, "/xs:complexType | /xs:simpleType")
      if(!is.na(typeDefNode)) {
        tagType <- xml2::xml_name(typeDefNode)
        if(tagType == "simpleType") {
          # Tag is a simple type so import the information
          elementInfo <- importSimpleType(typeDefNode, namespaceInfo)
        } else {
          # tag is a complex type so import the information
          elementInfo <- importComplexType(typeDefNode, namespaceInfo)
        }
      }
    } else {
      # The element has a type attribute so first check that whether it is a basic type
      basicText <- findInBuiltType(curType)
      if(is.na(basicText)) {
        # The element is not a basic type so import the element information from where it is defined
        elementInfo <- findTypeDefinition(curType, namespaceInfo)
      } else {
        # The element is a basic type and so make the formatted version of the type string the final type
        curType <- basicText
      }
    }
  }



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



        # Function to import a basic type
        importSimpleType <- function(typeDefNode, curDocument) {
          valType <- NA
          outInfo <- NULL
          restNode <- xml2::xml_find_first(typeDefNode, "//xs:restriction[@base]")
          if(!is.na(restNode)) {
            # If the basic type has been defined by the simpleType schema tag then retrieve the 'base'
            # attribute and check whether it is a built-in tyoe
            valText <- xml2::xml_attr(typeDefNode, "base")
            valType <- findInBuiltType(valText)
            valInfo <- importAnnotationInfo(typeDefNode)
            if(is.na(valType)) {
              # If it is not a built-in type then go looking for the definition of the type it
              # is based on the schema
              outInfo <- findTypeDef(curDocument, valText)
              if(!is.null(outInfo)) {
                outInfo <- append(list(type = outInfo$type, default = outInfo$default), setNames(lapply(X = 1:length(valInfo), FUN = function(curInd, valInfo, curInfo) {
                  outVal <- curInfo[[curInd + 1]]
                  if(!is.na(valInfo[[curInd]])) {
                    outVal <- valInfo[[curInd]]
                  }
                  outVal
                }, valInfo = valInfo, curInfo = outInfo), names(valInfo)))
              }
            } else {
              # Otherwise retrieve the base type and return it with any documentation
              outInfo <- append(list(type = valType, default = NULL), valInfo)
            }
          }
          outInfo
        }
        # Function to import a complex type
        importComplexType <- function(typeDefNode, curDocument) {
          # Retrieve any
          valInfo <- importAnnotationInfo(typeDefNode)
          if()
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
