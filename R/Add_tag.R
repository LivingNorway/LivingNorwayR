#' add_Tag
#' Add metadata html tag (only run in RMarkdown)
#' @param tagtype The name of the EML element that will be tagged
#' @param txt The txt for the tag
#' @return Output: html tag
#' @export

add_Tag<-function(tag,txt, hidden=FALSE){
  #fmt="html_document"
  #fmt <- rmarkdown::default_output_format(knitr::current_input())$name # this needs to be in the RMArkdown file
  if(fmt=="html_document"){
    if(hidden==FALSE){
      LNtag=paste0("<span", " class=LN_",tag, ">", txt, "</span>")
      LNtag
    }else{
      LNtag=paste0("<span"," style='display:none' ", "class=LN_",tag, ">", txt, "</span>")
      LNtag
      }
      }else{
    cat(txt)
      }
  }

#add_Tag("Project", "ABC", hidden=TRUE)

#' LN_alternativeIdentifier
#' Add alternativeIdentifier metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_alternateIdentifier<-function(alternateIdentifier, hidden=FALSE){
  if(hidden==FALSE){
    add_Tag("alternateIdentifier", alternateIdentifier)
  }else{
    add_Tag("alternateIdentifier", alternateIdentifier, hidden=TRUE)
  }

}

#' LN_title
#' Add title metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_title<-function(title, hidden=FALSE){
  if(hidden==FALSE){
    add_Tag("title", title)
  }else{
    add_Tag("title", title, hidden = TRUE)
  }
}

#' LN_individualName
#' Add individualName metadata html tag (only run in RMarkdown)
#' @param firstName The givenName of the individual
#' @param secondName The surName of the individual
#' @return Output: html tag
#' @export

LN_individualName=function(firstName,secondName, hidden=FALSE){
  if(hidden==FALSE){
    tag=add_Tag("individualName", paste0(firstName," ", secondName))
    tag
  }else{
  tag=add_Tag("individualName", paste0(firstName," ", secondName), hidden=TRUE)
  tag
  }

}

#' LN_creator
#' Add creator metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export


LN_creator=function(individualName, organizationName=NULL,
                    positionName=NULL, deliveryPoint=NULL, city=NULL, postalCode=NULL,
                    electronicMail=NULL, order=NULL, hidden=FALSE){
  if(hidden==FALSE){
  #Need to have individualName
  tag1=individualName
  if(!is.null(organizationName)){
    tag2=add_Tag("organizationName",organizationName)
  }else{
    tag2=""
  }

  if(!is.null(positionName)){
    tag3=add_Tag("positionName",positionName)
}else{
  tag3=""
}
  if(!is.null(deliveryPoint)){
    tag4=add_Tag("deliveryPoint",deliveryPoint)
  }else{
    tag4=""
  }
  if(!is.null(city)){
    tag5=add_Tag("city",city)
  }else{
    tag5=""
  }
  if(!is.null(deliveryPoint)){
    tag6=add_Tag("deliveryPoint", deliveryPoint)
  }else{
        tag6=""
  }
  if(!is.null(postalCode)){
    tag7=add_Tag("postalCode",postalCode)
  }else{
    tag7=""
  }
  if(!is.null(electonicMail)){
    tag8=add_Tag("electonicMail",electonicMail)
  }else{
        tag8=""
      }
  if(!is.null(order)){
  tag9=add_Tag("creator", paste0(tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8, order))
  }else{
    tag9=add_Tag("creator", paste0(tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8))
      }
  tag9
  }else{
    #Need to have individualName
    tag1=individualName
    if(!is.null(organizationName)){
      tag2=add_Tag("organizationName",organizationName, hidden=TRUE)
    }else{
      tag2=""
    }

    if(!is.null(positionName)){
      tag3=add_Tag("positionName",positionName, hidden=TRUE)
    }else{
      tag3=""
    }
    if(!is.null(deliveryPoint)){
      tag4=add_Tag("deliveryPoint",deliveryPoint, hidden=TRUE)
    }else{
      tag4=""
    }
    if(!is.null(city)){
      tag5=add_Tag("city",city, hidden=TRUE)
    }else{
      tag5=""
    }
    if(!is.null(deliveryPoint)){
      tag6=add_Tag("deliveryPoint", deliveryPoint, hidden=TRUE)
    }else{
      tag6=""
    }
    if(!is.null(postalCode)){
      tag7=add_Tag("postalCode",postalCode, hidden=TRUE)
    }else{
      tag7=""
    }
    if(!is.null(electonicMail)){
      tag8=add_Tag("electonicMail",electonicMail, hidden=TRUE)
    }else{
      tag8=""
    }
    if(!is.null(order)){
      tag9=add_Tag("creator", paste0(tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8, order), hidden=TRUE)
    }else{
      tag9=add_Tag("creator", paste0(tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8), hidden=TRUE)
    }
    tag9
  }
}

#' metadataProvider
#' Add metadataProvider metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export


LN_metadataProvider=function(individualName, organizationName=NULL,
                             positionName=NULL, deliveryPoint=NULL, city=NULL, postalCode=NULL,
                             electonicMail=NULL, userId=NULL, hidden=FALSE){
 if(hidden==FALSE){
   #Need to have individualName
  tag1=individualName
if(!is.null(organizationName)){
  tag2=add_Tag("organizationName",organizationName)
}else{
  tag2=""
}
if(!is.null(positionName)){
  tag3=add_Tag("positionName",positionName)
}else{
  tag3=""
}
if(!is.null(deliveryPoint)){
  tag4=add_Tag("deliveryPoint",deliveryPoint)
}else{
  tag4=""
}
if(!is.null(city)){
  tag5=add_Tag("city",city)
}else{
  tag5=""
}
if(!is.null(deliveryPoint)){
  tag6=add_Tag("deliveryPoint", deliveryPoint)
}else{
  tag6=""
}
if(!is.null(postalCode)){
  tag7=add_Tag("postalCode",postalCode)
}else{
  tag7=""
}
if(!is.null(electonicMail)){
  tag8=add_Tag("electonicMail",electonicMail)
}else{
  tag8=""
}
if(!is.null(userId)){
  tag9=add_Tag("userID", paste0("directory =", userId))
}else{
  tag9=""
}

tag10=add_Tag("metadataProvider", paste0(tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9))
tag10
 }else{
   #Need to have individualName
   tag1=individualName
   if(!is.null(organizationName)){
     tag2=add_Tag("organizationName",organizationName, hidden=TRUE)
   }else{
     tag2=""
   }
   if(!is.null(positionName)){
     tag3=add_Tag("positionName",positionName, hidden=TRUE)
   }else{
     tag3=""
   }
   if(!is.null(deliveryPoint)){
     tag4=add_Tag("deliveryPoint",deliveryPoint, hidden=TRUE)
   }else{
     tag4=""
   }
   if(!is.null(city)){
     tag5=add_Tag("city",city, hidden=TRUE)
   }else{
     tag5=""
   }
   if(!is.null(deliveryPoint)){
     tag6=add_Tag("deliveryPoint", deliveryPoint, hidden=TRUE)
   }else{
     tag6=""
   }
   if(!is.null(postalCode)){
     tag7=add_Tag("postalCode",postalCode, hidden=TRUE)
   }else{
     tag7=""
   }
   if(!is.null(electonicMail)){
     tag8=add_Tag("electonicMail",electonicMail, hidden=TRUE)
   }else{
     tag8=""
   }
   if(!is.null(userId)){
     tag9=add_Tag("userID", paste0("directory =", userId), hidden=TRUE)
   }else{
     tag9=""
   }

   tag10=add_Tag("metadataProvider", paste0(tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9), hidden=TRUE)
   tag10
 }
}


#' pubDate
#' Add pubDate metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_pubDate=function(Date, hidden=FALSE){

  if(hidden==FALSE){
    #need to check format
    add_Tag("pubDate", Date)
  }else{
    add_Tag("pubDate", Date, hidden=TRUE)
  }

}

#' language
#' Add language metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export


LN_language=function(language, hidden=FALSE){
  if(hidden==FALSE){
  add_Tag("language", toupper(language))
  }else{
    add_Tag("language", toupper(language), hidden=TRUE)
  }
  }

#' abstract
#' Add abstract metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_abstract=function(abstract, hidden=FALSE){
  # need to check if abstract is text
  if(is.character(abstract)==FALSE){
    print("Error. The abstract needs to be text")
  }else{
    if(hidden==FALSE){
  add_Tag("abstract", abstract)
    }else{
        add_Tag("abstract", abstract, hidden=TRUE)
      }
  }
}


#' keywordSet
#' Add keywordSet metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_keywordSet=function(keyword, keywordThesaurus=NULL, hidden=FALSE){
  if(hidden==FALSE){
    tag1=add_Tag("keyword",keyword)
  if(!is.null(keywordThesaurus)){
    tag2=add_Tag("keywordThesaurus", keywordThesaurus)
  }else{
    tag2=""
  }
  tag3=add_Tag("keywordSet", paste0(tag1,tag2))
  tag3
  }else{
    tag3=add_Tag("keywordSet", paste0(tag1,tag2),hidden=TRUE)
    tag3
  }
}




#' intellectualRights
#' Add intellectualRights metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_intellectualRights=function(intellectualRights, hidden=FALSE){
  if(hidden==FALSE){
  add_Tag("intellectualRights",intellectualRights )
  }else{
    add_Tag("intellectualRights",intellectualRights, hidden=TRUE )
  }
  }


##Coverage
### geographicCoverage

#'boundingCoordinates
#' Add boundingCoordinates metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_boundingCoordinates=function(westBoundingCoordinate, eastBoundingCoordinate, northBoundingCoordinate,
                                southBoundingCoordinate, hidden=FALSE){
if(hidden==FALSE){
  tag1=add_Tag("westBoundingCoordinate", westBoundingCoordinate)
  tag2=add_Tag("eastBoundingCoordinate", eastBoundingCoordinate)
  tag3=add_Tag("northBoundingCoordinate", southBoundingCoordinate)
  tag4=add_Tag("southBoundingCoordinate", northBoundingCoordinate)

  tag5=add_Tag("boundingCoordinates",  paste0(tag1,tag2,tag3,tag4))

  tag5
}else{
  tag1=add_Tag("westBoundingCoordinate", westBoundingCoordinate, hidden=TRUE)
  tag2=add_Tag("eastBoundingCoordinate", eastBoundingCoordinate, hidden=TRUE)
  tag3=add_Tag("northBoundingCoordinate", southBoundingCoordinate, hidden=TRUE)
  tag4=add_Tag("southBoundingCoordinate", northBoundingCoordinate, hidden=TRUE)

  tag5=add_Tag("boundingCoordinates",  paste0(tag1,tag2,tag3,tag4), hidden=TRUE)

  tag5
}
}

#' geographicCoverage
#' Add geographicCoveragemetadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_geographicCoverage=function(geographicDescription, boundingCoordinates, hidden=FALSE){
if(hidden==FALSE){
tag1=add_Tag("geographicDescription",geographicDescription)
tag2=add_Tag("geographicCoverage", paste0(tag1, boundingCoordinates))
}else{
  tag1=add_Tag("geographicDescription",geographicDescription, hidden=TRUE)
  tag2=add_Tag("geographicCoverage", paste0(tag1, boundingCoordinates, hidden=TRUE))

}
}

#' TemporalCoverage
#' Add TemporalCoverage html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_temporalCoverage=function(beginDate,endDate, hidden=FALSE){

if(hidden==FALSE){
  tag1=add_Tag("calendarDate", beginDate)
  tag2=add_Tag("calendarDate", endDate)
  tag3=add_Tag("beginDate", paste0(tag1))
  tag4=add_Tag("endDate", paste0(tag2))
  tag5=add_Tag("rangeOfDates", paste0(tag3, tag4))
  tag5
}else{
  tag1=add_Tag("calendarDate", beginDate, hidden=TRUE)
  tag2=add_Tag("calendarDate", endDate, hidden=TRUE)
  tag3=add_Tag("beginDate", paste0(tag1), hidden=TRUE)
  tag4=add_Tag("endDate", paste0(tag2), hidden=TRUE)
  tag5=add_Tag("rangeOfDates", paste0(tag3, tag4), hidden=TRUE)
  tag5
  }

}

#' TaxonomicCoverage
#' Add TaxonomicCoverage html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_taxonomicCoverage=function(taxonRankName, taxonRankValue, commonName, hidden=FALSE){

  if(hidden==FALSE){
    tag1=add_Tag("taxonRankValue",taxonRankValue)
    tag2=add_Tag("taxonRankName",taxonRankName)
    tag3=add_Tag("commonName", commonName)
    tag4=add_Tag("taxonomicClassification", paste0(tag2, tag1,tag3))
    tag4
  }else{
  tag1=add_Tag("taxonRankValue",taxonRankValue, hidden=TRUE)
  tag2=add_Tag("taxonRankName",taxonRankName, hidden=TRUE)
  tag3=add_Tag("commonName", commonName, hidden=TRUE)
  tag4=add_Tag("taxonomicClassification", paste0(tag2, tag1,tag3), hidden=TRUE)
  tag4
  }
}
#' coverage
#' Add coverage html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export

LN_coverage=function(geographicCoverage, temporalCoverage, taxonomicCoverage, hidden=FALSE){
  if(hidden==FALSE){
    tag1=add_Tag("coverage", paste0(geographicCoverage,temporalCoverage, temporalCoverage))
    tag1
  }else{
    tag1=add_Tag("coverage", paste0(geographicCoverage,temporalCoverage, temporalCoverage), hidden=TRUE)
    tag1
    }
  }
