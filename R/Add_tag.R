#some tags not needed
# anchortag - orcidID no text output
# set attributes
# empty span tags
#abstract, methods description, project description - long forms
# markdown LN functions
#abstract or summary acceptable
#begin and end tags
#mask/hidden
# Dates for workshop


#' add_Tag
#' Add metadata html tag (only run in RMarkdown)
#' @param tagtype The name of the EML element that will be tagged
#' @param txt The txt for the tag
#' @return Output: html tag
#' @export



add_Tag<-function(tag,txt){
  #fmt <- rmarkdown::default_output_format(knitr::current_input())$name # this needs to be in the RMArkdown file
  if(fmt=="html_document"){
      LNtag=paste0("<", "LN_",tag, ">", txt, "</", "LN_",tag, ">")
      LNtag
      }else{
    cat(txt)
      }
  }

#add_tag("Project", "ABC")

#' LN_alternativeIdentifier
#' @return Output: html tag
#' @export

LN_alternateIdentifier<-function(alternateIdentifier){
  add_Tag("alternateIdentifier", alternateIdentifier)
}

#' LN_title
#' @return Output: html tag
#' @export

LN_title<-function(title){
  add_Tag("title", title)
}

#' LN_individualName
#' Add individualName metadata html tag (only run in RMarkdown)
#' @param firstName The givenName of the individual
#' @param secondName The surName of the individual
#' @return Output: html tag
#' @export

LN_individualName=function(firstName,secondName){
  tag1=add_Tag("givenName",firstName)
  tag2=add_Tag("surName", secondName)
  tag3=add_Tag("individualName", paste0(tag1,tag2))

  #tag3=paste0("<", "individualName", ">", tag1,tag2, "</", "individualName", ">")
  #tag3
}

#' LN_creator
#' Add creator metadata html tag (only run in RMarkdown)
#' @return Output: html tag
#' @export



LN_creator=function(individualName, organizationName=NULL,
                    positionName=NULL, deliveryPoint=NULL, city=NULL, postalCode=NULL,
                    electonicMail=NULL, order=NULL){
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

}

#' metadataProvider
#' @return Output: html tag
#' @export


LN_metadataProvider=function(individualName, organizationName=NULL,
                             positionName=NULL, deliveryPoint=NULL, city=NULL, postalCode=NULL,
                             electonicMail=NULL, userId=NULL){
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
}


#' pubDate
#' @return Output: html tag
#' @export

LN_pubDate=function(Date){

  #need to check format

  add_Tag("pubDate", Date)
}

#' language
#' @return Output: html tag
#' @export


LN_language=function(language){
  add_Tag("language", toupper(language))
}

#' abstract
#' @return Output: html tag
#' @export

LN_abstract=function(abstract){
  # need to check if abstract is text
  if(abstract)
  add_Tag("abstract", abstract)
}


#' keywordSet
#' @return Output: html tag
#' @export

LN_keywordSet=function(keyword, keywordThesaurus=NULL){
  tag1=add_Tag("keyword",keyword)
  if(!is.null(keywordThesaurus)){
    tag2=add_Tag("keywordThesaurus", keywordThesaurus)
  }else{
    tag2=""
  }
  tag3=add_Tag("keywordSet", paste0(tag1,tag2))
  tag3
  }

#' intellectualRights
#' @return Output: html tag
#' @export

LN_intellectualRights=function(intellectualRights){
  add_Tag("intellectualRights",intellectualRights )
}

#' coverage
#' @return Output: html tag
#' @export

<coverage>
  <geographicCoverage>
  <geographicDescription>Study area mainly in central Norway (Lierne and SnÃ¥sa municipalities). Some re-locations of radio-collared ptarmigans in Sweden are included in the data set.</geographicDescription>
  <boundingCoordinates>
  <westBoundingCoordinate>12.659</westBoundingCoordinate>
  <eastBoundingCoordinate>14.653</eastBoundingCoordinate>
  <northBoundingCoordinate>64.774</northBoundingCoordinate>
  <southBoundingCoordinate>64.033</southBoundingCoordinate>
  </boundingCoordinates>
  </geographicCoverage>
  <temporalCoverage>
  <rangeOfDates>
  <beginDate>
  <calendarDate>      2012-02-14
</calendarDate>
  </beginDate>
  <endDate>
  <calendarDate>      2014-09-17
</calendarDate>
  </endDate>
  </rangeOfDates>
  </temporalCoverage>
  <taxonomicCoverage>
  <taxonomicClassification>
  <taxonRankName>Species</taxonRankName>
  <taxonRankValue>Lagopus muta</taxonRankValue>
  <commonName>Rock ptarmigan</commonName>
  </taxonomicClassification>
  </taxonomicCoverage>
  </coverage>



