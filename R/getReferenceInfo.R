#' Get DS References for a park-species combination
#'
#' @description \code{get.parkTaxonRefs} returns a data frame of metadata for references in Data Store corresponding to a particular taxon at a park.
#' The resultant data frame is then usable by other functions to extract metadata from the records.
#'
#' Note that this function only returns references that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @param ParkCode The four-letter unit code for the park of interest.
#' @param TaxonCode Taxonomic Serial Number for the taxon of interest.
#'
#' @examples
#' \dontrun{
#' get.parkTaxonRefs("APIS", 126749)
#' }
get.parkTaxonRefs<-function(ParkCode,TaxonCode){
  url<-paste("https://irmaservices.nps.gov/datastore-secure/v4/rest/UnitSpeciesSearch/",ParkCode,"/",TaxonCode,sep="")
  DSReference<-read_json(url,simplifyVector = TRUE)
  return(DSReference)
}

#' Get DataStore Citations for a park-species combination
#'
#' @description \code{get.parkTaxonCitations} returns a vector of citations in Data Store corresponding to a particular taxon at a park.
#'
#' Note that this function only returns citations that have been "tagged" with a taxon code and may represent only a subset of references that have information about a taxon.
#'
#' @param ParkCode The four-letter unit code for the park of interest.
#' @param TaxonCode Taxonomic Serial Number for the taxon of interest.
#' 
#' @return a dataframe
#'
#' @examples
#' \dontrun{
#' get.parkTaxonCitations("APIS", 126749)
#' }
get.parkTaxonCitations<-function(ParkCode,TaxonCode){
  references<-get.parkTaxonRefs(ParkCode,TaxonCode)
  citations<-References$citation
  return(citations)
}

#' Get URL for references for a park-species combination
#'
#' \code{get.parkTaxonURL} returns a vector of URLs for references in Data Store corresponding to a particular taxon at a park.
#'
#' Note that this function only returns URLs for references that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @param ParkCode The four-letter unit code for the park of interest.
#' @param TaxonCode Taxonomic Serial Number for the taxon of interest.
#'
#' @examples
#' \dontrun{
#' get.parkTaxonURLs("APIS", 126749)
#' }
get.parkTaxonURLs<-function(ParkCode,TaxonCode){
  references<-get.parkTaxonRefs(ParkCode,TaxonCode)
  referenceCodes<-references$referenceId
  URLs<-paste("https://irma.nps.gov/DataStore/Reference/Profile/", referenceCodes,sep="")
  return(URLs)
}

#' Get citation for Data Store holding info by HoldingID
#'
#' @description \code{get.refInfo(HoldingID,field)} returns a character string or a vector with information from one of the
#' metadata fields in a Data Store reference's associated xml file.
#'
#'
#' @param HoldingID The six-seven digit reference / holding ID number unique to the data store record.
#' @param field is one of the following: "Title" returns the title of the data store reference as a string value;
#' "Abstract" returns the abstract as a string value; "Citation" returns the citation as a string value, and "Keywords" returns a vector containing
#' all keywords as character values.
#'
#' @examples
#' \dontrun{
#' get.refInfo(2266196,"Title")
#' }
get.RefInfo<-function(HoldingID,field){
  url<-paste0("https://irmaservices.nps.gov/datastore/v4/rest/Profile/",HoldingID)
  DSReference<-jsonlite::read_json(url,simplifyVector = TRUE)

  if (field=="ReferenceType") {
    DSValue<-DSReference$referenceType
  }
  else if (field=="Title") {
    DSValue<-DSReference$bibliography$title
  }
  else if (field=="Abstract") {
    DSValue<-DSReference$bibliography$abstract
  }
  else if (field=="Citation") {
    DSValue<-DSReference$citation
  }
  else if (field=="Keywords") {
    DSValue<-DSReference$keywords
  }
  else if (field=="Visibility") {
    DSValue<-DSReference$visibility
  }
  else if (field=="Lifecycle") {
    DSValue<-DSReference$lifecycle
  }
  else if (field=="IssuedYear") {
    DSValue<-DSReference$bibliography$issued$year
  }
  else if (field=="Units") {
    DSValue<-DSReference$units$unitCode
  }
  else if (field=="Versions") {
    Version.Number<-DSReference$versions$versionNumber
    Version.Reference<-DSReference$versions$referenceId
    Version.Date<-DSReference$versions$dateOfIssue
    DSValue <- data.frame(Version.Number,Version.Reference,Version.Date)
  }
  else if (field=="CreatedBy") {
    DSValue<-DSReference$history$createdBy
  }
  else if (field=="ContentBeginYear") {
    DSValue<-DSReference$bibliography$contentBegin$year
  }
  else if (field=="ContentEndYear") {
    DSValue<-DSReference$bibliography$contentEnd$year
  }
  else {
    message(paste0("The requested parameter (",field,") could not be found."))
  }
  
  return(DSValue)
}