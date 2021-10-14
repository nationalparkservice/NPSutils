#' Get DS References for a park-species combination
#'
#' \code{DSgetParkTaxonReferences} returns a data frame of metadata for references in Data Store corresponding to a particular taxon at a park.
#' The resultant data frame is then usable by other functions to extract metadata from the records.
#'
#' Note that this function only returns references that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @param ParkCode The four-letter unit code for the park of interest.
#' @param TaxonCode Taxonomic Serial Number for the taxon of interest.
#'
#' @examples
#' DSgetParkTaxonReferences("APIS", 126749)
DSgetParkTaxonReferences<-function(ParkCode,TaxonCode){
  url<-paste("https://irmaservices.nps.gov/datastore-secure/v4/rest/UnitSpeciesSearch/",ParkCode,"/",TaxonCode,sep="")
  DSReference<-read_json(url,simplifyVector = TRUE)
  return(DSReference)
}

#' Get DS Citations for a park-species combination
#'
#' \code{DSgetParkTaxonCitations} returns a vector of citations in Data Store corresponding to a particular taxon at a park.
#'
#' Note that this function only returns citations that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @param ParkCode The four-letter unit code for the park of interest.
#' @param TaxonCode Taxonomic Serial Number for the taxon of interest.
#'
#' @examples
#' DSgetParkTaxonCitations("APIS", 126749)
DSgetParkTaxonCitations<-function(ParkCode,TaxonCode){
  References<-getParkTaxonReferences(ParkCode,TaxonCode)
  DSCitations<-DSReference$citation
  return(DSCitations)
}

#' Get URL for references for a park-species combination
#'
#' \code{DSgetParkTaxonReferenceURL} returns a vector of URLs for references in Data Store corresponding to a particular taxon at a park.
#'
#' Note that this function only returns URLs for references that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @param ParkCode The four-letter unit code for the park of interest.
#' @param TaxonCode Taxonomic Serial Number for the taxon of interest.
#'
#' @examples
#' DSgetParkTaxonURLs("APIS", 126749)
DSgetParkTaxonURLs<-function(ParkCode,TaxonCode){
  References<-getParkTaxonReferences(ParkCode,TaxonCode)
  DSReferenceCodes<-DSReference$referenceId
  DSURLs<-paste("https://irma.nps.gov/DataStore/Reference/Profile/",DSReferenceCodes,sep="")
  return(DSURLs)
}

#' Get citation for Data Store holding info by HoldingID
#'
#' \code{DSReferenceInfo(HoldingID,field)} returns a character string or a vector with information from one of the
#' metadata fields in a Data Store reference's associated xml file.
#'
#'
#' @param HoldingID The six-seven digit reference / holding ID number unique to the data store record.
#' @param field is one of the following: "Title" returns the title of the data store reference as a string value;
#' "Abstract" returns the abstract as a string value; "Citation" returns the citation as a string value, and "Keywords" returns a vector containing
#' all keywords as character values.
#'
#' @examples
#' DSRefernceInfo(2266196,"Title")
DSReferenceInfo<-function(HoldingID,field){
  url<-paste0("https://irmaservices.nps.gov/datastore/v4/rest/Profile/",HoldingID)
  DSReference<-jsonlite::read_json(url,simplifyVector = TRUE)

  if (field=="Title") {
    DSValue<-DSReference$bibliography$title
  }

  if (field=="Abstract") {
    DSValue<-DSReference$bibliography$abstract
  }

  if (field=="Citation") {
    DSValue<-DSReference$citation
  }

  if (field=="Keywords") {
    DSValue<-DSReference$keywords
  }

  return(DSValue)
}