#' Get DS References for a park-species combination
#'
#' @description \code{get_park_taxon_refs} returns a data frame of metadata for references in Data Store corresponding to a particular taxon at a park.
#' The resultant data frame is then usable by other functions to extract metadata from the records.
#'
#' Note that this function only returns references that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @param park_code The four-letter unit code for the park of interest.
#' @param taxon_code Taxonomic Serial Number for the taxon of interest.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_park_taxon_refs("APIS", 126749)
#' }
get_park_taxon_refs <- function(park_code, taxon_code) {
  url <- paste(
    "https://irmaservices.nps.gov/datastore-secure/v4/rest/UnitSpeciesSearch/",
    park_code, "/", taxon_code,
    sep = ""
  )
  DSReference <- httr::content(httr::GET(
    url, httr::authenticate(":", ":", "ntlm")
  )) %>%
    dplyr::bind_rows()
  return(DSReference)
}

#' Get DataStore Citations for a park-species combination
#'
#' @description \code{get_park_taxon_citations} returns a vector of citations in Data Store corresponding to a particular taxon at a park.
#'
#' Note that this function only returns citations that have been "tagged" with a taxon code and may represent only a subset of references that have information about a taxon.
#'
#' @inheritParams get_park_taxon_refs
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_park_taxon_citations("APIS", 126749)
#' }
get_park_taxon_citations <- function(park_code, taxon_code) {
  references <- get_park_taxon_refs(park_code, taxon_code)
  citations <- references$citation
  return(citations)
}

#' Get URL for references for a park-species combination
#'
#' \code{get_park_taxon_url} returns a vector of URLs for references in Data Store corresponding to a particular taxon at a park.
#'
#' Note that this function only returns URLs for references that have been "tagged" with a taxon code and may represent only a
#' subset of references that have information about a taxon.
#'
#' @inheritParams get_park_taxon_refs
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_park_taxon_url("APIS", 126749)
#' }
get_park_taxon_url <- function(park_code, taxon_code) {
  references <- get_park_taxon_refs(park_code, taxon_code)
  reference_codes <- references$referenceId
  URLs <- paste("https://irma.nps.gov/DataStore/Reference/Profile/",
    reference_codes,
    sep = ""
  )
  return(URLs)
}

#' Get citation for Data Store holding info by HoldingID
#'
#' @description \code{get_ref_info} returns a character string or a vector with information from one of the
#' metadata fields in a Data Store reference's associated xml file.
#'
#'
#' @param holding_id The six-seven digit reference / holding ID number unique to the data store record.
#' @param field is one of the following: "Title" returns the title of the data store reference as a string value;
#' "Abstract" returns the abstract as a string value; "Citation" returns the citation as a string value, and "Keywords" returns a vector containing
#' all keywords as character values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_ref_info(2266196, "Title")
#' }
get_ref_info <- function(holding_id, field) {
  url <- paste0(
    "https://irmaservices.nps.gov/datastore/v4/rest/Profile/",
    holding_id
  )
  DSReference <- httr::content(httr::GET(
    url, httr::authenticate(":", ":", "ntlm")
  ))

  if (field == "ReferenceType") {
    DSValue <- DSReference$referenceType
  } else if (field == "Title") {
    DSValue <- DSReference$bibliography$title
  } else if (field == "Abstract") {
    DSValue <- DSReference$bibliography$abstract
  } else if (field == "Citation") {
    DSValue <- DSReference$citation
  } else if (field == "Keywords") {
    DSValue <- DSReference$keywords %>%
      unlist()
  } else if (field == "Visibility") {
    DSValue <- DSReference$visibility
  } else if (field == "Lifecycle") {
    DSValue <- DSReference$lifecycle
  } else if (field == "IssuedYear") {
    DSValue <- DSReference$bibliography$issued$year
  } else if (field == "Units") {
    DSValue <- DSReference$units$unitCode
  } else if (field == "Versions") {
    Version.Number <- DSReference$versions$versionNumber
    Version.Reference <- DSReference$versions$referenceId
    Version.Date <- DSReference$versions$dateOfIssue
    DSValue <- data.frame(Version.Number, Version.Reference, Version.Date)
  } else if (field == "CreatedBy") {
    DSValue <- DSReference$history$createdBy
  } else if (field == "ContentBeginYear") {
    DSValue <- DSReference$bibliography$contentBegin$year
  } else if (field == "ContentEndYear") {
    DSValue <- DSReference$bibliography$contentEnd$year
  } else {
    message(paste0("The requested parameter (", field, ") could not be found."))
  }

  return(DSValue)
}
