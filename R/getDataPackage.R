#' Retrieve public digital data package holding from Data Store.
#'
#' @description getDataPackage downloads a data package from data store, copies it to the /data directory, and unzips it into a sub-directory of /data with the "HoldingID" name.
#'
#' @param ReferenceID is a 6-7 digit number corresponding to the reference ID of the data package.
#' @param Secure logical TRUE (default) or FALSE indicating whether the file should be acquired using data services available to NPS internal staff only.
#'
#' @examples
#' \dontrun{
#' get.dataPackage(2272461,Secure=TRUE)
#' }
get.DataPackage<-function(ReferenceID,Secure=FALSE){

  # Create directory to hold the data package if it does not already exist.
  if (!file.exists("data")) {
    dir.create("data")
  }

  DestinationDirectory<-paste("data/",ReferenceID,sep="")
  if (!file.exists(DestinationDirectory)) {
    dir.create(DestinationDirectory)
  }

  if (toupper(Secure)=="TRUE") {
    # get HoldingID from the ReferenceID - defaults to the first holding
    RestHoldingInfoURL <- paste0('https://irmaservices.nps.gov/datastore-secure/v4/rest/reference/',ReferenceID,'/DigitalFiles')
    xml <- httr::content(httr::GET(RestHoldingInfoURL, httr::authenticate(":", ":", "ntlm")))
    RestDownloadURL <- paste0("https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/",xml[[1]]$resourceId)
    #HoldingID <- xml[[1]]$resourceId
    DigitalFileType <- xml[[1]]$extension
    #if (HoldingID > 0) {
    #  RestDownladURL<-paste0('https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/',HoldingID)
    #} else {
    #  stop("An error occurred. A HoldingID could not be found for this reference.")
    #}
  
}   else if (toupper(Secure)=="FALSE"){
    # get the HoldingID from the ReferenceID - defaults to the first holding
    RestHoldingInfoURL <- paste0('https://irmaservices.nps.gov/datastore/v4/rest/reference/',ReferenceID,'/DigitalFiles')
    xml <- httr::content(httr::GET(RestHoldingInfoURL))
    RestDownloadURL<-xml[[1]]$downloadLink
    #HoldingID <- xml[[1]]$resourceId
    DigitalFileType <- xml[[1]]$extension
    #if (HoldingID > 0) {
    #  RestDownloadURL<-paste0('https://irmaservices.nps.gov/datastore/v4/rest/DownloadFile/',HoldingID)
    #else {
    #  stop("An error occurred. A HoldingID could not be found for this reference.")
    #}
  }
  
  # download the data package from Data Store into its own directory
  DestinationFilename<-paste("data/",ReferenceID, "/",ReferenceID,".", DigitalFileType ,sep="")
  #DestinationFilename<-paste(DestinationDirectory,"/",ReferenceID,".", DigitalFileType ,sep="")
  #download.file(RestDownloadURL,DestinationFilename,quiet=FALSE, mode="wb")
  httr::content(httr::GET(RestDownloadURL, httr::write_disk(DestinationFilename, overwrite=TRUE), httr::authenticate(":", ":", "ntlm")))
  
  # unzip data package
  # check to see if the downloaded file is a zip
  if(tools::file_ext(DestinationFilename) == "zip"){
    unzip(DestinationFilename, exdir = DestinationDirectory)
  }
  else {
    rlang::inform("File was not a zip file and could not be unzipped")
  }
    
  # check to see that the zip was downloaded and unzipped
  # this might be worth improving in the future, but looks for >1 files in the folder
  if (length(list.files(DestinationDirectory, include.dirs = FALSE)) > 1) {
    rlang::inform(paste0("Download and unzipping of reference ",ReferenceID," succeeded"))
    } else { 
    rlang::inform(paste0("Download and unzipping of reference ",ReferenceID," failed"))
  }
}
