#' Retrieve public digital data package holding from Data Store.
#'
#' \code{getDataPackage} downloads a data package from data store, copies it to the /data/raw directory, and unzips it into a subdirectory with the "HoldingID" name.
#'
#'
#' @param HoldingID is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#' @param Secure True or False (default) indicating whether the file should be acquired using data services available to NPS internal staff only.
#'
#' @examples
#' getDataPackage(2272461,Secure=TRUE)

getDataPackage<-function(HoldingID,Secure=FALSE){

  # Create directory to hold the data package if it does not already exist.
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  if (!file.exists("data/raw")) {
    dir.create("data/raw")
  }

  DestinationDirectory<-paste("data/raw/",HoldingID,sep="")
  if (!file.exists(DestinationDirectory)) {
    dir.create(DestinationDirectory)
  }

  if (Secure=="TRUE") {
    # get fileID from the reference number
    RestHoldingInfoURL<-paste0('https://irmaservices.nps.gov/datastore/v4/rest/reference/',HoldingID,'/DigitalFiles')
    xml<-httr::content(httr::GET(RestHoldingInfoURL))
    DigitalFileID<-xml[[1]]$resourceId
    RestDownladURL<-paste0('https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/',DigitalFileID)

  } else if (Secure=="FALSE") {
    # get fileID from the reference number
    RestHoldingInfoURL<-paste0('https://irmaservices.nps.gov/datastore/v4/rest/reference/',HoldingID,'/DigitalFiles')
    xml<-httr::content(httr::GET(RestHoldingInfoURL))
    DigitalFileID<-xml[[1]]$resourceId
    RestDownladURL<-paste0('https://irmaservices.nps.gov/datastore/v4/rest/DownloadFile/',DigitalFileID)

  }

  # download the data package from Data Store into its own directory
  DestinationFilename<-paste(DestinationDirectory,"/",HoldingID,".zip",sep="")
  download.file(RestDownladURL,DestinationFilename,quiet=TRUE, mode="wb")

  # unzip data package
  unzip(DestinationFilename, exdir = DestinationDirectory)

}
