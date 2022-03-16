#' Retrieve public digital data package holding from Data Store.
#'
#' \code{getDataPackage} downloads a data package from data store, copies it to the /data/raw directory, and unzips it into a subdirectory with the "HoldingID" name.
#'
#'
#' @param ReferenceID is a 6-7 digit number corresponding to the reference ID of the data package.
#' @param Secure True or False (default) indicating whether the file should be acquired using data services available to NPS internal staff only.
#'
#' @examples
#' getDataPackage(2272461,Secure=TRUE)

getDataPackage<-function(ReferenceID,Secure=FALSE){

  # Create directory to hold the data package if it does not already exist.
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  if (!file.exists("data/raw")) {
    dir.create("data/raw")
  }

  DestinationDirectory<-paste("data/raw/",ReferenceID,sep="")
  if (!file.exists(DestinationDirectory)) {
    dir.create(DestinationDirectory)
  }

  if (Secure=="TRUE") {
    # get HoldingID from the ReferenceID - defaults to the first holding
    RestHoldingInfoURL<-paste0('https://irmaservices.nps.gov/datastore-secure/v4/rest/reference/',ReferenceID,'/DigitalFiles')
    xml<-httr::GET(RestHoldingInfoURL)
    # check to see the response type; 200 is good; 401 is bad (are there others to consider?)
    if (xml$status_code == 200) {
      xml<-httr::content(xml)
    } else {
      stop("An error occurred. Are you connected to the NPS network/VPN?")
    }
    HoldingID<-xml[[1]]$resourceId
    if (DigitalFileID > 0) {
      RestDownladURL<-paste0('https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/',HoldingID)
    } else {
      stop("An error occurred. A resource ID could not be found for this reference.")
    }
    
  } else if (Secure=="FALSE") {
    # get the HoldingID from the ReferenceID - defaults to the first holding
    RestHoldingInfoURL<-paste0('https://irmaservices.nps.gov/datastore/v4/rest/reference/',ReferenceID,'/DigitalFiles')
    xml<-httr::content(httr::GET(RestHoldingInfoURL))
    HoldingID<-xml[[1]]$resourceId
    DigitalFileType<-xml[[1]]$extension
    RestDownladURL<-paste0('https://irmaservices.nps.gov/datastore/v4/rest/DownloadFile/',HoldingID)
  }

  # download the data package from Data Store into its own directory
  DestinationFilename<-paste(DestinationDirectory,"/",ReferenceID,".", DigitalFileType ,sep="")
  download.file(RestDownladURL,DestinationFilename,quiet=FALSE, mode="wb")

  # unzip data package
  # check to see if the downloaded file is a zip
  if (tools::file_ext(DestinationFilename) == "zip") {
    unzip(DestinationFilename, exdir = DestinationDirectory)
  }
  else {
    rlang::inform("File was not a zip file and could not be unzipped")
  }
  
  # check to see that the zip was downloaded and unzipped
  # this might be worth improving in the future, but looks for >2 files in the folder
  if (length(list.files(DestinationDirectory, include.dirs = FALSE)) > 2) {
    rlang::inform(paste0("Download and unzipping of reference ",ReferenceID," succeeded"))
  } else {
    rlang::inform(paste0("Download and unzipping of reference ",ReferenceID," failed"))
  }
}