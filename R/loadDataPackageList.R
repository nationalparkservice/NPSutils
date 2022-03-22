#' Read contents of data package file and construct a vector with the list of data frames to be created.
#' For file geodatabases this will only extract spatial data features.
#'
#' \code{loadDataPackageList} reads the data file from a package and loads it into a data frame.
#'
#' @param HoldingID is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#' @param dataFormat is a character value indicating the format of the data set(s) within the data package. Currently
#' allowable options are:
#' * "csv" for comma separated value text files
#' * "gdb" for file geodatabases
#' @param metadataFormat is a character value indicating the format of the metadata file within the data package.
#' Currently allowable options are:
#' * "eml" for eml-compliant xml metadata files
#' * "fgdc" for FGDC-compliant xml metadata files
#'
#' @return one or more data frames contained within the data package to the global environment.
#'
#' @examples
#'
#' DataPackageFiles<-loadDataPackageList(2272461,dataFormat="csv",metadataFormat="eml")


loadDataPackageList <-function(HoldingID,dataFormat,metadataFormat){
  DataPackageDirectory<-paste("data/",HoldingID,sep="")
  DataPackageFilename<-paste(DataPackageDirectory,"/",HoldingID,".zip",sep="")

  if (dataFormat=="csv" & metadataFormat=="eml") {
    fileList<-unzip(DataPackageFilename,list=TRUE)
    return(fileList)
    
  } else if (dataFormat=="gdb" & metadataFormat=="fgdc") {
    # And now working with the file geodatabase
    gdbfile <- list.dirs(path = DataPackageDirectory, full.names = TRUE, recursive = FALSE)
    fGDB <- paste0(getwd(),"/",gdbfile)
    datasets<-sf::st_layers(fGDB,do_count=TRUE)
    datasets<-datasets$name

    datasetList<-data.frame(dataset=character(),stringsAsFactors = FALSE)

    for (i in 1:length(datasets)){
      temp<-sf::st_read(dsn=fGDB,layer=datasets[i],stringsAsFactors = FALSE)

      if (class(temp[1])=="sf"){
        temp2<-data.frame(dataset=datasets[i],stringsAsFactors = FALSE)
        datasetList<-rbind.data.frame(datasetList,temp2,stringsAsFactors = FALSE)
        }
    }
    return(datasetList)

  } else {
    print ("data/metadata format combination not supported")
  }
}






