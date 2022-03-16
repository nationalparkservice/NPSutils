#' Read contents of data package file and construct a data frame based on the metadata file summarizing the fields and their types/definitions.
#'
#' \code{loadMetadata} reads the metadata file from a previously downloaded package and loads a list of fields and their attributes into a data frame.
#'
#' @param HoldingID is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#' @param metadataFormat is a character value indicating the format of the metadata file within the data package.
#' Currently allowable options are:
#' * "eml" for eml-compliant xml metadata files
#' * "fgdc" for FGDC-compliant xml metadata files
#'
#' @return one data frame to the global environment.
#'
#' @examples
#'
#' attributeTable<-loadMetadata(2266200,metadataFormat="fgdc")


loadMetadata <-function(HoldingID,dataFormat,metadataFormat){
  DataPackageDirectory<-paste("data/raw/",HoldingID,sep="")
  DataPackageFilename<-paste(DataPackageDirectory,"/",HoldingID,".zip",sep="")

  if (metadataFormat=="eml") {

    fileList<-unzip(DataPackageFilename,list=TRUE)

    csvfile <- subset(fileList, grepl(".csv",Name))
    emlfile <- subset(fileList, grepl(".xml",Name))
    csvFilename <- paste(DataPackageDirectory,"/",csvfile[1],sep="")
    emlFilename <- paste(DataPackageDirectory,"/",emlfile[1],sep="")

    workingEMLfile<-EML::read_eml(emlFilename, from = "xml")
    attributeList<-EML::get_attributes(workingEMLfile$dataset$dataTable$attributeList)
    attributes<-attributeList$attributes
    factors<-attributeList$factors

    # Figure out column clases based on attribute table (character, numeric, integer, logical, or complex)
    attributes$columnclass<-"character"
    if(!"numberType" %in% colnames(attributes)){
      attributes$numberType<-as.character(NA)
    }
    if(!"formatString" %in% colnames(attributes)){
      attributes$formatString<-as.character(NA)
    }
    attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="natural","integer",attributes$columnclass)
    attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="whole","integer",attributes$columnclass)
    attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="integer","integer",attributes$columnclass)
    attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="real","numeric",attributes$columnclass)
    attributes$columnclass<-ifelse(attributes$storageType=="date" & attributes$formatString=="YYYY-MM-DD" ,"Date",attributes$columnclass)

    # return the field table to the workspace.
    return(attributes)

  } else if (metadataFormat=="fgdc") {

    # Working with the metadata file first...
    xmlfile <-list.files(path=DataPackageDirectory,pattern = ".xml")
    # if more than one XML is found then attempt to use the one that includes "_metadata" in the name
    if (length(xmlfile) > 1) {
      message("More than 1 XML files found in this data package.")
      temp <- grep("_metadata", xmlfile)
      if (length(temp) >= 1) {
        xmlfile <- xmlfile[temp[1]]
      } else {
        #if none of them include "_metadata" in the name then just work with the first one
        xmlfile <- xmlfile[1]
      }
    }
    message(paste0("Processing ",xmlfile))
    xmlFilename <- paste0(DataPackageDirectory,"/",xmlfile)
    workingXMLfile<-EML::read_eml(xmlFilename, from = "xml")
    
    # Build attributes table from the xml file
    attributes<-data.frame(id=numeric(),attribute=character(),attributeDefinition=character(),attributeType=character(),attributeFactors=numeric(),stringsAsFactors = FALSE)
    for (i in 1:length(workingXMLfile$ea$detailed$attr)){
      attributes<-rbind(attributes,cbind(id=i,
                                                 attribute=workingXMLfile$ea$detailed$attr[[i]]$attrlabl,
                                                 attributeDefinition=workingXMLfile$ea$detailed$attr[[i]]$attrdef,
                                                 attributeType=workingXMLfile$ea$detailed$attr[[i]]$attrtype,
                                                 attributeFactors=length(workingXMLfile$ea$detailed$attr[[i]]$attrdomv)))
    }

    attributes$id<-as.integer(as.character(attributes$id))
    attributes$attribute<-as.character(attributes$attribute)
    attributes$attributeDefinition<-as.character(attributes$attributeDefinition)
    #attributes$attributeType<-as.character(attributes$attributeType)
    attributes$attributeFactors<-as.integer(as.character(attributes$attributeFactors))

    attributes$columnclass<-"character"
    #attributes$columnclass<-ifelse(attributes$attributeType=="OID","integer",attributes$columnclass)
    #attributes$columnclass<-ifelse(attributes$attributeType=="Date","Date",attributes$columnclass)
    #attributes$columnclass<-ifelse(attributes$attributeType=="Double","numeric",attributes$columnclass)

    message(paste0("Found ",nrow(attributes)," fields"))
    
    # return the field table to the workspace.
    return(attributes)

  } else {
    print ("data/metadata format combination not supported")
  }
}