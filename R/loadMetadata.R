#' Read contents of data package file and construct a data frame based on the metadata file summarizing the fields and their types/definitions.
#'
#' \code{loadMetadata} reads the metadata file from a previously downloaded package and loads a list of fields and their attributes into a data frame.
#'
#' @param HoldingID is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#'
#' @return one data frame to the global environment.
#'
#' @examples
#'
#' attributeTable<-loadMetadata(2266200)


loadMetadata <-function(HoldingID){
  DataPackageDirectory<-paste("data/",HoldingID,sep="")
  
  metadatafile <- list.files(path=DataPackageDirectory,
                          pattern="metadata.xml")
  metalocation<-paste0(DataPackageDirectory,"/",metadatafile)
  
#Look for a metadatafile and let the user know about the results of the search.
  if(length(metadatafile)==0){
    writeLines(paste0("No metadata file found in: ~", DataPackageDirectory, "\nThe filename must end in metadata.xml"))
  }
  if(length(metadatafile)>1){
    writeLines('Multiple metadata files found. Please insure there is only one metadata file ending in "metadata.xml"')
    ##### some sort of 'graceful' stop/exit function TBD.
  }
  if(length(metadatafile)==1){
    writeLines(paste0("Metadata file found:\n", metadatafile))
  }
  if(!file.exists(metalocation)){
    writeLines(paste0("The data package zip folder for Holding ", HoldingID, " was not found. Check to make sure you have the correct working directory or try using getDataPackage() to download the data package."))
  }
  
#open the metadatafile and determine what format it is. Tell the user what format was detected, and or if the format is unsupported (or incorrectly formatted)
  if(
  sum(grepl("<metstdv>FGDC-STD-001-1998", readLines(metalocation)))>0){
    metaformat<-"fgdc"
    print(paste0("\nYou are working with ", metaformat, " metadata"))
    }
  if(sum(grepl("<eml:eml", readLines(metalocation)))>0){
    metaformat<-"eml"
    writeLines(paste0("\nYou are working with ", metaformat, " metadata"))
  }
  if(sum(grepl("ISO 19115", readLines(metalocation)))>0){
    metaformat<-"ISO19915"
    print(paste0("\nYou are working with ", metaformat, " metadata"))
  }
  if(!exists("metaformat")){
    print("your metadata file format is not supported. Please make sure you are using correctly formatted eml, ISO 19115, or CSDGM (FGDC) metadata")
  }
  
#Construct attribute tables:
  if (metaformat=="eml") {
    #emlFilename <- metalocation
    workingEMLfile<-EML::read_eml(metalocation, from = "xml")
    attributeList<-EML::get_attributes(workingEMLfile$dataset$dataTable$attributeList)
    attributes<-attributeList$attributes
    factors<-attributeList$factors

    # Figure out column classes based on attribute table (character, numeric, integer, logical, or complex)
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

  } else if (metaformat=="fgdc") {
    #xmlFilename <- metalocation
    workingXMLfile<-EML::read_eml(metalocation, from = "xml")
    
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
