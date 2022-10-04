#' Read contents of data package file and construct a data frame based on the metadata file summarizing the enumerated domains for
#' categorical fields and their types/definitions.
#'
#' @description \code{load.domains} reads the data file from a package and loads a list of fields and their attributes into a data frame.
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
#' @return one data frame to the global environment.
#'
#' @examples
#' \dontrun{
#' load.domains(2266200,dataFormat="gdb",metadataFormat="fgdc")
#' }
load.domains <-function(HoldingID,dataFormat,metadataFormat){
  DataPackageDirectory<-paste("data/raw/",HoldingID,sep="")
  DataPackageFilename<-paste(DataPackageDirectory,"/",HoldingID,".zip",sep="")
  
  if (dataFormat=="csv" & metadataFormat=="eml") {
    
    fileList<-unzip(DataPackageFilename,list=TRUE)
    
    csvfile <- subset(fileList, grepl(".csv",Name))
    emlfile <- subset(fileList, grepl(".xml",Name))
    csvFilename <- paste(DataPackageDirectory,"/",csvfile[1],sep="")
    emlFilename <- paste(DataPackageDirectory,"/",emlfile[1],sep="")
    
    workingEMLfile<-EML::read_eml(emlFilename, from = "xml")
    attributeList<-EML::get_attributes(workingEMLfile$dataset$dataTable$attributeList)
    attributes<-attributeList$attributes
    factors<-attributeList$factors
    factors<-factors[,c(3,1,2)]
    
    return(factors)
    
  } else if (dataFormat=="gdb" & metadataFormat=="fgdc") {
    
    # Working with the metadata file first...
    
    xmlfile <-list.files(path=DataPackageDirectory,pattern = ".xml")
    xmlFilename <- paste0(DataPackageDirectory,"/",xmlfile)
    workingXMLfile<-EML::read_eml(xmlFilename, from = "xml")
    
    # return the metadata to the workspace as a list.
    assign(paste0(HoldingID,"_fgdcMetadata"), workingXMLfile, envir=.GlobalEnv)
    
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
    attributes$attributeType<-as.character(attributes$attributeType)
    attributes$attributeFactors<-as.integer(as.character(attributes$attributeFactors))
    
    attributes$columnclass<-"character"
    attributes$columnclass<-ifelse(attributes$attributeType=="OID","integer",attributes$columnclass)
    attributes$columnclass<-ifelse(attributes$attributeType=="Date","Date",attributes$columnclass)
    attributes$columnclass<-ifelse(attributes$attributeType=="Double","numeric",attributes$columnclass)
    
    # Get the factor definitions for class variables
    attributeLevels<-data.frame(attribute=character(),factor=character(),factorDefinition=character(),stringsAsFactors=FALSE)
    attributesWithFactors<-subset(attributes,attributeFactors >1)
    for (i in 1:nrow(attributesWithFactors)){
      for (j in 1:attributesWithFactors[i,5]){
        attributeLevels<-rbind(attributeLevels,cbind(
          attribute=attributesWithFactors[i,2],
          factor=workingXMLfile$ea$detailed$attr[[attributesWithFactors[i,1]]]$attrdomv[[j]]$edom$edomv,
          factorDefinition=workingXMLfile$ea$detailed$attr[[attributesWithFactors[i,1]]]$attrdomv[[j]]$edom$edomvd))
      }
    }
    
    attributeLevels$attribute<-as.character(attributeLevels$attribute)
    attributeLevels$factor<-as.character(attributeLevels$factor)
    attributeLevels$factorDefinition<-as.character(attributeLevels$factorDefinition)
    
    # return the enumerated domain table to the workspace as a list and data frame respectively
    return(attributeLevels)
    
  } else {
    print ("data/metadata format combination not supported")
  }
}