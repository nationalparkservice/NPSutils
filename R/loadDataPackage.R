#' Read contents of data packge file and construct a data frame based on the data file(s) and associated metadata.
#'
#' \code{loadDataPackage} reads the data file from a package and loads it into one or more data frames.
#'
#' @param HoldingID is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#' @param dataFormat is a character value indicating the format of the data set(s) within the data package. Currently
#' allowable options are:
#' * "csv" for comma seperated value text files
#' * "gdb" for file geodatabases
#' @param metadataFormat is a character value indicating the format of the metadata file within the data package.
#' Currently allowable options are:
#' * "eml" for eml-compliant xml metadata files
#' * "fgdc" for FGDC-compliant xml metadata files
#' @param features is a character value indicating the name of the feature class / data set (only used with file geodatabases)
#'
#' @return one or more data frames contained within the data package to the global enviroment.
#'
#' @examples
#'
#' ParkTandETaxa<-loadDataPackage(2272461,dataFormat="csv",metadataFormat="eml")


loadDataPackage <-function(HoldingID,dataFormat,metadataFormat,features=NULL){
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

    # Figure out column clases based on attribute table (character, numeric, integer, logical, or complex)
    attributes$columnclass<-"character"
    if("attributes$numberType" %in% colnames(attributes)){
      attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="natural","integer",attributes$columnclass)
      attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="whole","integer",attributes$columnclass)
      attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="integer","integer",attributes$columnclass)
      attributes$columnclass<-ifelse(attributes$storageType=="float" & attributes$numberType=="real","numeric",attributes$columnclass)
    }
    if("attributes$formatString" %in% colnames(attributes)) {
      attributes$columnclass<-ifelse(attributes$storageType=="date" & attributes$formatString=="YYYY-MM-DD" ,"Date",attributes$columnclass)
    }
    workingdatafile<-read.csv(csvFilename, col.names= attributes$attributeName,colClasses=attributes$columnclass)

    # Assign levels for fields with enumerated domains and convert those columns to factors
    attributes2<-subset(attributes, select=c(attributeName,measurementScale,domain))

    for (i in 1:nrow(attributes2)) {
      columnName<-attributes2[i,1]
      factorsubset<-subset(factors,factors$attributeName==columnName)
      if(attributes2[i,2]=="nominal" & attributes2[i,3]=="enumeratedDomain") {
        workingdatafile[,i]<-factor(workingdatafile[,i],levels=factorsubset$code)
      }
      if(attributes2[i,2]=="ordinal" & attributes[i,3]=="enumeratedDomain") {
        workingdatafile[,i]<-factor(workingdatafile[,i],levels=factorsubset$code)
        levels(workingdatafile[,i]) <- factorsubset$code
      }
    }

    # return the metadata and data to the workspace as a list and data frame respectively

    assign(paste0(HoldingID,"_data"),workingdatafile, envir=.GlobalEnv)
    # assign(paste0(HoldingID,"_emlMetadata"), EML::read_eml(emlFilename, from = "xml"), envir=.GlobalEnv)

    return(workingdatafile)
    
  } else if (dataFormat=="gdb" & metadataFormat=="fgdc") {

    # Working with the metadata file first...

    xmlfile <-list.files(path=DataPackageDirectory,pattern = ".xml")
    xmlFilename <- paste0(DataPackageDirectory,"/",xmlfile)
    workingXMLfile<-EML::read_eml(xmlFilename, from = "xml")

    # return the metadata to the workspace as a list.
    # assign(paste0(HoldingID,"_fgdcMetadata"), workingXMLfile, envir=.GlobalEnv)

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

    # And now working with the file geodatabase

    gdbfile <- list.dirs(path = DataPackageDirectory, full.names = TRUE, recursive = FALSE)
    fGDB <- paste0(getwd(),"/",gdbfile)

    if(is.null(features)){
      datasets<-loadDataPackageList(HoldingID,dataFormat,metadataFormat)
      datasets<-datasets$dataset
    } else {
      datasets<-features
    }

    datasetList<-data.frame(dataset=character(),stringsAsFactors = FALSE)

    for (i in 1:length(datasets)){
      temp<-sf::st_read(dsn=fGDB,layer=datasets[i],stringsAsFactors = FALSE)

      if (class(temp[1])=="sf"){
        temp2<-sf::st_set_geometry(temp,NULL) # non-shape fields
        temp3<-temp$Shape # spatial fields

        columnnames<-as.data.frame(colnames(temp2))
        colnames(columnnames)[1]<-"attribute"
        columnclasses<-dplyr::inner_join(columnnames,attributes)

        for (j in 1:nrow(columnclasses)){
          if (columnclasses[j,6]=="Date"){temp2[,j]<-as.Date(temp2[,j])}
          if (columnclasses[j,6]=="character"){temp2[,j]<-as.character(temp2[,j])}
          if (columnclasses[j,6]=="numeric"){temp2[,j]<-as.numeric(temp2[,j])}
          if (columnclasses[j,6]=="character" & columnclasses[j,5]> 1){
            factorLevels<-subset(attributeLevels,attribute==columnclasses[j,1])
            temp2[,j]<-factor(temp2[,j],factorLevels$factor,ordered=FALSE)
          }
        }

        temp4<-sf::st_sf(cbind(temp2,temp3))

        # return data to the workspace as a spatial data frame.
        datasetList<-rbind(datasetList,paste0(datasets[i]),stringsAsFactors=FALSE)
        colnames(datasetList)[1]<-"dataset"
        assign(paste0(datasets[i]), temp4, envir=.GlobalEnv)
        }
    }

    datasetList<-datasetList$dataset
    return(datasetList)

  } else {
    print ("data/metadata format combination not supported")
  }
}






