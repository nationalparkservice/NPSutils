#' Read contents of data package file and construct a data frame based on the
#' metadata file summarizing the fields and their types/definitions.
#'
#' @description `load_pkg_metadata()` reads the metadata file from a previously
#' downloaded package and loads a list of fields and their attributes into a
#' dataframe.
#'
#' @param holding_id is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#' @param directory String. Path to the data package
#'
#' @return one data frame to the global environment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_pgk_metadata(2266200)
#' }
load_pkg_metadata <- function(holding_id, directory = here::here("data")) {
  data_package_directory <- paste(directory, "/", holding_id, sep = "")

  metadata_file <- list.files(
    path = data_package_directory,
    pattern = "metadata.xml"
  )

  # Look for a metadatafile and let the user know about the results of the search.
  if (length(metadata_file) == 0) {
    cli::cli_abort(c(
      "No metadata file found in: {.path {data_package_directory}}.",
      "i" = "The filename must end in _metadata.xml"))
    return(invisible())
  }
  if (length(metadata_file) > 1) {
    cli::cli_abort(c(
      "Multiple metadata files found.",
      "i" = "{.path {data_package_directory}} can contain only one 
      {.file *_metadata.xml}."))
    return(invisible())
  }
  
  meta_location <- paste0(data_package_directory, "/", metadata_file)
  if (!file.exists(meta_location)) {
    cli::cli_abort(c(
      "The data package for: {.var {holding_id}} was not found.",
      "i" = "Make sure {.path {data_package_directory}} is the correct location",
      "i" = "Make sure you downloaded the correct data package using {.fn get_data_package}."
    ))
    return(invisible())
  }

  #load metadata
  eml_object <- EML::read_eml(meta_location, from = "xml")
    attributeList <- EML::get_attributes(workingEMLfile$dataset$dataTable$attributeList)
    attributes <- attributeList$attributes
    factors <- attributeList$factors

    # Figure out column classes based on attribute table (character, numeric, integer, logical, or complex)
    attributes$columnclass <- "character"
    if (!"numberType" %in% colnames(attributes)) {
      attributes$numberType <- as.character(NA)
    }
    if (!"formatString" %in% colnames(attributes)) {
      attributes$formatString <- as.character(NA)
    }
    attributes$columnclass <- ifelse(attributes$storageType == "float" & attributes$numberType == "natural", "integer", attributes$columnclass)
    attributes$columnclass <- ifelse(attributes$storageType == "float" & attributes$numberType == "whole", "integer", attributes$columnclass)
    attributes$columnclass <- ifelse(attributes$storageType == "float" & attributes$numberType == "integer", "integer", attributes$columnclass)
    attributes$columnclass <- ifelse(attributes$storageType == "float" & attributes$numberType == "real", "numeric", attributes$columnclass)
    attributes$columnclass <- ifelse(attributes$storageType == "date" & attributes$formatString == "YYYY-MM-DD", "Date", attributes$columnclass)

    # return the field table to the workspace.
    return(attributes)

if (metaformat == "fgdc") {
    # xmlFilename <- metalocation
    workingXMLfile <- EML::read_eml(metalocation, from = "xml")

    # Build attributes table from the xml file
    attributes <- data.frame(
      id = numeric(),
      attribute = character(),
      attributeDefinition = character(),
      attributeType = character(),
      attributeFactors = numeric(),
      stringsAsFactors = FALSE
    )
    for (i in 1:length(workingXMLfile$ea$detailed$attr)) {
      attributes <- rbind(
        attributes,
        cbind(
          id = i,
          attribute = workingXMLfile$ea$detailed$attr[[i]]$attrlabl,
          attributeDefinition = workingXMLfile$ea$detailed$attr[[i]]$attrdef,
          attributeType = workingXMLfile$ea$detailed$attr[[i]]$attrtype,
          attributeFactors = length(workingXMLfile$ea$detailed$attr[[i]]$attrdomv)
        )
      )
    }

    attributes$id <- as.integer(as.character(attributes$id))
    attributes$attribute <- as.character(attributes$attribute)
    attributes$attributeDefinition <- as.character(attributes$attributeDefinition)
    # attributes$attributeType<-as.character(attributes$attributeType)
    attributes$attributeFactors <- as.integer(as.character(attributes$attributeFactors))

    attributes$columnclass <- "character"
    # attributes$columnclass<-ifelse(attributes$attributeType=="OID","integer",attributes$columnclass)
    # attributes$columnclass<-ifelse(attributes$attributeType=="Date","Date",attributes$columnclass)
    # attributes$columnclass<-ifelse(attributes$attributeType=="Double","numeric",attributes$columnclass)

    cat("Found ", crayon::blue$bold(nrow(attributes)), " fields.", sep = "")

    # return the field table to the workspace.
    return(attributes)
  } else {
    print("data/metadata format combination not supported")
  }
}
