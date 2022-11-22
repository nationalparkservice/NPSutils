#' Read contents of data package file and construct a vector with the list of data frames to be created.
#'
#' @description \code{load_data_package_list} reads the data file from a package and loads it into a data frame.
#'
#' @param holding_id is a 6-7 digit number corresponding to the holding ID of the data package zip file.
#' @param data_format is a character value indicating the format of the data set(s) within the data package. Currently
#' allowable options are:
#' * "csv" for comma separated value text files
#' * "gdb" for file geodatabases
#' @param metadata_format is a character value indicating the format of the metadata file within the data package.
#' Currently allowable options are:
#' * "eml" for eml-compliant xml metadata files
#' * "fgdc" for FGDC-compliant xml metadata files
#'
#' @export
#'
#' @return one or more data frames contained within the data package to the global environment.
#'
#' @examples
#' \dontrun{
#' load_data_package_list(2272461, data_format = "csv", metadata_format = "eml")
#' }
#'
load_data_package_list <- function(holding_id, data_format, metadata_format) {
  DataPackageDirectory <- paste("data/", holding_id, sep = "")
  DataPackageFilename <- paste(DataPackageDirectory, "/", holding_id, ".zip", sep = "")

  if (data_format == "csv" & metadata_format == "eml") {
    fileList <- utils::unzip(DataPackageFilename, list = TRUE)
    return(fileList)
  } else if (data_format == "gdb" & metadata_format == "fgdc") {
    # And now working with the file geodatabase
    gdbfile <- list.dirs(
      path = DataPackageDirectory,
      full.names = TRUE,
      recursive = FALSE
    )
    fGDB <- paste0(getwd(), "/", gdbfile)
    datasets <- sf::st_layers(fGDB, do_count = TRUE)
    datasets <- datasets$name

    datasetList <- data.frame(dataset = character(), stringsAsFactors = FALSE)

    for (i in 1:length(datasets)) {
      temp <- sf::st_read(dsn = fGDB, layer = datasets[i], stringsAsFactors = FALSE)

      if (class(temp[1]) == "sf") {
        temp2 <- data.frame(dataset = datasets[i], stringsAsFactors = FALSE)
        datasetList <- rbind.data.frame(datasetList, temp2, stringsAsFactors = FALSE)
      }
    }
    return(datasetList)
  } else {
    print("data/metadata format combination not supported")
  }
}
