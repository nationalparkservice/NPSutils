#' Read contents of data package(s) and return a tibble with dataframe for each data file. 
#'
#' @description `load_data_packages()` finds all the specified data packages in a /data directory (generated via `getDataPackages()`) and returns a tibble of tibbles where each data package is a tibble and within that each data file is it's own tibble. `load_data_packages()` also utilizes the metadata (only EML is currently supported) to correctly assign attributes to each data column.
#' 
#' @details currently `load_data_packages()` only supports EML metadata and .csv files in a very specific file structure, which is most easily set up using `getDataPackages()`. Archived (.zip) files must be extractecd for `load_data_packages()` to work properly. Again, `getDataPackages()` will accomplish this for you. 
#' '
#' @param dataStoreReference is a list of 6-7 digit numbers corresponding to the DataStore reference ID of the datapackage(s) to load. Alternatively, you can set `dataStoreReference` to "loadAll", which will load all the data packages in your /data folder.
#' @param path is the location of a folder, 'data' (created during `getDataPackages()`) which contains subdirectories where each subdirectory is the DataStore referenceId of the data package. Again, this file structure is all set up using `getDataPackages()`. Defaults to the current workign directory (which is the default location for `getDataPackages()`). 
#' @param dataFormat is a character value indicating the format of the data set(s) within the data package. Currently the only supported option is: *.csv for comma separated value text files. Defaults to "csv".
#' @param metadataFormat is a character value indicating the format of the metadata file within the data package. Currently the only supported format is Ecological Metadata (EML) and the parameter defaults to EML.
#' @param simplify logical. Defaults to TRUE. If there is only a single data package loaded, the function will return a simple list of tibbles (where each tibble reflects a data file from within the data package). If set to FALSE, the function will return a list that contains a list of tibbles. This structure mirrors the object structure returned if multiple data packages are simultaneously loaded (a list of data packages with each data package containing a list of tibbles where each tibble corresponds to a data file in the given data package).
#'
#' @keywords private
#'
#' @return a list of (of lists of) tibbles. 
#'
#' @examples
#' \dontrun{
#' load_data_packages(2272461, data_format = "csv", metadata_format = "eml")
#' }
#'
load_data_packages <- function(datastore_reference, 
                             path = here::here(),
                             dataFormat = "*.csv",
                             metadataFormat = "EML",
                             simplify = TRUE){
  #capture original working directory
  orig_wd <- getwd()
  #set directory back to original working directory on exit.
  on.exit(setwd(orig_wd))
  #set wd to path; defaults to wd. 
  setwd(path)
  
  
  if(length(
    seq_along(
      datastore_reference)) == 1 
    & datastore_reference != "allData" 
    & simplify == TRUE) {
  #return a tibble of data files  
  }
  if(length(
    seq_along(
      datastore_reference)) == 1 
    & datastore_reference != "allData" 
    & simplify == FALSE) {
    #return a list of of tibbles, each tibble corresponds to a data files  
  }
  
  

  for(i in 1:seq_along(datastore_reference)){
    dataPackageDirectory <- paste("data/", datastore_reference[i])
    filenames <- list.files(
      path = dataPackageDirectory,
      pattern = dataFormat)
    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)
    
    ### Load all files into tibbles
    dataStoreReference[i] <- list()
    for (j in names) {
      filepath <- file.path(dataPackageDirectory, paste(j, ".csv", sep = ""))
      tibble_List[[i]] <- assign(j, readr::read_csv(filepath, show_col_types = FALSE))
    }

    
    
    
    
    
        
  }
  
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
