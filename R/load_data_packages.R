#' Read contents of data package(s) and return a tibble with a tibble for each data file. 
#' 
#' `r lifecycle::badge("experimental")`
#'
#' @description `load_data_packages()` loads one to may data packages and returns a tibble of tibbles where each data package is a tibble and within that each data file is it's own tibble. `load_data_packages()` will only work with .csv data files and EML metadata. `load_data_packages()` can also utilize the metadata to assign attributes to each data column.
#' 
#' @details currently `load_data_packages()` only supports EML metadata and .csv files. To take advantage of the default settings in load_data_packages, use the default settings in `get_data_package()` or `get_data_packages()`. Archived (.zip) files must be extracted before `load_data_packages()` will work properly. Again, `get_data_package()` or `get_data_packages()` will accomplish this for you. 
#' '
#' @param reference_id is a list of 6-7 digit numbers corresponding to the DataStore reference ID of the datapackage(s) to load. Alternatively, you can set `reference_id` to "load_all", which will load all the data packages in your /data folder.
#' @param directory is the location of a folder, 'data' (created during `get_data_packages()`) which contains sub-directories where each sub-directory is the DataStore referenceId of the data package. Again, this file structure is all set up using `get_data_packages()`. Defaults to the current working directory (which is the default location for `get_data_packages()`).
#' @param assign_attributes Logical. Defaults to FALSE. Data will be loaded using `readr::read_csv()` guessing algorithm for calling column types. If set to TRUE, column types will be set using metadata attributes via the yet-to-be written `load_metadata()` function. `r lifecycle::badge('experimental')`
#' @param simplify Logical. Defaults to TRUE. If there is only a single data package loaded, the function will return a simple list of tibbles (where each tibble reflects a data file from within the data package). If set to FALSE, the function will return a list that contains a list of tibbles. This structure mirrors the object structure returned if multiple data packages are simultaneously loaded (a list of data packages with each data package containing a list of tibbles where each tibble corresponds to a data file in the given data package).
#'
#' @return a list of (of lists of) tibbles. 
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' dat <- load_data_packages(2272461)
#' }
#'
load_data_packages <- function(reference_id, 
                             directory = here::here(),
                             assign_attributes = FALSE,
                             simplify = TRUE){
  #capture original working directory
  orig_wd <- getwd()
  #set directory back to original working directory on exit.
  on.exit(setwd(orig_wd))
  #set wd to path; defaults to wd. 
  setwd(directory)
  
  #is user specifies "allData" get all directories from the data folder:
  if (reference_id == "all_data") {
    reference_id <- list.dirs(path = directory,
                                     full.names = FALSE,
                                     recursive = FALSE)
  }
  
  ### fix how single data packages are handled later:
  if(length(
    seq_along(
      reference_id)) == 1 
    & reference_id != "all_data" 
    & simplify == TRUE) {
  #return a tibble of data files  
  }
  
  
  for(i in 1:seq_along(reference_id)){
    data_package_directory <- paste("data/", reference_id[i])
    filenames <- list.files(
      path = data_package_directory,
      pattern = data_format)
    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)
    
    ### Load all files into tibbles
    reference_id[i] <- list()
    for (j in names) {
      filepath <- file.path(data_package_directory, paste(j, ".csv", sep = ""))
      tibble_list[[i]] <- assign(j,
                                 readr::read_csv(filepath,
                                                 show_col_types = FALSE))
    }
  }
  
  data_package_filename <- paste0(data_package_directory, "/", reference_id,
                               ".zip")

  if (data_format == "csv" & metadata_format == "eml") {
    filelist <- utils::unzip(data_package_filename, list = TRUE)
    if (assign_attributes == TRUE) {
      #assign attributes using metadata via a yet-to-be-built sub-function.
    }
    return(fileList)
  }  else {
    print("data/metadata format combination not supported")
  }
}
