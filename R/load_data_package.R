#' Read contents of data package and constructs a list of tibbles based on the data file(s)
#'
#' `r lifecycle::badge("deprecated")`
#' @description `load_data_package_deprecated()` reads the data file(s) from a package and loads it into a list of tibbles. Current implementation only supports .csv data files.
#'
#' @param reference_id is a 6-7 digit number corresponding to the reference ID of the data package.
#'
#' @return a list of one or more tibbles contained within the data package to the global environment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_data_package_deprecated(2272461)
#' }
load_data_package_deprecated <- function(reference_id) {
  data_package_directory <- paste("data/", reference_id, sep = "")
  data_package_filename <- paste(data_package_directory, ".zip", sep = "")
  
  lifecycle::deprecate_warn("0.3.2",
                            "load_data_pacakge_deprecated()",
                            "load_data_packages()")

  # Look for the zipped data package and attempt to unzip it. If the zipped file exists but cannot be unzipped, give the user a warning. If neither the unzipped nor zipped data packages exist, suggest the user check their working directory or use getDataPackage() to get the data package.
  if (!file.exists(data_package_directory)) {
    if (file.exists(data_package_filename)) {
      tryCatch(expr = {
        file_list <- utils::unzip(data_package_filename, list = TRUE)
      }, error = function(e) {
        message("The zip file cannot be unzipped.")
        stop()
      })
    }
    if (!file.exists(data_package_filename)) {
      cat("The data package zip folder for Holding ",
        crayon::blue$bold(reference_id), " was not found.\n",
        sep = ""
      )
      cat("Check to make sure you have the correct working directory.\n")
      cat("Or try using ", crayon::green$bold("get_data_packages()"), sep = "")
    }
  }

  # Older data packages may not have a "data" sub-folder. This if statement looks for that sub-folder and if it does not exist loads the first .csv file in the data package folder.
  # if(!file.exists(paste0("data/",HoldingID,"/data"))){
  #  csvfile<-subset(fileList, grepl(".csv",Name))
  #   csvFilename <- paste(DataPackageDirectory,"/",csvfile[1],sep="")
  #   workingdatafile<-read.csv(csvFilename)
  # }


  # if there is no "data" sub-folder, load all .csv files into tibbles and names each tibble based on the .csv file name (tibble to speed up processing).
  #### RLB: will need to be expanded to handle .xml and .json
  if (!file.exists(paste0("/data/", reference_id, "/data"))) {
    filenames <- list.files(
      path = data_package_directory,
      pattern = "*csv"
    )
    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)
    ### Load all files into tibbles
    tibble_list <- list()
    for (i in names) {
      file_path <- file.path(data_package_directory, paste(i, ".csv", sep = ""))
      suppressWarnings(tibble_list[[i]] <- assign(i,
                                                  readr::read_csv(file_path,
                                                  show_col_types = FALSE)))
    }
    return(tibble_list)
  }


  # updated data package specs. As of 3/2022 all data will be in a sub-folder, "data". This if statement finds the first .csv file in that folder and opens it. Will need more tweaking to accommodate multiple .csv files.
  # if(file.exists(paste0("data/",HoldingID,"/data"))){
  #  FileList<-paste0("data/", HoldingID, "/data/")
  #  csvFilename<-subset(fileList, grepl(".csv",Name))
  #  csvFilename<-paste(DataPackageDirectory, "/", csvfile[1], sep="")
  #  workingdatafile<-read.csv(csvFilename)
  # }

  # Looks for data within a "data" sub-folder for data packages (moslty generated after 3/2022). Loads all .csv files in the "data" sub-folder into tibbles (to speed up processing) and gives each tibble the filename of the .csv it came from (without .csv extension)
  #### RLB: will need to be expanded to handle .xml and .json
  if (file.exists(paste0("data/", reference_id, "/data"))) {
    ## change directories to the /data sub-directory within the data package
    data_package_directory <- paste0(data_package_directory, "/data")

    filenames <- list.files(path = data_package_directory, pattern = "*csv")

    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)

    ### Load all files
    tibble_list <- list()
    for (i in names) {
      file_path <- file.path(data_package_directory, paste(i, ".csv", sep = ""))
      tibble_list[[i]] <- assign(i, readr::read_csv(file_path, show_col_types = FALSE))
    }
    return(tibble_list)
  }
}
