#' Read contents of data package file and constructs a list of tibbles based on the data file(s)
#'
#' @description \code{load_data_package} reads the data file(s) from a package and loads it into a list of tibbles. Current implementation only supports .csv data files. Planned future iterations will support .json and .xml data files.
#'
#' @param holding_id is a 6-7 digit number corresponding to the holding ID of the data package.
#'
#' @return a list of one or more tibbles contained within the data package to the global environment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' load_data_package(2272461)
#' }
load_data_package <- function(holding_id) {
  DataPackageDirectory <- paste("data/", holding_id, sep = "")
  DataPackageFilename <- paste(DataPackageDirectory, ".zip", sep = "")

  # Look for the zipped data package and attempt to unzip it. If the zipped file exists but cannot be unzipped, give the user a warning. If neither the unzipped nor zipped data packages exist, suggest the user check their working directory or use getDataPackage() to get the data package.
  if (!file.exists(DataPackageDirectory)) {
    if (file.exists(DataPackageFilename)) {
      tryCatch(expr = {
        fileList <- utils::unzip(DataPackageFilename, list = TRUE)
      }, error = function(e) {
        message("The zip file cannot be unzipped.")
        stop()
      })
    }
    if (!file.exists(DataPackageFilename)) {
      cat("The data package zip folder for Holding ",
        crayon::blue$bold(holding_id), " was not found.\n",
        sep = ""
      )
      cat("Check to make sure you have the correct working directory.\n")
      cat("Or try using ", crayon::green$bold("get_data_package()"), sep = "")
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
  if (!file.exists(paste0("/data/", holding_id, "/data"))) {
    filenames <- list.files(
      path = DataPackageDirectory,
      pattern = "*csv"
    )
    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)
    ### Load all files into tibbles
    tibble_List <- list()
    for (i in names) {
      filepath <- file.path(DataPackageDirectory, paste(i, ".csv", sep = ""))
      suppressWarnings(tibble_List[[i]] <- assign(i,
                                                  readr::read_csv(filepath,
                                                  show_col_types = FALSE)))
    }
    return(tibble_List)
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
  if (file.exists(paste0("data/", holding_id, "/data"))) {
    ## change directories to the /data sub-directory within the data package
    DataPackageDirectory <- paste0(DataPackageDirectory, "/data")

    filenames <- list.files(path = DataPackageDirectory, pattern = "*csv")

    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)

    ### Load all files
    tibble_List <- list()
    for (i in names) {
      filepath <- file.path(DataPackageDirectory, paste(i, ".csv", sep = ""))
      tibble_List[[i]] <- assign(i, readr::read_csv(filepath, show_col_types = FALSE))
    }
    return(tibble_List)
  }
}
