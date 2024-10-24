#' Retrieve digital data package holding(s) from DataStore.
#'
#' @description `get_data_packages()` creates a directory called "data" in the current working directory (or user-specified location, see the `path` option). For each data package, it writes a new sub-directory within the "data" directory named with the corresponding data package reference ID. All the data package files are then copied to the directory specific to the data package. 
#' 
#' @details In the default mode (force = FALSE), `get_data_packages()` will inform a user if the directory for that data package already exists and give the user the option to overwrite it (or skip downloading). In the default mode (force = FALSE), `get_data_packages()` will also check to see if there are newer versions of the requested data package and if there are newer versions, `get_data_packages()` will inform the user of when the requested data package was issued, when the newest data package was issued, and will then ask the user if they would like to download the newest version instead of the requested version. In the default mode (force = FALSE), `get_data_packages()` will warn a user if the reference ID supplied was not found (does not exist or requires VPN) and if a reference ID refers to a product that is not a data package, `get_data_packages()` will ask if the user wants to attempt to download it anyway. `get_data_packages()` will automatically time out if any individual file download exceeds 300 seconds (5 minutes). Very large files or slow internet connections may hit this limit.
#' 
#' Although `get_data_packages()` is designed to handle the data package reference type on DataStore, it should in theory work on any reference type and download all files associated with a reference (it ignores web links/web resources associated with a reference). If the reference includes a .zip file, the file will be downloaded and the contents extracted. The original .zip archive file will then be deleted. If the .zip contains files with the same name as files in the parent directory, the parent directory files will be over-written by the contents of the .zip archive.
#'
#' @param reference_id is a 6-7 digit number corresponding to the reference ID of the data package.
#' @param secure logical indicating whether the file should be acquired using data services available to NPS internal staff only. Defaults to FALSE for public data. TRUE indicates internal data and requires a VPN connection (unless you are in an NPS office).
#' @param path String. Indicates the location that the "data" directory and all sub-directories and files should be written to. Defaults to the working directory.
#' @param force Logical. Defaults to FALSE. In the FALSE condition, the user is prompted if a directory already exists and asked whether to overwrite it or not. The user is also given information about which files are being downloaded, extracted, deleted, and where they are being written to. The user is also notified if there is a newer version of the requested data package and given the option to download the newest version instead of the requested version. It also provides information about what errors (if any) were encountered and give suggestions on how to address them. 
#' 
#' A user may choose to set `force = TRUE`, especially when scripting or batch processing to minimize print statements to the console.  When force is set to TRUE, all existing files are automatically overwritten without prompting. Feedback about which files are being downloaded and where is not reported. The user is not informed of newer versions of the requested data packages; only the exact reference specified is downloaded. If a reference ID corresponds to something that is not a data package, the contents will be downloaded anyway. Only critical errors that stop the function (such as failed API calls) generate warnings. Failed downloads (invalid reference ID, insufficient DataStore privileges) will result in an empty folder corresponding to that data package within the 'data' folder. 
#' @param dev Logical. Defaults to FALSE. FALSE indicates all operations will be performed in DataStore's production environment. Setting dev = TRUE enables running functions against the DataStore development environment. Working in the development environment will allow you to test functions and code without affecting the publicly accessible DataStore application. 
#' 
#'
#' @export
#' @return String. The path (including the /data folder) where all data package sub-directories and data files are contained.
#'
#' @examples
#' \dontrun{
#' #get a public data package
#' get_data_packages(2300498, secure = FALSE)
#' 
#' #get a list of data packages, some public some restricted
#' get_data_packages(c(2272461, 1234567, 7654321), secure = TRUE)
#' 
#' #pass a list of data packages to retrieve to the function:
#' data_packages<-c(2272461, 1234567, 7654321)
#' get_data_packages(data_packages, secure = TRUE, path="../../my_custom_directory")
#' 
#' #get a data package and return the path the data package is saved to
#' pathToDataForPiping <- get_data_packages(2272461, secure = TRUE, force = FALSE)
#' }
get_data_packages <- function(reference_id,
                              secure = FALSE,
                              path=here::here(),
                              force = FALSE,
                              dev = FALSE) {
  #capture original working directory
  orig_wd <- getwd()
  #set directory back to original working directory on exit.
  on.exit(setwd(orig_wd), add=TRUE)
  #set wd to path; defaults to wd. 
  setwd(path)
  #create "data" directory, if necessary:
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  #enforce proper specification of TRUE/FALSE parameters:
  secure <- toupper(secure)
  force <- toupper(force)
  dev <- toupper(dev)
  
  for (i in seq_along(reference_id)) {
    if(force == FALSE) {
      cat("Working on: ", crayon::bold$green(reference_id[i]), ".\n", sep = "")
    }
    # does reference exist?
    exists <- NPSutils::check_ref_exists(reference_id = reference_id,
                                         secure = secure,
                                         dev = dev)
    if (exists == FALSE) {
      cat("Reference ", reference_id[i],
         " does not exist. Are you logged on to the VPN?\n",
          sep = "")
      next
    }
    #is ref a data package?
    data_package <- check_is_data_package(reference_id = reference_id,
                                          secure = secure,
                                          dev = dev)
    if (data_package == FALSE) {
      if (force == FALSE) {
        cat("Error: reference ", crayon::red$bold(reference_id[i]),
            " is not a data package.\n", sep ="")
        cat("Would you like to attempt to download the resource anyway?\n\n")
        cat("1: Yes\n2: No\n")
        var1 <- .get_user_input()
        if (var1 == 2) {
          next
        }
      }
      if (force == FALSE) {
        cat("Reference ", crayon::red$bold(reference_id[i]),
          " is not a data package. Attempting to download anyway.\n", sep = "")
      }
    }
    
    #if interactive, ask whether to download newer version
    if (force == FALSE) {
      new_version <- check_new_version(reference_id = reference_id,
                                       secure = secure,
                                       dev = dev)
      if (new_version == TRUE){
        cat("For reference ", crayon::bold$blue(reference_id[i]), 
            " a newer version exists.\n", sep ="")
        cat("Do you want to download the newest version?\n\n")
        cat("1: Yes\n 2: No\n")
        var2 <- .get_user_input()
        if(var2 == 1){
          reference_id <- get_new_version_id(reference_id = reference_id,
                                             secure = secure,
                                             dev = dev)
        }
      }
    }
    
    #if necessary, create a package-specific directory within the /data:
    destination_dir <- paste("data/", reference_id[i], sep = "")
    #if the directory already exists, prompt user to overwrite:
    if(force == FALSE) {
      if (file.exists(destination_dir)){
        cat("The directory ",
            crayon::blue$bold(destination_dir),
            " already exists.\n",
            sep = "")
        cat("Write over the existing directory and its contents?\n\n")
        cat("1: Yes\n2: No\n")
        var3 <- .get_user_input()
        if(var3 == 2){
          cat("The original directory ",
              crayon::blue$bold(destination_dir),
              " was retained.\n",
              sep = "")
          cat("Data package ",
              crayon::blue$bold(reference_id[i]),
              " was not downloaded.\n\n",
              sep = "")
          next #exit this iteration and go on to the next one
        }
      }
    }
    #if the directory doesn't exist, create it:
    if (!file.exists(destination_dir)) {
      dir.create(destination_dir)
    }
    
    #get reference holding IDs:
    #public references:
    if (secure == FALSE && dev == FALSE) {
      rest_holding_info_url <- paste0(.ds_api(),
                                      "reference/",
                                      reference_id[i],
                                      "/DigitalFiles")
    }
    # restricted references:
    if (secure == TRUE && dev == FALSE) {
      rest_holding_info_url <- paste0(.ds_secure_api(),
                                      "reference/",
                                      reference_id[i],
                                      "/DigitalFiles")
    }
    # references on dev server:
    if (dev == TRUE) {
      rest_holding_info_url <- paste0(.ds_dev_api(),
                                      "reference/",
                                      reference_id[i],
                                      "/DigitalFiles")
    }
  
    xml <- suppressMessages(httr::content
                            (httr::GET(
                              rest_holding_info_url,
                              httr::authenticate(":", ":", "ntlm"))))
    
    #test whether requires secure=TRUE & VPN; alert user:
    if(!is.null(xml$message)){
      if(force == FALSE){
        if(nchar(xml$message) <= 90){
          cat("For ", crayon::blue$bold(reference_id[i]), " ", 
              crayon::red$bold(xml$message), "\n", sep="")
          cat("Please re-run ", crayon::green$bold("get_data_packages()"), 
              " and set ", crayon::bold$blue("secure=TRUE"), ".\n", sep="")
          cat("Don't forget to log on to the VPN!\n")
        }
        else{
          cat(crayon::red$bold("ERROR:"), xml$exceptionMessage)
          cat("The resource was not downloaded.\n\n")
        }
      }
      next #move on to the next reference
    }
    
    if(is.null(xml$message)){
      for(j in seq_along(xml)){
        rest_download_url <- xml[[j]]$downloadLink
        download_filename <- xml[[j]]$fileName
        download_file_path <- paste0("data/",
                                     reference_id[i], "/",
                                     download_filename)
        #download the file:
        invisible(capture.output(
          suppressMessages(httr::content(
            httr::GET(
              rest_download_url,
              httr::timeout(300),
              httr::progress(),
              httr::write_disk(download_file_path,
                               overwrite = TRUE),
              httr::authenticate(":", ":", "ntlm"))))))
        if(force == FALSE){
          cat("Writing: ",
              crayon::blue$bold(download_file_path),
              ".\n", sep="")
        }      
        # check to see if the downloaded file is a zip; unzip.
        if (tools::file_ext(tolower(download_filename)) == "zip") {
          utils::unzip(zipfile = paste0("data\\",
                                        reference_id[i], "\\",
                                        download_filename),
                       exdir = paste0("data\\", reference_id[i]))
          #delete .zip file
          file.remove(paste0("data/",
                             reference_id[i],
                             "/",
                             download_filename))
          if(force == FALSE){
            cat("Unzipping ",
                crayon::blue$bold(download_filename),
                ".\n", sep="")
            cat("The original .zip file was removed.\n\n")
          }
        }
      }
    }
  }
  data_path <- paste0(path, "/data")
  if (force == FALSE) {
    cat("Any downloaded data package(s) can be found at:\n")
  }
  on.exit(return(data_path))
}


#' @export
#' @rdname get_data_packages
get_data_package <- function(reference_id,
                            secure = FALSE,
                            path=here::here(),
                            force = FALSE,
                            dev = FALSE) {
  
  x <- get_data_packages(reference_id,
                         secure = secure,
                         path = path,
                         force = force,
                         dev = dev)
  return(x)
}
