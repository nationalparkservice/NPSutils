#' Retrieve digital data package holding from DataStore.
#'
#' @description get_data_package creates a directory called "data" in the current working directory (unless it already exists). For each data package, it writes a new sub-directory of "data" named with the corresponding data package reference ID. All the data package files are then copied to that directory. .Zip archives files are extracted and the original .zip file is deleted. Remember, to download restricted data packages, set secure=TRUE *and* log on to the VPN.
#'
#' @param reference_id is a 6-7 digit number corresponding to the reference ID of the data package.
#' @param secure logical indicating whether the file should be acquired using data services available to NPS internal staff only. Defaults to FALSE for public data. TRUE indicates internal data and requires a VPN connection (unless you are in an NPS office).
#' @param path the directory where data directories and data files will be saved. Defaults to current working directory.
#'
#' @export
#' @return String. The path where the /data folder is written and all data package sub-directories and data files are contained.
#'
#' @examples
#' \dontrun{
#' get_data_package(2272461, secure = FALSE)
#' path_to_data_for_piping <- get_data_package(2272461, secure = TRUE)
#' }
get_data_package <- function(reference_id, secure = FALSE, path=here::here()) {
  
  #capture original working directory
  orig_wd <- getwd()
  #return the directory where /data is written.
  on.exit(return(getwd()))
  #set directory back to original working directory on exit.
  on.exit(setwd(orig_wd), add=TRUE)
  
  #set wd to path; defaults to wd. 
  setwd(path)
  
  #create "data" directory, if necessary:
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  #Secure route (needs further testing).
  if (toupper(secure) == "TRUE") {
    for(i in seq_along(reference_id)){
      #create a package-specific sub-directory within "data", if necessary:
      destination_dir <- paste("data/", reference_id[i], sep = "")
      if (!file.exists(destination_dir)) {
        dir.create(destination_dir)
      }
      #get HoldingID from the ReferenceID - defaults to the first holding
      rest_holding_info_url <- paste0(
        "https://irmaservices.nps.gov/datastore-secure/v4/rest/reference/",
        reference_id[i], "/DigitalFiles")
      xml <- httr::content(httr::GET(rest_holding_info_url,
                                   httr::authenticate(":", ":", "ntlm")))
      #download each file in the holding:
      for(j in seq_along(xml)){
        rest_download_url <- paste0(
          "https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/",
          xml[[j]]$resourceId)
      
        download_filename <- xml[[j]]$fileName
        download_file_path <- paste0("data/", reference_id[i], "/",
                                   download_filename)
        #download the file:
        invisible(capture.output(
          httr::content(
            httr::GET(
              rest_download_url,
              httr::write_disk(download_file_path,
                               overwrite = TRUE),
              httr::authenticate(":", ":", "ntlm")))))
        cat("writing: ", crayon::blue$bold(download_file_path), "\n", sep="")
      
        #test for .zip; if found extract files and delete .zip file.
        if (tools::file_ext(download_filename) == "zip") {
          tryCatch(
            {utils::unzip(zipfile=paste0("data/",
                                  reference_id[i], "/",
                                  download_filename),
            exdir = paste0("data/", reference_id[i]))
            cat("    ", crayon::blue$bold(download_filename),
                "was unzipped.\n")},
            warning = function(w){
              cat(crayon::red$bold("     The .zip file appears empty. Are you sure you have permissions to access this file?"), "\n")},
            finally = {
              file.remove(paste0("data/", reference_id[i], "/",
                                 download_filename))
              cat("     The original .zip file was removed.\n")}
            )
        }
      }
    }
  }
  #public/non-secure route:
  if (toupper(secure) == "FALSE"){
    
    for(i in seq_along(reference_id)){
      #if necessary, create a package-specific directory within the /data:
      destination_dir <- paste("data/", reference_id[i], sep = "")
      if (!file.exists(destination_dir)) {
        dir.create(destination_dir)
      }
      
      # get the HoldingID from the ReferenceID
      rest_holding_info_url <- paste0(
      "https://irmaservices.nps.gov/datastore/v4/rest/reference/",
      reference_id[i], "/DigitalFiles")
      xml <- httr::content(httr::GET(rest_holding_info_url))
      
      #test whether requires secure=TRUE & VPN; alert user:
      if(!is.null(xml$message)){
        cat("For ", crayon::blue$bold(reference_id[i]), " ", 
            crayon::red$bold(xml$message), "\n", sep="")
        cat("Please re-run ", crayon::green$bold("get_data_package()"), 
            " and set ", crayon::bold$blue("secure=TRUE"), ".\n", sep="")
        cat("Don't forget to log on to the VPN!")
      }
      #download all files in reference:
      if(is.null(xml$message)){
        for(j in seq_along(xml)){
          rest_download_url <- xml[[j]]$downloadLink
          download_filename <- xml[[j]]$fileName
          download_file_path <- paste0("data/",
                                     reference_id[i], "/",
                                     download_filename)
          #independent tests show download.file is faster than httr::GET or curl
          download.file(rest_download_url, 
                      download_file_path, 
                      quiet=TRUE, 
                      mode="wb")
          cat("writing: ", crayon::blue$bold(download_file_path), "\n", sep="")
        }
        # check to see if the downloaded file is a zip; unzip.
        if (tools::file_ext(download_filename) == "zip") {
          utils::unzip(zipfile = paste0("data\\",
                                  reference_id[i], "\\",
                                  download_filename),
                   exdir = paste0("data\\", reference_id[i]))
          #delete .zip file
          file.remove(paste0("data/", reference_id[i], "/", download_filename))
          cat("    ", crayon::blue$bold(download_filename), "was unzipped.\n")
          cat("     The original .zip file was removed.\n")
        }
      }
    }
  }
}