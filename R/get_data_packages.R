#' Retrieve digital data package holding from DataStore.
#'
#' @description `get_data_packages()` creates a directory called "data" in the current working directory (unless it already exists). For each data package, it writes a new sub-directory of "data" named with the corresponding data package reference ID. All the data package files are then copied to that directory. 
#'
#' @param reference_id is a 6-7 digit number corresponding to the reference ID of the data package.
#' @param secure logical indicating whether the file should be acquired using data services available to NPS internal staff only. Defaults to FALSE for public data. TRUE indicates internal data and requires a VPN connection (unless you are in an NPS office).
#' @param path String. Indicates the location that the "data" directory and all subdirectories and files should be written to. Defaults to the working directory.
#' @param force Logical. Defaults to FALSE. In the FALSE condition, the user is prompted if a directory already exists and asked whether to overwrite it or not. The user is also given information about which files are being downloaded, extracted, deleted, and where they are being written to. It also provides information about what errors (if any) were encountered. A user may choose to set FORCE = TRUE, especially when generating scripts to minimize print statements to the console.  When force is set to TRUE, all existing files are automatically overwritten without prompting. Feedback about which files are being downloaded and where is not reported. Only critical errors that stop the function (such as failed API calls) generate warnings.
#'
#' @export
#' @return String. The path (including the /data folder) where all data package sub-directories and data files are contained.
#'
#' @examples
#' \dontrun{
#' #get a public data package
#' get_data_packages(2272461, secure = FALSE)
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
                              force = FALSE) {
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
  #Secure route (needs further testing).
  if (toupper(secure) == "TRUE") {
    for(i in seq_along(reference_id)){
      #create a package-specific sub-directory within "data", if necessary:
      destination_dir <- paste("data/",
                               reference_id[i],
                               sep = "")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# To impliment after DataStore patch 2.9 is pushed ----
      #check for newer version:
      #if(force == FALSE){
      #  url <- paste0(
      #    "https://irmaservices.nps.gov/datastore-secure/v5/rest/ReferenceCodeSearch?q=",
      #    reference_id[i])
        #api call to see if ref exists
      #  test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
      #  status_code <- httr::stop_for_status(test_req)$status_code
      #  #if API call fails, alert user and remind them to log on to VPN:
      #  if(!status_code == 200){
      #    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
      #  }
        #get version info:
      #  ref_data <- jsonlite::fromJSON(
      #    httr::content(
      #      test_req, "text"))
      # version <-ref_data$newestVersion
        #date <- stringr::sub_str(ref_data$dateOfIssue, 1, 10)
      #  if(!is.na(version)){
      #    cat("There is a newer version of ",
      #        crayon::blue$bold(reference_id[i]),
      #        " available.\n",
      #        sep = "")
      #    cat("The newest version, ", 
      #          crayon::bold$blue(version)),
      #        " was uploaded on ",
      #          crayon::bold$blue(date),
      #        ".\n",
      #        sep = "")
      #    cat("Would you like to download the newest version instead?\n\n")
      #    var2 <- readline(prompt = "1: Yes\n2: No\n")
      #    if(var1 == 1){
      #      reference_id[i] <- version
      #    }
      #  }
      #}     
      #if the directory already exists, prompt user to overwrite:
# End version testing code section----
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
      if (file.exists(destination_dir) & force == FALSE){
        cat("The directory ",
            crayon::blue$bold(destination_dir),
            " already exists.\n",
            sep = "")
        cat("Write over the existing directory and its contents?\n\n")
        var1 <- readline(prompt = "1: Yes\n2: No\n")
        if(var1 == 2){
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
      if (!file.exists(destination_dir)) {
        dir.create(destination_dir)
      }
      #get HoldingID from the ReferenceID - defaults to the first holding
      rest_holding_info_url <- paste0(
        "https://irmaservices.nps.gov/datastore-secure/v5/rest/reference/",
        reference_id[i], "/DigitalFiles")
      xml <- suppressMessages(httr::content(httr::GET(rest_holding_info_url,
                                   httr::authenticate(":", ":", "ntlm"))))
        #download each file in the holding:
      for(j in seq_along(xml)){
        #get file URL
        tryCatch(
          {rest_download_url <- paste0(
            "https://irmaservices.nps.gov/datastore-secure",
            "/v4/rest/DownloadFile/",xml[[j]]$resourceId)},
          error = function(e){
            cat(crayon::red$bold(
              "ERROR: You do not have permissions to access ",
              crayon::blue$bold(reference_id[i]),
              ".\n", sep = ""))
            cat("Try logging on to the NPS VPN before running ",
                crayon::green$bold("get_data_package()"),
                ".\n",
                sep = "")
            cat(crayon::red$bold("Function terminated.\n"))
            stop()
          }
          #finally = {}
          #finally = { stop(cat(crayon::red$bold("Function terminated.\n"))) } 
        )
        
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
        if(force == FALSE){
          cat("writing: ",
              crayon::blue$bold(download_file_path),
              ".\n", sep="")
        }    
        #test for .zip; if found extract files and delete .zip file.
        if (tools::file_ext(tolower(download_filename)) == "zip") {
          tryCatch(
            {utils::unzip(zipfile=paste0("data/",
                                  reference_id[i], "/",
                                  download_filename),
            exdir = paste0("data/", reference_id[i]))
            if(force == FALSE){
              cat(crayon::blue$bold(download_filename),
                "was unzipped.\n")
            }
            },
            warning = function(w){
              if(stringr::str_detect(w$message, "-1") & force == FALSE){
                cat("The .zip file appears empty. ",
                    crayon::red$bold("Are you sure you have permissions to access this file?\n"),
                    sep = "")
              }
              if(stringr::str_detect(w$message, "3") & force == FALSE){
                cat("There was an undiagnosed problem with the .zip file. ",
                    crayon::red$bold("Please double check your results.\n"),
                    sep = "")
              }
            },
            finally = {
              if(force == FALSE){
              cat("unzipping ", 
                  crayon::blue$bold(download_filename),
                  ".\n",
                  sep="")
              }
              #remove .zip after extracting
              file.remove(paste0("data/", reference_id[i], "/",
                                 download_filename))
              if(force == FALSE){
                cat("The original .zip file was removed.\n")
              }
            }
          )
        }
      }
    }
  }
  #public/non-secure route:
  if (toupper(secure) == FALSE){
    for(i in seq_along(reference_id)){
      
      #if necessary, create a package-specific directory within the /data:
      destination_dir <- paste("data/", reference_id[i], sep = "")
      
      #if the directory already exists, prompt user to overwrite:
      if (file.exists(destination_dir) & force == FALSE){
        cat("The directory ",
            crayon::blue$bold(destination_dir),
            " already exists.\n",
            sep = "")
        cat("Write over the existing directory and its contents?\n\n")
        var1 <- readline(prompt = "1: Yes\n2: No\n")
        if(var1 == 2){
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
      if (!file.exists(destination_dir)) {
        dir.create(destination_dir)
      }
      # get the HoldingID from the ReferenceID
      rest_holding_info_url <- paste0(
      "https://irmaservices.nps.gov/datastore/v5/rest/reference/",
      reference_id[i], "/DigitalFiles")
      xml <- httr::content(httr::GET(rest_holding_info_url))
      
      #test whether requires secure=TRUE & VPN; alert user:
      if(!is.null(xml$message)){
        cat("For ", crayon::blue$bold(reference_id[i]), " ", 
            crayon::red$bold(xml$message), "\n", sep="")
        cat("Please re-run ", crayon::green$bold("get_data_package()"), 
            " and set ", crayon::bold$blue("secure=TRUE"), ".\n", sep="")
        cat("Don't forget to log on to the VPN!\n")
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
          if(force == FALSE){
            cat("writing: ",
                crayon::blue$bold(download_file_path),
                "\n", sep="")
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
              cat("unzipping",
                  crayon::blue$bold(download_filename),
                  ".\n", sep="")
              cat("The original .zip file was removed.\n")
            }
          }
        }
      }
    }
  }
  data_path<-paste0(path, "/data")
  on.exit(return(data_path))
}
