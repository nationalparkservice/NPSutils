#' Retrieve digital data package holding(s) from DataStore.
#'
#' @description `get_data_packages()` creates a directory called "data" in the current working directory (or user-specified location, see the `path` option). For each data package, it writes a new sub-directory within the "data" directory named with the corresponding data package reference ID. All the data package files are then copied to the directory specific to the data package. 
#' 
#' @detail In the default mode (force = FALSE), `get_data_packages()` will inform a user if the directory for that data package already exists and give the user the option to overwrite it (or skip downloading). In the default mode (force = FALSE), `get_data_packages()` will also check to see if there are newer versions of the requested data package and if there are newer versions, `get_data_packages()` will inform the user of when the requested data package was issued, when the newest data package was issued, and will then ask the user if they would like to download the newest version instead of the requested version. In the default mode (force = FALSE), `get_data_packages()` will warn a user if the reference ID supplied was not found (does not exist or requires VPN) and if a reference ID refers to a product that is not a data package, `get_data_packages()` will ask if the user wants to attempt to download it anyway. 
#' 
#' Although `get_data_packages()` is designed to handle the data package reference type on DataStore, it should in theory work on any reference type and download all files associated with a reference (it ignores web links/web resources associated with a reference). If the reference includes a .zip file, the file will be downloaded and the contents extracted. The original .zip archive file will then be deleted. If the .zip contains files with the same name as files in the parent directory, the parent directory files will be over-written by the contents of the .zip archive.
#'
#' @param reference_id is a 6-7 digit number corresponding to the reference ID of the data package.
#' @param secure logical indicating whether the file should be acquired using data services available to NPS internal staff only. Defaults to FALSE for public data. TRUE indicates internal data and requires a VPN connection (unless you are in an NPS office).
#' @param path String. Indicates the location that the "data" directory and all sub-directories and files should be written to. Defaults to the working directory.
#' @param force Logical. Defaults to FALSE. In the FALSE condition, the user is prompted if a directory already exists and asked whether to overwrite it or not. The user is also given information about which files are being downloaded, extracted, deleted, and where they are being written to. The user is also notified if there is a newer version of the requested data package and given the option to download the newest version instead of the requested version. It also provides information about what errors (if any) were encountered and give suggestions on how to address them. 
#' 
#' A user may choose to set `force = TRUE`, especially when generating scripts to minimize print statements to the console.  When force is set to TRUE, all existing files are automatically overwritten without prompting. Feedback about which files are being downloaded and where is not reported. The user is not informed of newer versions of the requested data packages; only the exact reference specified is downloaded. If a reference ID corresponds to something that is not a data package, the contents will be downloaded anyway. Only critical errors that stop the function (such as failed API calls) generate warnings. Failed downloads (invalid reference ID, insufficient DataStore privileges) will result in an empty folder corresponding to that data package within the 'data' folder. 
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
      
      #check for newer version:
      if(force == FALSE){
        cat("Working on: ", crayon::bold$green(reference_id[i]), ".\n", sep="")
        url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id[i])
        #api call to see if ref exists
        test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
        status_code <- httr::stop_for_status(test_req)$status_code
        #if API call fails, alert user and remind them to log on to VPN:
        if(!status_code == 200){
          stop("DataStore connection failed. Are you logged in to the VPN?\n")
        }
        #get version info:
        ref_data <- jsonlite::fromJSON(
          httr::content(
            test_req, "text"))
        #if an invalid reference number was supplied (no reference found):
        if(length(ref_data) == 0){
          cat("Invalid DataStore reference ID supplied (",
              crayon::red$bold(reference_id[i]), 
              "). These data were not downloaded.\n\n", sep = "")
          next
        }
        #Alert to incorrect (not data package) reference type:
        ref_type <- ref_data$referenceType
        if(!identical(ref_type, "Data Package")){
          cat("Error: reference ", crayon::red$bold(reference_id[i]),
              " is a ", crayon::red$bold(ref_type),
              " not a data package.\n", sep ="")
          cat("Would you like to attempt to download the resource anyway?\n\n")
          var1 <- readline(prompt = "1: Yes\n2: No\n")
          if(var1 == 2){
            cat("Reference ", reference_id[i], " was not downloaded.\n\n")
            next
          }
        }
        #check for a newer version:    
        version <-ref_data$mostRecentVersion
        if(!is.na(version)){
          newest_url <- paste0(.ds_secure_api(),
                               "ReferenceCodeSearch?q=",
                               version)
          new_req <- httr::GET(newest_url, httr::authenticate(":", ":", "ntlm"))
          new_status <- httr::stop_for_status(new_req)$status_code
          if(!new_status == 200){
            stop("DataStore connection failed. Are you logged in to the VPN?\n")
          }
        new_data <- jsonlite::fromJSON(httr::content(new_req, "text"))
        ref_date <- stringr::str_sub(ref_data$dateOfIssue, 1, 10)
        new_date <- stringr::str_sub(new_data$dateOfIssue, 1, 10)
        cat("Reference ", crayon::blue$bold(reference_id[i]),
            " was issued on ", crayon::blue$bold(ref_date), ".\n", sep = "")
        cat("There is a newer version available.\n", sep = "")
        cat("The newest version, ", crayon::bold$blue(version),
            ", was issued on ", crayon::bold$blue(new_date),
            ".\n", sep = "")
        cat("Would you like to download the newest version instead?\n\n")
        var1 <- readline(prompt = "1: Yes\n2: No\n")
        if(var1 == 1){
          #update to new reference and destination directory
          reference_id[i] <- version
          }
        }
      }
      
      #generate name for package-specific sub-directory within "data", if necessary:
      destination_dir <- paste("data/", reference_id[i], sep = "")
      #if the directory already exists, prompt user to overwrite:
      if(file.exists(destination_dir) & force == FALSE){
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
      #if the directory does not already exist, create it:
      if (!file.exists(destination_dir)) {
        dir.create(destination_dir)
      }
      
      #get HoldingID from the ReferenceID - defaults to the first holding
      rest_holding_info_url <- paste0(.ds_secure_api(),
                                      "reference/",
                                      reference_id[i],
                                      "/DigitalFiles")
        xml <- suppressMessages(httr::content(httr::GET(rest_holding_info_url,
                                   httr::authenticate(":", ":", "ntlm"))))
        
        #catch invalid DataStore ID (code does this above, but only if force = FALSE; this catches it if force = TRUE; better to have both so in a force = FALSE case the error gets caught sooner, fewer API calls, and faster function response times)
        if(!is.null(xml$message)){
          next
        }
        #download each file in the holding:
      for(j in seq_along(xml)){
        #get file URL
        tryCatch(
          {rest_download_url <- paste0(.ds_secure_api(),
                                       "DownloadFile/",
                                       xml[[j]]$resourceId)},
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
        )
        download_filename <- xml[[j]]$fileName
        download_file_path <- paste0("data/", reference_id[i], "/",
                                   download_filename)
        #download the file:
        invisible(capture.output(
          suppressMessages(httr::content(
            httr::GET(
              rest_download_url,
              httr::progress(),
              httr::write_disk(download_file_path,
                               overwrite = TRUE),
              httr::authenticate(":", ":", "ntlm"))))))
        if(force == FALSE){
          cat("Writing: ",
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
                cat(
                  "The .zip file appears empty. ",
                  crayon::red$bold(
                    "Are you sure you have permissions to access this file?\n"),
                    sep = "")
              }
              if(stringr::str_detect(w$message, "3") & force == FALSE){
                cat("There was an undiagnosed problem with the .zip file. ",
                    crayon::red$bold("Please double check your results.\n"),
                    sep = "")
              }
            },
            finally = {
              #remove .zip after extracting
              file.remove(paste0("data/", reference_id[i], "/",
                                 download_filename))
              if(force == FALSE){
                cat("The original .zip file was removed.\n\n")
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
      #check for newer version:
      if(force == FALSE){
        cat("Working on: ", crayon::bold$green(reference_id[i]), ".\n", sep="")
        url <- paste0(.ds_api(),
                      "ReferenceCodeSearch?q=",
                      reference_id[i])
        #api call to see if ref exists
        test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
        status_code <- httr::stop_for_status(test_req)$status_code
        
        #if API call fails, alert user and remind them to log on to VPN:
        if(!status_code == 200){
          stop("DataStore connection failed. 
               Are you sure this reference is public?\n")
        }
        #get version info:
        ref_data <- jsonlite::fromJSON(
          httr::content(
            test_req, "text"))
        
        #if an invalid reference number was supplied (no reference found):
        if(length(ref_data) == 0){
          cat("Reference ",
              crayon::red$bold(reference_id[i]),
              " not found.\n", sep = "")
          cat("Invalid reference ID supplied or VPN required.\n")
          cat("Please re-run ", crayon::green$bold("get_data_packages()"), 
              " and set ", crayon::bold$blue("secure=TRUE"), ".\n", sep="")
          cat("Reference ", crayon::red$bold(reference_id[i]),
              " Not downloaded.\n\n", sep = "")
          next
        }
        #Alert to incorrect (not data package) reference type:
        ref_type <- ref_data$referenceType
        if(!identical(ref_type, "Data Package")){
          cat("Error: reference ", crayon::red$bold(reference_id[i]),
              " is a ", crayon::red$bold(ref_type),
              " not a data package.\n", sep ="")
          cat("Would you like to attempt to download the resource anyway?\n\n")
          var1 <- readline(prompt = "1: Yes\n2: No\n")
          if(var1 == 2){
            cat("Reference ", reference_id[i], " was not downloaded.\n\n")
            next
          }
        }
        #Look for a newer version:
        version <-ref_data$mostRecentVersion
        if(!is.na(version)){
          newest_url <- paste0(.ds_api(),
                               "ReferenceCodeSearch?q=",
                               version)
          new_req <- httr::GET(newest_url, httr::authenticate(":", ":", "ntlm"))
          new_status <- httr::stop_for_status(new_req)$status_code
          if(!new_status == 200){
            stop("DataStore connection failed. 
               Are you sure this reference is public?\n")
          }
          new_data <- jsonlite::fromJSON(
            httr::content(
              new_req, "text"))
          ref_date <- stringr::str_sub(ref_data$dateOfIssue, 1, 10)
          new_date <- stringr::str_sub(new_data$dateOfIssue, 1, 10)
          cat("Reference ", crayon::blue$bold(reference_id[i]),
              " was issued on ", crayon::blue$bold(ref_date), ".\n", sep = "")
          cat("There is a newer version available.\n", sep = "")
          cat("The newest version, ", crayon::bold$blue(version),
              ", was issued on ", crayon::bold$blue(new_date),
              ".\n", sep = "")
          cat("Would you like to download the newest version instead?\n\n")
          var1 <- readline(prompt = "1: Yes\n2: No\n")
          if(var1 == 1){
            #update to new reference and destination directory
            reference_id[i] <- version
          }
        }
      }     
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
      rest_holding_info_url <- paste0(.ds_api(),
                                      "reference/",
                                      reference_id[i],
                                      "/DigitalFiles")
      xml <- httr::content(httr::GET(rest_holding_info_url))
      
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
        next
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
  }
  data_path<-paste0(path, "/data")
  cat("Any downloaded data package(s) can be found at:\n")
  on.exit(return(data_path))
}
