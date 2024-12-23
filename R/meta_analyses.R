#' Get a list of reference codes from DataStore
#' 
#' `get_ref_list` will return a list of the DataStore reference codes associated with a given reference type. Where "All" might be a bit generous: I would not expect more than the number given by "no_of_entries" as that is technically the number of entries per page and the function defaults to returning just one page (not entirely sure what a "page" is in this context).
#' 
#'
#' @param reference_type String. The reference type to to query data store for. Defaults to data package ("dataPackage"). 
#' @param no_of_entries Integer. The number of entries to return per page (where only one "page" of results is returned by default). Defaults to 500. 
#' @param secure Logical. Defaults to FALSE for external users. Setting secure = TRUE will, with the proper credentials, return DataStore references with visibility set to both Public and Restricted.  
#'
#' @return A List of reference IDs
#' @export
#'
#' @examples
#' \dontrun{
#' get_ref_list()
#' }
get_ref_list <- function (reference_type = "dataPackage",
                                 no_of_entries = 500,
                                 secure = FALSE) {
  server <- NULL
  if (secure == TRUE) {
    server <- "https://irmaservices.nps.gov/datastore-secure/v7/rest/"
  }
  if (secure == FALSE) {
    server <- "https://irmaservices.nps.gov/datastore/v7/rest/"
  }
  
  url <- paste0(server,
                "ReferenceTypeSearch/",
                reference_type,
                "?top=",
                no_of_entries,
                "&page=1")
  ref_list <- httr::content(httr::GET(url,
                                      httr::authenticate(":", ":", "ntlm")))
  DS_reference_list <- NULL
  
  for (i in 1:length(seq_along(ref_list[[1]]))) {
    DS_reference_list <- append(DS_reference_list,
                                ref_list[[1]][[i]][["referenceId"]])
  }
  return(DS_reference_list)
}

#' Return Basic information about a list of DataStore References
#' 
#' The function will return a data frame containing information about a given number of references within a reference type. The data returned includes the reference ID (referenceId), the date the references was activated on DataStore (dateOfIssue), the references visibility (visibility), the number of files associated with the reference (fileCount), the access level of the files (fileAccess), the reference title (title), the abbreviated citation (citation), the URL for the DataStore reference (referenceUrl), the group-type for the reference (referenceGroupType), the type of reference (typeName), whether the reference has a DOI associated with it (isDOI), whether their is a newer version of the reference (newVersion) and what the most recent version of the reference is (mostRecentReference).
#' 
#' @param reference_type String. The reference type to to query data store for. Defaults to data package ("dataPackage"). 
#' @param no_of_entries Integer. The number of entries to return per page (where only one "page" of results is returned by default). Defaults to 500. 
#' @param secure Logical. Defaults to FALSE for external users. Setting secure = TRUE will, with the proper credentials, return DataStore references with visibility set to both Public and Restricted.  
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' get_ref_info()
#' }
#' 
get_refs_info <- function (reference_type = "dataPackage",
                          no_of_entries = 500,
                          secure = FALSE) {
  server <- NULL
  if (secure == TRUE) {
    server <- "https://irmaservices.nps.gov/datastore-secure/v7/rest/"
  }
  if (secure == FALSE) {
    server <- "https://irmaservices.nps.gov/datastore/v7/rest/"
  }
  
  url <- paste0(server,
                "ReferenceTypeSearch/",
                reference_type,
                "?top=",
                no_of_entries,
                "&page=1")
  ref_list <- httr::content(httr::GET(url,
                                      httr::authenticate(":", ":", "ntlm")))
  DS_reference_list <- data.frame(referenceId =integer(),
                                  referenceType = character(),
                                  dateOfIssue = as.Date(character()),
                                  visibility = factor(),
                                  fileCount = integer(),
                                  fileAccess = character(),
                                  title = character(),
                                  citation = character(),
                                  referenceUrl = character(),
                                  referenceGroupType = character(),
                                  typeName = character(),
                                  isDOI = logical(),                     
                                  newVersion = character(),
                                  mostRecentVersion = character()
  )
  
  for (i in 1:length(seq_along(ref_list[[1]]))) {
    
    if (is.null(ref_list[[1]][[i]][["newVersion"]])) {
      newVersion <- NA
    } else {
      newVersion <- ref_list[[1]][[i]][["newVersion"]]
    }
    
    if (is.null(ref_list[[1]][[i]][["mostRecentVersion"]])) {
      mostRecentVersion <- NA
    } else {
      mostRecentVersion <- ref_list[[1]][[i]][["mostRecentVersion"]]
    }
    
    ref <- c(ref_list[[1]][[i]][["referenceId"]],
             ref_list[[1]][[i]][["referenceType"]],
             ref_list[[1]][[i]][["dateOfIssue"]],
             ref_list[[1]][[i]][["visibility"]],
             ref_list[[1]][[i]][["fileCount"]],
             ref_list[[1]][[i]][["fileAccess"]],
             ref_list[[1]][[i]][["title"]],
             ref_list[[1]][[i]][["citation"]],
             ref_list[[1]][[i]][["referenceUrl"]],
             ref_list[[1]][[i]][["referenceGroupType"]],
             ref_list[[1]][[i]][["typeName"]],
             ref_list[[1]][[i]][["isDOI"]],                     
             newVersion,
             mostRecentVersion
    )
    
    ref <- t(ref)
    colnames(ref) <- colnames(DS_reference_list)
    
    DS_reference_list <- rbind(DS_reference_list, ref)
    
  }
  return(DS_reference_list)
}

#' Collect summary statistics on data packages
#' 
#' Given a list of data package references from DataStore the function will download the indicated data packages (using creating the folders /data/reference for each data package; see `get_data_packages` for details), load them into R, and then collect some summary statistics on the data packages. 
#' 
#' If a data package fails to download (or load) into R, the function will return NAs instead of summary data about the data package as well as a message about the package status ("Loads", "Error") in the dataframe that the function returns. The function will ignore files that fall outside the data package specifications (one or more .csv files and a single .xml file ending in *_metadata.xml). 
#' 
#' When `check_metadata` is set to the default `FALSE`, the function will attempt to and load any .csv, regardless of the contents. Data packages with restricted access can produce false positives if you do not have the appropriate permissions to download the data as the function will still download the files, but they will be populated with unhelpful hmtl rather than the intended data. In this case, each .csv will be listed as having 5 columns and one row of data. Functions that completely fail to load into R likely violate the data package specifications in some fundamental way (e.g. .CSV file instead of .csv or no .csv files at all).
#' 
#' When `check_metadata` is set to `TRUE`, additional checks and tests are run on the data package and load errors may occur for all of the above reasons and also if there are multiple .xml files, if the metadata file name does not end in "*_metadata.xml", if there is no metadata file, or if the metadata file is EML schema-invalid.
#'  
#' If you have access to restricted DataStore references (e.g. in an NPS office or logged in to an NPS VPN), you can set secure = TRUE. This will give you access to restricted (internal to NPS) references but if a reference is restricted to a named list of individuals you must be on that named list to access the reference.
#' 
#' @param ref_list list or string of data package reference IDs from DataStore (potentially generated via `get_references_list`.
#' @param secure logical. Defaults to TRUE to access secure DataStore server and restricted data packages. Set to FALSE to to access only public references.
#' @param check_metadata Logical. Defaults to FALSE. In this case, metadata will not be checked or loaded. Any load errors will occur due to problems with .csv files (for instance if they don't exist). To test whether the metadata meets minimal requirements (is schema-valid), set check_metadata = TRUE. 
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' x <- get_ref_list()
#' get_ref_info(x[[1]])
#' }
summarize_packages <- function(ref_list,
                               secure = TRUE,
                               check_metadata = FALSE) {
  #setup a dataframe to return data to:
  df <- data.frame(pkgid = character(),
                   status = character(),
                   fileNumber = integer(),
                   colNumber = integer(),
                   cellNumber = integer(),
                   fileSize = integer())
  
  #get data from each data package
  for (i in 1:length(seq_along(ref_list))) {
    # places to add data to for each package
    file_number <- 0
    col_number <- 0
    cell_number <- 0
    file_size <- 0
    
    #This is where the data package will be downloaded to:
    destination_dir <- paste("data/", ref_list[i], sep = "")
    
    #only download if the file/directory does not already exist
    #caution: partially downloaded data packages WILL cause issues here!
    if (!file.exists(destination_dir)) {
      pkg_download <- tryCatch(
        NPSutils::get_data_package(ref_list[i],
                                   secure = secure,
                                   force = TRUE),
        error = function(e) e)
      
      if(inherits(pkg_download, "error")) {
        dat <- data.frame(ref_list[i], "Error", NA, NA, NA, NA)
        colnames(dat) <- colnames(df)
        df <- rbind(df, dat)
        next
      }
    }
    
    #tryCatch to load the package
    pkg <- tryCatch(
      if (check_metadata == FALSE) {
        NPSutils::load_data_package(ref_list[i])
      } else {
        NPSutils::load_data_package(ref_list[i], assign_attributes = TRUE)
        
      },
      error = function(e) e)
    #if loading fails, put in a bunch of NAs instead of data:
    if (inherits(pkg, "error")) {
      dat <- data.frame(ref_list[i], "Error", NA, NA, NA, NA)
      colnames(dat) <- colnames(df)
      df <- rbind(df, dat)
    } else {
      #if loading is successful, get some basic info about the data package:  
      #number of data files:
      file_number <- length(seq_along(pkg))
      
      #number of columns of data
      for (j in 1:length(seq_along(pkg))) {
        col_number <- col_number + ncol(pkg[[j]])
      }
      
      #number of cells of data:
      for(j in 1:length(seq_along(pkg))) {
        cell_number <- 
          cell_number + ncol(pkg[[j]])*nrow(pkg[[j]])
      }
      #total data (not metadata) file size of the data package
      for (j in 1:file_number) {
        file_size <- file_size + file.size(
          list.files(here::here("data",
                                ref_list[i]),
                     full.names = TRUE)[j])
      }
      #put all the info into a dataframe
      dat <- data.frame(ref_list[i],
                        "Loads",
                        file_number,
                        col_number,
                        cell_number,
                        file_size)
      colnames(dat) <- colnames(df)
      #append the package-specific info to the overall dataframe:
      df <- rbind(df, dat)
    }
    #remove the package specific data frame so that it can be re-written
    rm(dat)
  }
  #return the dataframe for the entire set of data packages:
  return(df)
}