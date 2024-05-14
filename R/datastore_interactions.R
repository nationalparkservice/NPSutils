


#' Check whether a reference exists on DataStore
#' 
#' @description given a DataStore reference ID, the function will hit the DataStore API to determine whether the reference exists. If it cannot contact DataStore, it will stop with an error that states the connection failed. 
#'
#' @param reference_id Integer. A 7-digit number indicating the datastore reference to be queried.
#' @param secure Logical. Defaults to TRUE, indicating that the user is logged on to the NPS VPN (or is accessing the resource from an NPS office). Can be set to FALSE for non-NPS users.
#'
#' @return logical. TRUE means the reference exists, false means it does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#' check_ref_exists(1234567)
#' check_ref_exists(1234567, secure = FALSE)
#' }
check_ref_exists <- function (reference_id, secure = TRUE) {
  
  test_req <- NULL
  if (secure == FALSE) {
    #using secure api should be an option though...
    url <- paste0(.ds_api(), "ReferenceCodeSearch?q=", reference_id)
    test_req <- httr::GET(url)
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if(!status_code == 200){
      stop("DataStore connection failed. Try setting secure = TRUE and logging on to the VPN.\n")
    }
  }
  if (secure == TRUE) {
    url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id)
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("DataStore connection failed. Are you logged in to the VPN?\n")
    }
  }
  #get API request info:
  ref_data <- jsonlite::fromJSON(httr::content(test_req, "text"))
  
  #if an invalid reference number was supplied (no reference found):
  if(length(ref_data) == 0){
    return(FALSE)
  } else {
    return (TRUE)
  }
}
  
#' Check whether reference is a data package
#' 
#' @description The function tests whether a valid DataStore reference is a data package or not. If it is a data package, the value "TRUE" is returned. If it is not a data package, the value FALSE is returned. The default setting assumes the user is logged on to the VPN (secure = TRUE), but this can be set to secure = FALSE for non-NPS users. This is a relatively simple helper function that will not test whether a reference exists and may produce unexpected errors if the reference does not exist or is not accessible. You are advised to run `check_reference_exists()` prior to using this function.
#'
#' @inheritParams check_ref_exists
#'
#' @return Logical.FALSE if the DataStore reference is not a data package. TRUE if the DataStore reference is a data package.
#' @export
#'
#' @examples
#' \dontrun{
#' check_is_data_package(1234567)
#' check_is_data_package(1234567, secure = FALSE)
#' }
check_is_data_package <- function(reference_id, secure = TRUE) {
  test_req <- NULL
  if (secure == FALSE) {
    url <- paste0(.ds_api(), "ReferenceCodeSearch?q=", reference_id)
    test_req <- httr::GET(url)
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if(!status_code == 200){
      stop("DataStore connection failed. Try setting secure = TRUE and logging on to the VPN.\n")
    }
  }
  if (secure == TRUE) {
    url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id)
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("DataStore connection failed. Are you logged in to the VPN?\n")
    }
  }
  #get reference info:
  ref_data <- jsonlite::fromJSON(httr::content(test_req, "text"))
  ref_type <- ref_data$referenceType
  
  #test whether it is a data package or not:
  if (!identical(ref_type, "Data Package")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Checks whether a reference has a more recent version
#'
#' @description
#' This function tests whether an existing and accessible DataStore reference
#' has a newer version or not. If a newer version exists, the value "TRUE" is returned. If a newer version does not exist, the value FALSE is returned. The default setting assumes the user is logged on to the VPN (secure = TRUE), but this can be set to secure = FALSE for non-NPS users. This is a relatively simple helper function that will not test whether a reference exists and may produce unexpected errors if the reference does not exist or is not accessible. You are advised to run `check_reference_exists()` prior to using this function.
#' 
#' @inheritParams check_ref_exists
#' 
#' @return Logical. TRUE if a newer version exists, FALSE if a newer version
#'  does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#' check_new_version(1234567)
#' check_new_version(1234567, secure = FALSE)
#' }
check_new_version <- function(referenc_id, secure = TRUE) {
  test_req <- NULL
  if (secure == FALSE) {
    #using secure api should be an option though...
    url <- paste0(.ds_api(), "ReferenceCodeSearch?q=", reference_id)
    test_req <- httr::GET(url)
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if(!status_code == 200){
      stop("DataStore connection failed. Try setting secure = TRUE and logging on to the VPN.\n")
    }
  }
  if (secure == TRUE) {
    url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id)
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("DataStore connection failed. Are you logged in to the VPN?\n")
    }
  }
  #get request info:
  ref_data <- jsonlite::fromJSON(httr::content(test_req, "text"))
  #check for a newer version:    
  version <-ref_data$mostRecentVersion
  
  #test for newer version:
  if (!is.na(version)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
