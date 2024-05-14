


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
#' @description first runs `check_reference_exists()`. Assuming the reference exists, the function test whether the reference is a data package or not. If it is a data package, the value "TRUE" is returned. If it is not a data package, the value FALSE is returned. The default setting assumes the user is logged on to the VPN (secure = TRUE), but this can be set to secure = FALSE for non-NPS users.
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
  #check whether reference exists:
  good_ref_id <- check_ref_exists(reference_id = reference_id,
                                  secure = secure)
  if (good_ref_id == FALSE) {
    stop("Incorrect or unavailable reference id provided.")
  } 
  #assuming it does exist:
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


check_new_version <- function(referenc_id) {
  url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id)
  test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(test_req)$status_code
  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get version info:
  ref_data <- jsonlite::fromJSON(httr::content(test_req, "text"))
  ref_type <- ref_data$referenceType
  
}