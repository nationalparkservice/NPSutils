


#' Check whether a reference exists on DataStore
#' 
#' @description given a DataStore reference ID, the function will hit the DataStore API to determine whether the reference exists. If it cannot contact DataStore, it will stop with an error that states the connection failed. 
#'
#' @param reference_id string a 7-digit number indicating the datastore reference to be queried.
#'
#' @return logical. TRUE means the reference exists, false means it does not exist.
#' @export
#'
#' @examples
#' \dontrun{
#' check_ref_exists(1234567)
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
  
check_is_data_package <- function(reference_id) {
  #using secure api should be an option; can this happen silently?
  url <- paste0(.ds_api(), "ReferenceCodeSearch?q=", reference_id)
  
  #does this really need to authenticate?
  test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(test_req)$status_code
  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get reference info:
  ref_data <- jsonlite::fromJSON(httr::content(test_req, "text"))
  
  
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