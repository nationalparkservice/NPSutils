


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
check_ref_exists <- function (reference_id) {
  #construct request url:
  url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id[i])
  
  #api call to see if ref exists
  test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(test_req)$status_code
  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get version info:
  ref_data <- jsonlite::fromJSON(httr::content(test_req, "text"))
  
  #if an invalid reference number was supplied (no reference found):
  if(length(ref_data) == 0){
    return(FALSE)
  } else {
    return (TRUE)
  }
}
  
  #Alert to incorrect (not data package) reference type:
  ref_type <- ref_data$referenceType
  if(!identical(ref_type, "Data Package")){
    cat("Error: reference ", crayon::red$bold(reference_id[i]),
        " is a ", crayon::red$bold(ref_type),
        " not a data package.\n", sep ="")
    cat("Would you like to attempt to download the resource anyway?\n\n")
}

check_new_version <- function(referenc_id) {
  url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", reference_id[i])
  
}