#' Validate a Tabular Data Package
#'
#' @description \code{validate_data_package} checks a zipped data package for required files and structure. This function is still under construction.
#'
#' @param file is a path to a zipped data package
#'
#' @export
#'
#' @examples
#' \dontrun{
#' validate_data_package(File = "C:/test/test_package.zip")
#' }
validate_data_package <- function(file) {
  #### It seems like the DPchecker package now performs this function?
  #### Is this function really necessary anymore?

  # look for a manifest
  # compare manifest to zip contents
  # look for metadata file - only one
  # look for data files - provide a count
  # look for a readme - optional
  # look for additional files
  rlang::inform("This function is still under construction")
}
