#' Validate a Tabular Data Package
#'
#' @description \code{validateDataPackage} checks a zipped data package for required files and structure. This function is still under construction.
#'
#' @param File is a path to a zipped data package
#'
#' @examples
#' \dontrun{
#' validate.dataPackage(File="C:/test/test_package.zip")
#' }
validate.dataPackage<-function(File){
  #look for a manifest
  #compare manifest to zip contents
  #look for metadata file - only one
  #look for data files - provide a count
  #look for a readme - optional
  #look for additional files
  rlang::inform("This function is still under construction")
}
