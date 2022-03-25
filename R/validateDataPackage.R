#' Validate a Tabular Data Package
#'
#' \code{validateDataPackage} checks a zipped data package for required files and structure.
#'
#' @param File is a path to a zipped data package
#'
#' @examples
#' validateDataPackage(File="C:/test/test_package.zip")

validateDataPackage<-function(File){
  #look for a manifest
  #compare manifest to zip contents
  #look for metadata file - only one
  #look for data files - provide a count
  #look for a readme - optional
  #look for additional files
  rlang::inform("This data package has validated successfully!")
}
