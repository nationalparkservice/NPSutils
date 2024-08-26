## assign global package variables

#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("ds_api", "https://irmaservices.nps.gov/datastore/v7/rest/", envir=.pkgglobalenv)

#data_store secure API base URL:
assign("ds_secure_api", "https://irmaservices.nps.gov/datastore-secure/v7/rest/", envir=.pkgglobalenv)

#data_store dev api (requires secure)
assign("ds_dev_api", "https://irmadevservices.nps.gov/datastore-secure/v7/rest/", envir = .pkgglobalenv)

.ds_api <- function(x){
  get("ds_api", envir = .pkgglobalenv)
}

.ds_secure_api <- function(x){
  get("ds_secure_api", envir = .pkgglobalenv)
}

.ds_dev_api <- function(x){
  get("ds_dev_api", envir = .pkgglobalenv)
}

#' Get Binary User Input
#'
#' Prompts for, gets, and returns binary user input (1 or 2)
#'
#' @return Factor. 1 or 2.
#'
#' @examples
#' \dontrun{
#' var1 <- .get_user_input()
#' }
.get_user_input <- function () {
  var1 <- readline(prompt = "1: Yes\n2: No\n")
  return(var1)
}

#this gets rid of the "no visible bindings for global variable 'x'"
#error during build checks:
globalVariables(c("capture.output",
                  "UnitDesignationName",
                  "FullName",
                  "UnitCode",
                  "UnitLifecycle",
                  "Network",
                  "NetworkName",
                  "Region",
                  "RegionName",
                  "StateCodes",
                  "DataPackageDirectory",
                  "holding_id",
                  "Name",
                  "load_pkg_metadata",
                  "workingEMLfile",
                  "metalocation",
                  "map_wkt",
                  "attributeFactors",
                  "metaformat",
                  "data_format",
                  "metadata_format",
                  "fileList"
                  ))