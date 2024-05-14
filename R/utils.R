## assign global package variables

#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("ds_api", "https://irmaservices.nps.gov/datastore/v6/rest/", envir=.pkgglobalenv)

#data_store secure API base URL:
assign("ds_secure_api", "https://irmaservices.nps.gov/datastore-secure/v6/rest/", envir=.pkgglobalenv)

.ds_api <- function(x){
  get("ds_api", envir = .pkgglobalenv)
}

.ds_secure_api <- function(x){
  get("ds_secure_api", envir = .pkgglobalenv)
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