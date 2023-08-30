## assign global package variables

#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("ds_api", "https://irmadevservices.nps.gov/datastore/v6/rest/", envir=.pkgglobalenv)

#data_store secure API base URL:
assign("ds_secure_api", "https://irmadevservices.nps.gov/datastore-secure/v6/rest/", envir=.pkgglobalenv)

.ds_api <- function(x){
  get("ds_api", envir = .pkgglobalenv)
}

.ds_secure_api <- function(x){
  get("ds_secure_api", envir = .pkgglobalenv)
}