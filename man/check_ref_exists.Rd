% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datastore_interactions.R
\name{check_ref_exists}
\alias{check_ref_exists}
\title{Check whether a reference exists on DataStore}
\usage{
check_ref_exists(reference_id, secure = TRUE, dev = FALSE)
}
\arguments{
\item{reference_id}{Integer. A 7-digit number indicating the datastore reference to be queried.}

\item{secure}{Logical. Defaults to TRUE, indicating that the user is logged on to the NPS VPN (or is accessing the resource from an NPS office). Can be set to FALSE for non-NPS users. If `secure` is set to FALSE, `dev` is automatically set to FALSE.}

\item{dev}{Logical. Defaults to FALSE, indicating that the user wishes to work with the production version of DataStore. Can be set to TRUE if the user wants to work with the development environment on DataStore. Setting dev = TRUE will only work if the user has the appropriate permissions. Requires secure = TRUE.}
}
\value{
logical. TRUE means the reference exists, false means it does not exist.
}
\description{
given a DataStore reference ID, the function will hit the DataStore API to determine whether the reference exists. If it cannot contact DataStore, it will stop with an error that states the connection failed.
}
\examples{
\dontrun{
check_ref_exists(1234567)
check_ref_exists(1234567, secure = FALSE)
}
}
