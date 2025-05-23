% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getParkUnitInfo.R
\name{get_unit_code_info}
\alias{get_unit_code_info}
\title{Gets information about a park Unit Code}
\usage{
get_unit_code_info(code)
}
\arguments{
\item{code}{is a case-insensitive string. It typically is 4 letters long and typically does not include numbers but may be longer, shorter, or include special characters such as "-", e.g. "SFCN".}
}
\value{
one data frame to the global environment. May contain multiple matches. Sufficient detail should be provided to determine the intended  Park Unit's name. Data frame includes UnitCode, FullName, UnitLifeCycle, Network, Region, and StateCodes.
}
\description{
`get_unit_code_info` accesses info from irmaservices.nps.gov and allows you to search a Park Unit Code and determine which park, network, or other entity it is associated with along with ancillary information.
}
\examples{
\dontrun{
get_unit_code_info("SFCN")
}
}
