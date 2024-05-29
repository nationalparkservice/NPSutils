
#' Dynamically access NPS park unit code data
#'
#' @description \code{get_unit_code} accesses info from irmaservices.nps.gov. Search for park or park unit with any string and return all applicable UnitCodes. Handy for use with get.dataPackage if you don't know a Park's UnitCode. allows the user to access information based on park codes
#'
#' @details  Contains multiple somewhat redundant functions for searching park units including unit codes, names, states, regions, networks, regions, etc.
#' These functions can be handy if you need to supply the unit code when downloading data but only know the park name, if you have a unit code but don't know what park, region, etc it refers to, or if you want to know all the parks that are within a given network, region, or state (note: it will actually supply all park units, not just National Parks).
#' Regions do NOT use the region numbering system because the underlying data this code draws on from irma does not include the region numbers.
#'
#' @param unit is a case-insensitive string containing some part of the unit's FullName.
#'
#' @importFrom magrittr %>%
#'
#' @return one data frame to the global environment. May contain multiple matches. Sufficient detail should be provided to choose the appropriate UnitCode for use with other NPSutils functions such as get.parkTaxonReferences (in get.referenceInfo.R).
#'
#' @export
#' @examples
#' \dontrun{
#' get_unit_code("ROMO")
#' }
get_unit_code <- function(unit) { # input must have quotes to indicate strings
  # To do:
  # Warnings and checks on parameter inputs are not yet implemented.
  # The output dataframe is a little unwieldy and could be cleaned up.


  # It is worth figuring out how stable the URL is: when were the data last updated? How frequently are they updated? Are field codes ever changed, or just data added? Do updates entail a new URL?
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    curl::curl_download("https://irmaservices.nps.gov/v2/rest/unit/", f) # access all park codes from NPS xml file
    # curl::curl_download(paste0("https://irmaservices.nps.gov/v2/rest/unit/",Unit, f)) #doesn't work
    # curl::curl_download(paste0("https://irmaservices.nps.gov/v2/rest/unit/", Unit,""), f) #works but requires removing preceding if(!file.exists(f))
    print("file downloaded") # check for download; remove when dev phase over
  }
  result <- XML::xmlParse(file = f) # parse xml
  dat <- XML::xmlToDataFrame(result) # xml to dataframe
  dat <- dat[, c(1, 3, 5, 7, 8, 9, 11, 13)] # pare down the output some
  alpha <- dat %>% dplyr::filter(grepl(unit, FullName, ignore.case = TRUE)) # filter FullName for input
  return(alpha) # return unit code
}


#' Restricts get_unit_code to just National Parks
#'
#' @description \code{get_park_code} is identical to \code{get_unit_code} except output is restricted to just National Parks (as opposed to including networks, etc.).Accesses info from irmaservices.nps.gov. Search for park or park unit with any string and return all applicable national park unitCodes. Handy for use with get.dataPackage if you don't know a Park's UnitCode.
#'
#' @param park is a case-insensitive string containing some part of the unit's FullName, e.g "Yellow".
#'
#' @return one data frame to the global environment. May contain multiple matches. Sufficient detail should be provided to choose the appropriate UnitCode for use with other NPSutils functions such as get.parkTaxonReferences (in ReferenceInfo.R). Dataframe contains UnitCode, FullName, UnitLifeCycle, Network, Region, and StateCodes.
#'
#' @export
#' @examples
#' \dontrun{
#' get_park_code("Yellow")
#' }
get_park_code <- function(park) { # case-insensitive string (in quotes) containing some part of the unit's FullName
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    curl::curl_download("https://irmaservices.nps.gov/v2/rest/unit/", f) # access all park codes from NPS xml file
  }
  result <- XML::xmlParse(file = f)
  dat <- XML::xmlToDataFrame(result) # xml to dataframe
  dat <- dat %>% dplyr::filter(grepl("National Park", UnitDesignationName)) # limit search to just National Parks
  dat <- dat[, c(1, 3, 8, 9, 11, 13)] # cleanup the output some
  alpha <- dat %>% dplyr::filter(grepl(park, FullName, ignore.case = TRUE)) # filter FullName for input
  return(alpha) # return park code
}

#' Gets information about a park Unit Code
#'
#' @description \code{get_unit_code_info} accesses info from irmaservices.nps.gov and allows you to search a Park Unit Code and determine which park, network, or other entity it is associated with along with ancillary information.
#'
#' @param code is a case-insensitive string. It typically is 4 letters long and typically does not include numbers but may be longer, shorter, or include special characters such as "-", e.g. "SFCN".
#'
#' @return one data frame to the global environment. May contain multiple matches. Sufficient detail should be provided to determine the intended  Park Unit's name. Data frame includes UnitCode, FullName, UnitLifeCycle, Network, Region, and StateCodes.
#'
#' @export
#' @examples
#' \dontrun{
#' get_unit_code_info("SFCN")
#' }
get_unit_code_info <- function(code) { # input must have quotes to indicate strings
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    curl::curl_download(paste0("https://irmaservices.nps.gov/v2/rest/unit/", f)) # access all park codes from NPS xml file
  }
  result <- XML::xmlParse(file = f)
  dat <- XML::xmlToDataFrame(result) # xml to dataframe
  dat <- dat[, c(1, 3, 8, 9, 11, 13)] # cleanup/reduce the output some
  alpha <- dat %>% dplyr::filter(grepl(code, UnitCode, ignore.case = TRUE)) # filter FullName for input
  return(alpha) # return park code
}


#' Search irmaservices.nps using any piece of information.
#'
#' @description \code{get_unit_info} accesses info from irmaservices.nps.gov and allows you to search a Park Unit based on any number or combination of parameters. Not all parameters need to be specified, but it is probably worth specifying which parameters ARE specified, e.g. get.unitInfo(LifeCycle="Inactive"). If the arguments are not specified, they will default to the order supplied in the function.
#'
#' @param code defaults to NULL. Is a case-insensitive string. It typically is 4 letters long and typically does not include numbers but may be longer, shorter, or include special characters such as "-".
#' @param park defaults to NULL. Is a case-insensitive string. It will search for any subset of the FullName or parks or park units
#' @param life_cycle is a case-insensitive string that will filter search results based on LifeCycle status. Currently acceptable values are:
#' * "Active", "Inactive" or "Pending"
#' @param network_code defaults to NULL. Is a case-insensitive string for the network code (or some subset of it). Note: not all park units have Network designations.
#' @param net_name defaults to NULL. Is a case-insensitive string containing of the network name (or some subset of it). Note not all park units are associated with networks.
#' @param region_abb defaults to NULL. Is a case-insensitive string containing the region abbreviation (or some subset of it), e.g.IMR for Intermountain Region. Not all park units are associated with a region.
#' @param region defaults to NULL. Is a case-insensitive string of the region name (or some subset of it). Not all park units are associated with a region.
#' @param state defaults to NULL. Is a case-insensitive search of the park unit's state. States are indicated using 2-letter abbreviations. Park units may span multiple states. Not all parks have state designations.

#' @return one data frame to the global environment. May contain multiple matches. Sufficient detail should be provided to determine the intended  Park Unit's name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_unit_info(state = "CO")
#' }
get_unit_info <- function(code = NULL,
                          park = NULL,
                          life_cycle = NULL,
                          network_code = NULL,
                          net_name = NULL,
                          region_abb = NULL,
                          region = NULL,
                          state = NULL) { # input must have quotes to indicate strings
  f <- file.path(tempdir(), "irmadownload.xml")
  if (!file.exists(f)) {
    # access all park codes from NPS xml file
    curl::curl_download("https://irmaservices.nps.gov/v2/rest/unit/", f) 
  }
  result <- XML::xmlParse(file = f)
  dat <- XML::xmlToDataFrame(result) # xml to dataframe
  # check UnitCode:
  if (!is.null(code)) {
    # if(grepl("[0-9]", Code)>0){
    #  stop("Code cannot contain numbers")
    # if(grepl("all", Code, ignore.case=TRUE)>1){
    #  dat<-dat
    #  }
    # else{
    dat <- dat %>% dplyr::filter(grepl(code, UnitCode, ignore.case = TRUE))
    #  }
    # }
  }

  # check UnitCycle:
  if (!is.null(life_cycle)) {
    # LifeCycle<-tolower(LifeCycle)
    # if( (LifeCycle == "active") + (LifeCycle == "inactive")\
    #  (LifeCycle=="pending") < 1)
    #  stop("LifeCycle must be \"Active\", \"Inactive\", or \"Pending\"")

    names <- paste0("\\<", life_cycle, "\\>")
    dat <- dat %>% dplyr::filter(grepl(names, UnitLifecycle,
                                      ignore.case = TRUE))
  }
  # check Park Name:
  if (!is.null(park)) {
    dat <- dat %>% dplyr::filter(grepl(park, FullName,
                                       ignore.case = TRUE))
  }
  # Network Code:
  if (!is.null(network_code)) {
    dat <- dat %>% dplyr::filter(grepl(network_code, Network,
                                       ignore.case = TRUE))
  }
  # Network Name:
  if (!is.null(net_name)) {
    dat <- dat %>% dplyr::filter(grepl(net_name, NetworkName,
                                       ignore.case = TRUE))
  }
  if (!is.null(region_abb)) {
    dat <- dat %>% dplyr::filter(grepl(region_abb, Region,
                                       ignore.case = TRUE))
  }
  if (!is.null(region)) {
    dat <- dat %>% dplyr::filter(grepl(region, RegionName,
                                       ignore.case = TRUE))
  }
  if (!is.null(state)) {
    dat <- dat %>% dplyr::filter(grepl(state, StateCodes,
                                       ignore.case = TRUE))
  }
  return(dat) # return park info
}
