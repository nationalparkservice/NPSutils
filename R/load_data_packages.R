#' Read contents of data package(s) and return a list of tibbles list of tibbles based on the data file(s). Can use metadata to specify data types.
#'
#' @description `load_data_packages()` loads one to many data packages and returns a list. If only one data package is loaded, the list will be a list of tibbles where each tibble is a data (.csv) file from the data package. If multiple data packages are loaded, the list will be a list of lists where each nested list contains a list of tibble and each tibble is a data file (.csv). See `simplify` below for details on handling these lists.
#' 
#' @details currently `load_data_packages()` only supports EML metadata and .csv files. The reference_id 
#' '
#' @param reference_id the immediate directory/directories where your data packages reside. For data packages downloaded from DataStore using `get_data_package()` or `get_data_packages()` default settings, this is the DataStore reference ID for your data package(s). Alternatively, you can set `reference_id` to "`load_all`", which will load all the data packages in the directory specified in via `directory` (typically ./data).
#' @param directory is the location of a folder that contains all of the data packages (where data packages are a folder containing .csv data files and a single .xml EML metadata file). If these data packages were downloaded from DataStore using the default settings for `get_data_packages`, this folder is "./data" and you can use the default settings for `directory`.
#' @param assign_attributes Logical. Defaults to FALSE. Data will be loaded using `readr::read_csv()` guessing algorithm for calling column types. If you set to `assign_attributes = TRUE`, column types will be set using the data types specified in the metadata. Currently supported data types include string, dateTime, float, double, integer, and categorical (factor in R). This assignment is very stringent: for instance if you did not specify date-time formats using ISO-8601 notation (i.e. "YYYY", not "yyyy"), your data will import as NAs. If you have undefined missing values or blank cells, your data will not import at all. If you run into problems consider using the default settings and letting `read_csv` guess the column types.
#' @param simplify Logical. Defaults to TRUE. If `simplify = TRUE`, the function will return a list of tibbles where each tibble is a data file from the data package(s) specified. The tibbles are named using the following format: "pkg_<reference_id.filename" (without the filename extension). If you want to load each individual data file into R for further processing, use `simplify = TRUE` and then run `list2env(x, envir=.GlobalEnv)`. If you set `simplify = FALSE`, the object returned will either be a list of tibbles identical to that returned by `simplify = TRUE` (if only one data package is loaded) or will be a list of lists where each nested list is a contains one tibble for each data file in each data package.Setting `simplify = FALSE` may make it easier to do post-processing on a package-by-package level rather than a tibble-by-tibble level.
#'
#' @return a list of (of lists of) tibbles. 
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' dat <- load_data_packages(2272461)
#' }
#'
load_data_packages <- function(reference_id, 
                             directory = here::here("data"),
                             assign_attributes = FALSE,
                             simplify = TRUE){

  
  #is user specifies "allData" get all directories from the data folder:
  if (length(seq_along(reference_id)) == 1 && reference_id == "load_all") {
    reference_id <- list.dirs(path = directory,
                                     full.names = FALSE,
                                     recursive = FALSE)
  }
  
  tibble_list <- list()
  for (h in 1:length(seq_along(reference_id))) {
    suppressWarnings(rm(directory1))
    directory1 <- paste0(directory, "/", reference_id[h])
    #get csv file names:
    filenames <- list.files(path = directory1,
                            pattern = "*csv")
    ## Create list of data frame names without the ".csv" part
    names <- gsub(pattern = "\\.csv$", "", filenames)
    
    ### Load all files into tibbles
    package_data <- list()
    for (i in 1:length(seq_along(filenames))) {
        file_path <- file.path(paste0(directory1,"/", filenames[i]))
      
        #get attributes information from metadata:
        #To do: handle case when only one data file in the data package!
        if (assign_attributes == TRUE) {
        #load metadata:
          metadata <- DPchecker::load_metadata(directory = directory1)
          # when there is only one dataTable:
          if ("physical" %in% names(metadata$dataset$dataTable)) {
            dataTable <- metadata[["dataset"]][["dataTable"]]
          } else {
            for (j in 1:length(seq_along(metadata$dataset$dataTable))) {
              if (filenames[i] %in% 
                  metadata$dataset$dataTable[[j]]$physical$objectName) {
                dataTable <- metadata[["dataset"]][["dataTable"]][[j]]
              }
            }
          }
          #turn the metadata into a useable tibble          
          attribs <- purrr::map_dfr(dataTable[["attributeList"]][["attribute"]],
                                    tibble::as_tibble)
          #map_dfr started double counting rows; fix it if it happens:
          attribs <- attribs %>% dplyr::distinct(attributeName,
                                                 .keep_all = TRUE)

          attribs <- attribs %>% dplyr::mutate(R_data_type = dplyr::case_when(
            storageType == "string" ~ "collector_character",
            storageType == "date" ~ "collector_date",
            storageType == "float" ~ "collector_double",
            storageType == "double" ~ "collector_double",
            storageType == "integer" ~ "collector_integer"))
      
          #get column specification as R would guess:
          csv_cols <- readr::spec_csv(file_path)
      
          #set data types based on EML, simple:
          for(j in 1:nrow(attribs)) {
            class(csv_cols$cols[[j]]) <- attribs$R_data_type[[j]]
          }
      
          #set date/time col type format string:
          for(j in 1:nrow(attribs)) {
            if("dateTime" %in% names(attribs$measurementScale[j])) {
              eml_date <- 
                attribs$measurementScale[j][["dateTime"]][["formatString"]]
              r_date <- QCkit::convert_datetime_format(eml_date)
              csv_cols$cols[[j]]$format <- r_date
            }
          }
          #set levels for factor call types:
          for (j in 1:nrow(attribs)) {
            if("nominal" %in% names(attribs$measurementScale[j])) {
              nom <- attribs$measurementScale[j][["nominal"]]
              if ("nonNumericDomain" %in% names(nom)) {
                nom2 <- nom[["nonNumericDomain"]]
                if ("enumeratedDomain" %in% names(nom2)) {
                  nom3 <- nom2[["enumeratedDomain"]]
                  if ("codeDefinition" %in% names(nom3)) {
                    nom4 <- nom3[["codeDefinition"]]
                    #get factors
                    factors <- NULL
                    #if (length(seq_along(nom4)) > 1) {
                      #nom4 <- unlist(nom4, recursive = FALSE)
                    #}
                    #handle case where there is only one code definition
                    if ("code" %in% names(nom4)) {
                      nom4 <- list(nom4)
                    }
                    # for(k in 1:length(seq_along(nom4))) {
                    #  if("code" %in% names(nom4[k])) {
                    #    factors <- append(factors, nom5[[k]])
                    #  }
                    #}
                    
                    for (k in 1:length(seq_along(nom4))) {
                      #print(paste0("i=",i, ", j=", j, " k=, ", k, "."))
                      factors <- append(factors, nom4[[k]][["code"]])
                    }
                    #set column type:
                    csv_cols$cols[[j]] <- readr::col_factor(factors,
                                                            include_na = FALSE,
                                                            ordered = FALSE)
                  }
                }
              }
            }
          }
          suppressWarnings(package_data[[i]] <-
                             assign(names[i],
                                    readr::read_csv(file_path,
                                                    col_types = csv_cols,
                                                    show_col_types = FALSE)
                                    )
                           )
          names(package_data)[i] <- names[i]
        } else {
        # Do not call attributes:
        suppressWarnings(package_data[[i]] <- 
                           assign(names[i],
                                  readr::read_csv(file_path,
                                                  show_col_types = FALSE)
                           )
        )
        names(package_data)[i] <- names[i]
      }
    }
    tibble_list[[h]] <- package_data
    names(tibble_list)[[h]] <- paste0("pkg_", reference_id[h])
  }
  #put all the tibbles in a single list that is not nested 
  #(simplifies subsequent extraction)
  if (simplify == TRUE) {
    tibble_list <- extract_tbl(tibble_list)
  }
  return(tibble_list)
}

#' @export
#' @rdname load_data_packages
load_data_package <- function(reference_id, 
                              directory = here::here("data"),
                              assign_attributes = FALSE,
                              simplify = TRUE) {
  
  x <- load_data_packages(reference_id, 
                          directory,
                          assign_attributes,
                          simplify)
  return(x)
}

#' extract nested tibbles
#' 
#' Adapted from stack overflow find_df function found at:
#' https://stackoverflow.com/questions/70512869/extract-data-frames-from-nested-list
#' And accessed on 2024-10-02
#'
#' @param x a (potentially deeply) nested list containing at least one tibble
#'
#' @return a list where each item in the list is a tibble found in the nested list `x`
#' @keywords Internal
#' @noRd
#'
#' @examples 
#' \dontrun{
#' z <- .extract_tbl(x)
#' }
extract_tbl <- function(x) {
  if (tibble::is_tibble(x))
    return(list(x))
  if (!is.list(x))
    return(NULL)
  unlist(lapply(x, extract_tbl), FALSE)
}
