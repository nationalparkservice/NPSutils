#' Read contents of data package(s) and return a tibble with a tibble for each data file. 
#' 
#' `r lifecycle::badge("experimental")`
#'
#' @description `load_data_packages()` loads one to may data packages and returns a tibble of tibbles where each data package is a tibble and within that each data file is it's own tibble. `load_data_packages()` will only work with .csv data files and EML metadata. `load_data_packages()` can also utilize the metadata to assign attributes to each data column.
#' 
#' @details currently `load_data_packages()` only supports EML metadata and .csv files. To take advantage of the default settings in load_data_packages, use the default settings in `get_data_package()` or `get_data_packages()`. Archived (.zip) files must be extracted before `load_data_packages()` will work properly. Again, `get_data_package()` or `get_data_packages()` will accomplish this for you. 
#' '
#' @param reference_id is a list of 6-7 digit numbers corresponding to the DataStore reference ID of the datapackage(s) to load. Alternatively, you can set `reference_id` to "load_all", which will load all the data packages in your /data folder.
#' @param directory is the location of a folder, 'data' (created during `get_data_packages()`) which contains sub-directories where each sub-directory is the DataStore referenceId of the data package. Again, this file structure is all set up using `get_data_packages()`. Defaults to the current working directory (which is the default location for `get_data_packages()`).
#' @param assign_attributes Logical. Defaults to FALSE. Data will be loaded using `readr::read_csv()` guessing algorithm for calling column types. If set to TRUE, column types will be set using metadata attributes via the yet-to-be written `load_metadata()` function. `r lifecycle::badge('experimental')`
#' @param simplify Logical. Defaults to TRUE. If there is only a single data package loaded, the function will return a simple list of tibbles (where each tibble reflects a data file from within the data package). If set to FALSE, the function will return a list that contains a list of tibbles. This structure mirrors the object structure returned if multiple data packages are simultaneously loaded (a list of data packages with each data package containing a list of tibbles where each tibble corresponds to a data file in the given data package).
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
  if (reference_id == "all_data") {
    reference_id <- list.dirs(path = directory,
                                     full.names = FALSE,
                                     recursive = FALSE)
  }
  
  ### if only one data package is specified:
  ### fix how single data packages are handled later:
  if (assign_attributes == TRUE) {
    tibble_list <- list()
    for (h in 1:length(seq_along(reference_id))) {

      directory <- paste0(directory, "/", reference_id[h])
      #get csv file names:
      filenames <- list.files(path = directory,
                              pattern = "*csv")
      ## Create list of data frame names without the ".csv" part
      names <- gsub(pattern = "\\.csv$", "", filenames)
    
      #load metadata:
      metadata <- DPchecker::load_metadata(directory = directory)
    
      ### Load all files into tibbles
      tibble <- list()
      for (i in 1:length(seq_along(filenames))) {
        file_path <- file.path(paste0(directory,"/", filenames[i]))
      
        #get attributes information from metadata:
        # To do: specifically call dataTable by name, not position! #########
        dataTable <- metadata[["dataset"]][["dataTable"]][[i]]
        attribs <- purrr::map_dfr(dataTable[["attributeList"]][["attribute"]],
                                  tibble::as_tibble)
      
        attribs <- attribs %>% dplyr::mutate(R_data_type = dplyr::case_when(
          storageType == "string" ~ "collector_character",
          storageType == "date" ~ "collector_date",
          storageType == "float" ~ "collector_double"))
      
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
                  #handle case where there is only one code definition
                  if ("code" %in% names(nom4)) {
                    nom4 <- list(nom4)
                  }
                  for (k in 1:length(seq_along(nom4))) {
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
        suppressWarnings(tibble_list[[i]] <- 
                           assign(names[i],
                                  readr::read_csv(file_path,
                                                  col_types = csv_cols,
                                                  show_col_types = FALSE)
                                  )
        )
        names(tibble_list)[i] <- names[i]
      }
    }
  }
  return(tibble_list)
}      

      



get_attribute_type <- function(data_filename,
                               reference_id,
                               directory = here::here("data")
                               ){
  
  metadata <- DPchecker::load_metadata(directory = paste0(directory,
                                                         "/",
                                                         reference_id))
  #get dataTable(s):
  #if there is only one dataTable, put it in a list for consitency:
  if("physical" %in% names(metadata$dataset$dataTable)) {
    dataTable <- list(metadata$dataset$dataTable)
  } else {
    dataTable <- metadata$dataset$dataTable
  }
  # create a place to put attributes and information
  attribute_list <- list()
  #find the right dataTable:
  for (i in 1:length(seq_along(dataTable))) {
    if (dataTable[[i]][["physical"]][["objectName"]] == filename) {
      #get attribute names:
      attr_names <- unlist(dataTable[[i]])[grepl('attributeName',
                                                names(unlist(dataTable[[i]])),
                                                fixed=T)]
      names(attr_names) <- NULL
      
      #get attribute storage types
      attr_type <- unlist(dataTable[[i]])[grepl('storageType',
                                                    names(unlist(dataTable[[i]])),
                                                    fixed=T)]
      names(attr_type) <- NULL
      
      #turn these into a dataframe:
      filename_data <- tibble::as_tibble(data.frame(attr_names, attr_type))
      
      
      date_format <- unlist(dataTable[[i]])[grepl('formatString',
                                                names(unlist(dataTable[[i]])),
                                                fixed=T)]
      names(date_format) <- NULL
      
      
      filename_data2 <- filename_data %>% dplyr::mutate(date_format = dplyr::casewhen(attr_type == "date" ~ x))
      
      
      
      filename_data1 <- filename_data %>% dplyr::mutate(attr_type_abbr = dplyr::case_when(
        attr_type == "float" ~ "d",
        attr_type == "date" ~ "T",
        attr_type == "string" ~ "c"
      ))
      
      
      
      
      
      
      #add date formats to the dataframe:
      #get date formats:
      date_format <- unlist(dataTable[[i]])[grepl('formatString',
                                                  names(unlist(dataTable[[i]])),
                                                  fixed=T)]
      names(date_format) <- NULL
      
      transform(filename_data, format = ifelse( (attr_type == "date"), "Y", "unk"))
      
      
      
      
      
      attribute_list[i] <- assign(attributeNames,
                                  readr::read_csv(file_path,
                                                  show_col_types = FALSE))
    }
      
  }
}

