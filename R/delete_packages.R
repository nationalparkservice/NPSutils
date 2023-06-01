#' Delete data packages from your local machine
#'
#' @description `delete_packages()` will delete one or more specified data packages. The function assumes that all data packages are in a folder called "data" and will delete items within the folder "data" but will not remove the "data" folder itself. 
#'
#' @param reference_id List. One or more data packages indicated by their 7-digit reference number.
#' @param all Logical. Defaults to `FALSE`. When set to `TRUE` all files and directories within the "data" folder will be deleted. It does not matter whether they are data packages or other files: the "data" folder will be empty. When `all = TRUE`, you do not need to supply the data package reference IDs in the reference_id parameter. When set to TRUE, 
#' @param path String. Defaults to the current working directory. The assumption is that the "data" folder containing data packages is located in the working directory (in other words, it assumes you used the default path when downloading data packages using `get_data_packages()`). If you downloaded data packages to a custom directory, use `path` to specify the directory where the "data" folder containing the data packages is located. Do not include the "data" folder itself in the path you supply. 
#' @param force Logical. Defaults to `FALSE`. If set to `TRUE` (for instance for scripting purposes) the output to the console is suppressed.
#'
#' @return invisible
#' @export
#'
#' @examples
#' \dontrun{
#' #delete a single data package:
#' delete_packages(1234567)
#' 
#' #delete multiple data packages:
#' pkgs<-c(1234567, 1234568, 1234569)
#' delete_packages(pkgs, force=TRUE)
#' 
#' #delete all data packages:
#' delete_packages(all = TRUE)
#' 
#' #delete data packages from a "data" directory in a custom location:
#' delete_packages(1234567, path = "C:/Users/username/Documents")
#' }
delete_packages <- function (reference_id, all = FALSE, path = here::here(), force = FALSE){
  if(all == FALSE){
    for(i in seq_along(reference_id)){
      d_path <- paste0(path, "/data/", reference_id[i])
      if(file.exists(d_path)){
        unlink(d_path, recursive = TRUE)
      }
      if(force == FALSE){
        cat("Data package ",
            crayon::blue$bold(reference_id[i]),
            " has been deleted from ", 
            crayon::blue$bold(paste0(path, "/data")), 
            ".\n",sep = "")
      }
    }
  } else {
    d_path <- paste0(path, "/data/*")
    unlink(d_path, recursive = TRUE)
    if(force == FALSE){
      cat("All data packges in ", 
          crayon::blue(paste0(path, "/data")), 
          " have been deleted.\n", sep = "")
    }
  }
}
