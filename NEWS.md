# NPSutils 0.3.0

  * updated all datastore api requests from v4/v5 to v6 (units service remains at v2)
  * add global variables for base datastore api urls and helper functions to access them in utils.R
  * added new functionality to `get_data_packages()`: it will now check to see if a DataStore reference ID is invalid or not. It will also check whether the reference is a data package or not. Substantial feedback is reported to the user if the flag force is set to FALSE.
  * added a new function `rm_local_packages()` which can delete one or more (or all) packages within a folder called "data" (where your packages should be if you downloaded them with `get_data_packages()`). This only deletes local copies and does not delete any data from DataStore.
  * changed function name `load_metadata()` is now `load_pkg_metadata()` so as not to conflict with `DPchecker::load_metadata()`
  * updated all function names to be snake_case
  * updated all file names to match function names
  * updated `get_data_packages()` to (when `force = FALSE`) check for newer versions of requested data packages and  prompt the user to download the newest version if they so choose.
  * updated `get_data_packages()` to:
  * include a force option. Force defaults to false for a verbose & interactive function. Setting force = TRUE removes the interactive portions and eliminates all informative messages except critical errors.
  * `get_data_packages()` now inspects the location the data packages are being written to. If a folder with the data package reference id already exists, the function will prompt the user asking if they want to re-download and overwrite the existing data package (when force = FALSE)
  * `get_data_packages()` now returns the full path to the data package folders, including the "/data" directory they are all in.
  * update `get_park_taxon_refs()` to hit v5/rest services
  * update documentation: make it clear that the taxon_code parameter in `get_park_taxon_refs()` is the IRMA taxon code, not the ITIS TSN.
  * update documentation: make it explicit when `get_park_taxon_refs()` and `get_park_taxon_citations()` are hitting secure servers and warn users that the results may also need to be restricted.

# NPSutils 0.2.0.3
  * added `map_wkt()` function to map points, polygons, or both from Well Known Text coordinates (WKT). WKT is used in place to GPS coordinates when sensitive species locations have been "fuzzed". In this case, providing a polygon rather than the an exact (albeit fuzzed) is preferable as it is clear that the location is not exact. WKT is an efficient way to store geographic shapes such as polygons in flat files such as .csv.

# NPSutils 0.2.0.1
  
  * updated get_data_package to retrieve 1 or more files from a given reference, if for instance a data package has multiple files associated with it. get_data_package is file extension agnostic.
  * get_data_package can now take a list of reference IDs from data store. It will write a separate folder for each reference (within a /data folder) and put the data files in the relevant folder.
  * get_data_package can now specify the directory that the /data folder and all child data package folders and data files are saved to. Defaults to the working directory.
  * get_data_package now returns the directory that the /data directory (where all sub-directories and data files are) to enable piping with downstream functions.
  * get_data_package informs the user of the file names being downloaded and where they are being saved; informs the user if a .zip is extracted and informs the user when the original .zip file is deleted.
  * get_data_package informs the user if a download failed due to not specifying secure=TRUE.

# NPSutils 0.2.0.0

Facelift to the entire package:

* Functions and parameters have snake_case names
* Tidyverse styling via styler
* Added bare-bones pkgdown site

# NPSutils 0.1.0.0

* Added a `NEWS.md` file to track changes to the package.
